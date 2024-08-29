import ee
import pandas as pd
import geopandas as gpd
import json
import sys

eeproject, filepath, outpath, first_year, last_year = sys.argv[1:]

try:
  ee.Initialize(project = eeproject)
except Exception as e:
  ee.Authenticate()
  ee.Initialize(project = eeproject)

fields = gpd.read_file(filepath)

# Defining the parameters for filtering for clouds
CLOUD_FILTER = 95
CLD_PRB_THRESH = 5
NIR_DRK_THRESH = 0.15
CLD_PRJ_DIST = 1
BUFFER = 50

# Here we define the functions that filter for clouds, add vegetation indices, crop to area of interest
## Function to add NDVI data to the landsat images
def addIndicesSentinel(img):
    ndvi = img.normalizedDifference(['B8', 'B4']).rename('ndvi')
    gcvi = img.expression('(nir / green) - 1', {'nir': img.select('B8'), 'green': img.select('B3')}).select([0], ['gcvi'])
    ndre = img.normalizedDifference(['B8', 'B5']).rename('ndre')
    evi = img.expression('2.5 * (nir - red) / (nir + (6 * red) - (7.5 * blue) + 1)',
                          {'nir': img.select('B8').divide(10000), 
                           'blue':img.select('B2').divide(10000),
                           'red': img.select('B4').divide(10000)}).select([0], ['evi'])
    reci = img.expression('(nir / rededge) - 1', {'nir': img.select('B8'), 'rededge': img.select('B5')}).select([0], ['reci'])
    bsi = img.expression('((swir + red) - (nir + blue)) / ((swir + red) + (nir + blue))', 
                         {'swir': img.select('B11').divide(10000), 
                          'red': img.select('B4').divide(10000),
                          'blue':img.select('B2').divide(10000),
                          'nir':img.select('B8').divide(10000)}).select([0], ['bsi'])


    return img\
        .addBands(ndvi)\
        .addBands(gcvi)\
        .addBands(ndre)\
        .addBands(evi) \
        .addBands(reci)\
        .addBands(bsi)


## Function to crop the image collection to an AOI
def wrapCropToField(ImageCollection, aoi):
    def cropToField(img):
        return img.clip(aoi)
    return ImageCollection.map(lambda img: cropToField(img))

## Function to retrieve the median
def wrapReduceMedian(list, aoi, band):
    def calculateMedian(img):
        img = ee.Image(img)
        reduced  =  img.select(band).reduceRegion(
            reducer = ee.Reducer.median(),
            geometry = aoi.geometry(),
            scale = 30
        )
        return reduced\
            .set('time', img.get('system:time_start'))
    return list.map(lambda img: calculateMedian(img))


def add_cloud_bands(img):
    # Get s2cloudless image, subset the probability band.
    cld_prb = ee.Image(img.get('s2cloudless')).select('probability')

    # Condition s2cloudless by the probability threshold value.
    is_cloud = cld_prb.gt(40).rename('clouds')

    # Add the cloud probability layer and cloud mask as image bands.
    return img.addBands(ee.Image([cld_prb, is_cloud]))


def add_shadow_bands(img):
    # Identify water pixels from the SCL band.
    not_water = img.select('SCL').neq(6)

    # Identify dark NIR pixels that are not water (potential cloud shadow pixels).
    SR_BAND_SCALE = 1e4
    dark_pixels = img.select('B8').lt(0.15*SR_BAND_SCALE).multiply(not_water).rename('dark_pixels')

    # Determine the direction to project cloud shadow from clouds (assumes UTM projection).
    shadow_azimuth = ee.Number(90).subtract(ee.Number(img.get('MEAN_SOLAR_AZIMUTH_ANGLE')))

    # Project shadows from clouds for the distance specified by the CLD_PRJ_DIST input.
    cld_proj = (img.select('clouds').directionalDistanceTransform(shadow_azimuth, 2*10)
        .reproject(**{'crs': img.select(0).projection(), 'scale': 100})
        .select('distance')
        .mask()
        .rename('cloud_transform'))

    # Identify the intersection of dark pixels with cloud shadow projection.
    shadows = cld_proj.multiply(dark_pixels).rename('shadows')

    # Add dark pixels, cloud projection, and identified shadows as image bands.
    return img.addBands(ee.Image([dark_pixels, cld_proj, shadows]))



def add_cld_shdw_mask(img):
    # Add cloud component bands.
    img_cloud = add_cloud_bands(img)

    # Add cloud shadow component bands.
    img_cloud_shadow = add_shadow_bands(img_cloud)

    # Combine cloud and shadow mask, set cloud and shadow as value 1, else 0.
    is_cld_shdw = img_cloud_shadow.select('clouds').add(img_cloud_shadow.select('shadows')).gt(0)

    # Remove small cloud-shadow patches and dilate remaining pixels by BUFFER input.
    # 20 m scale is for speed, and assumes clouds don't require 10 m precision.
    is_cld_shdw = (is_cld_shdw.focalMin(2).focalMax(200*2/20)
        .reproject(**{'crs': img.select([0]).projection(), 'scale': 20})
        .rename('cloudmask'))

    # Add the final cloud-shadow mask to the image.
    return img.addBands(is_cld_shdw)

def apply_snow_mask(img):
    ndsi = img.normalizedDifference(['B3', 'B11']).rename('ndsi')
    snow = ndsi.gt(0.42)
    snowMask = snow.eq(0)
    return img.updateMask(snowMask)


def apply_cld_shdw_mask(img):
    # Subset the cloudmask band and invert it so clouds/shadow are 0, else 1.
    not_cld_shdw = img.select('cloudmask').Not()

    # Subset reflectance bands and update their masks, return the result.
    return img.select('B.*').updateMask(not_cld_shdw).copyProperties(img, img.propertyNames())


def get_s2_sr_cld_col(aoi, start_date, end_date):
    # Import and filter S2 SR.
    s2_sr_col = (ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED')
        .filterBounds(aoi)
        .filterDate(start_date, end_date)
        .filter(ee.Filter.lte('CLOUDY_PIXEL_PERCENTAGE', CLOUD_FILTER)))

    # Import and filter s2cloudless.
    s2_cloudless_col = (ee.ImageCollection('COPERNICUS/S2_CLOUD_PROBABILITY')
        .filterBounds(aoi)
        .filterDate(start_date, end_date))

    # Join the filtered s2cloudless collection to the SR collection by the 'system:index' property.
    return ee.ImageCollection(ee.Join.saveFirst('s2cloudless').apply(**{
        'primary': s2_sr_col,
        'secondary': s2_cloudless_col,
        'condition': ee.Filter.equals(**{
            'leftField': 'system:index',
            'rightField': 'system:index'
        })
    }))


    
   
## Masking ladsat images
def apply_cloud_mask_landsat(image):
    qaMask = image.select('QA_PIXEL').bitwiseAnd(int('11111', 2)).eq(0)
    saturationMask = image.select('QA_RADSAT').eq(0)
    opticalBands = image.select('SR_B.').multiply(0.0000275).add(-0.2)
    thermalBands = image.select('ST_B.*').multiply(0.00341802).add(149.0)

    return image.addBands(opticalBands, None, True)\
                .addBands(thermalBands, None, True)\
                .updateMask(qaMask)\
                .updateMask(saturationMask)



## Adding the vegetation indices to landsat 8
def addIndicesLandsat(img):
    ndvi = img.normalizedDifference(['SR_B5', 'SR_B4']).rename('ndvi')
    gcvi = img.expression('(nir / green) - 1', {'nir': img.select('SR_B5'), 'green': img.select('SR_B3')}).select([0], ['gcvi'])
    evi = img.expression('2.5 * (nir - red) / (nir + (6 * red) - (7.5 * blue) + 1)',
                          {'nir': img.select('SR_B5'), 
                           'blue':img.select('SR_B2'),
                           'red': img.select('SR_B4')}).select([0], ['evi'])
    
    bsi = img.expression('((swir + red) - (nir + blue)) / ((swir + red) + (nir + blue))', 
                        {'swir': img.select('SR_B6'), 
                        'red': img.select('SR_B4'),
                        'blue':img.select('SR_B2'),
                        'nir':img.select('SR_B5')}).select([0], ['bsi'])


    return img\
        .addBands(ndvi)\
        .addBands(gcvi)\
        .addBands(evi) \
        .addBands(bsi)

## Adding the vegetation indices to landsat 7
def addIndicesLandsat7(img):
    ndvi = img.normalizedDifference(['SR_B4', 'SR_B3']).rename('ndvi')
    gcvi = img.expression('(nir / green) - 1', {'nir': img.select('SR_B4'), 'green': img.select('SR_B2')}).select([0], ['gcvi'])
    evi = img.expression('2.5 * (nir - red) / (nir + (6 * red) - (7.5 * blue) + 1)',
                          {'nir': img.select('SR_B4'), 
                           'blue':img.select('SR_B1'),
                           'red': img.select('SR_B3')}).select([0], ['evi'])
    
    bsi = img.expression('((swir + red) - (nir + blue)) / ((swir + red) + (nir + blue))', 
                        {'swir': img.select('SR_B5'), 
                        'red': img.select('SR_B3'),
                        'blue':img.select('SR_B1'),
                        'nir':img.select('SR_B4')}).select([0], ['bsi'])


    return img\
        .addBands(ndvi)\
        .addBands(gcvi)\
        .addBands(evi) \
        .addBands(bsi)

# Here we loop over the fields and apply these functions. We combine all the information into a dataframe.

# %%
comb_df = pd.DataFrame()

for index, row in  fields.iterrows():
## Define an EE polygon

    field = fields.iloc[[index]].to_json()
    field = ee.FeatureCollection(json.loads(field)) 

## initial and final dates for filtering
    i_date = first_year + '-01-01'
    f_date = last_year + '-12-31'

    try:
    ### Retrieve the sentinel collection 
        s2_sr_cld_col_eval = get_s2_sr_cld_col(field.geometry(), i_date, f_date)
        s2_sr_cld_col_eval = s2_sr_cld_col_eval.map(add_cld_shdw_mask).map(apply_cld_shdw_mask).map(apply_snow_mask)

    ### Crop to the AOI
        sentinelCroped = wrapCropToField(s2_sr_cld_col_eval, field)
    ### Add NDVI
        sentinelCroped = sentinelCroped.map(addIndicesSentinel)
    ### Convert to list so we can extract the data more easily
        sentinelList = sentinelCroped.toList(sentinelCroped.size())
    ### Reduce to the median of the field
        sentinelCroped = wrapReduceMedian(sentinelList, field, ['ndvi', 'gcvi', 'bsi', 'evi'])
    ### Retrieve the information from GEE
        sentinel_info = sentinelCroped.getInfo()
    ### Convert to a data frame
        sentinel_df = pd.DataFrame(sentinel_info)
    ### Convert timestart to datetime
        sentinel_df['datetime'] = pd.to_datetime(sentinel_df['time'], unit='ms')

    ## Add fieldname to data frame
        sentinel_df['fieldName'] = filepath
        sentinel_df['year'] = pd.DatetimeIndex(sentinel_df['datetime']).year
        sentinel_df['platform'] = 'sentinel'
        comb_df = pd.concat([comb_df, sentinel_df], ignore_index = True)

    except Exception as e:
       print(e)

    try:
        landsatCollection =  ee.ImageCollection("LANDSAT/LC08/C02/T1_L2")\
                                        .filterDate(i_date, f_date)\
                                        .filter(ee.Filter.lte('CLOUD_COVER', CLOUD_FILTER))\
                                        .filterBounds(field)\
                                        .map(apply_cloud_mask_landsat)
        
        # landsatCollection = s2_sr_cld_col_eval = get_s2_sr_cld_col(field.geometry(), i_date, f_date).map(apply_cloud_mask_landsat)



        ### Crop to the AOI
        landsatCropped = wrapCropToField(landsatCollection, field)
        ### Add NDVI
        landsatCropped = landsatCropped.map(addIndicesLandsat)
        ### Convert to list so we can extract the data more easily
        landsatList = landsatCropped.toList(landsatCropped.size())
        ### Reduce to the median of the field
        landsatCropped = wrapReduceMedian(landsatList, field, ['ndvi', 'gcvi',  'bsi', 'evi'])
        ### Retrieve the information from GEE
        landsat_info = landsatCropped.getInfo()
        ### Convert to a data frame
        landsat_df = pd.DataFrame(landsat_info)
        ### Convert timestart to datetime
        landsat_df['datetime'] = pd.to_datetime(landsat_df['time'], unit='ms')
        landsat_df['fieldName'] = filepath
        landsat_df['year'] = pd.DatetimeIndex(landsat_df['datetime']).year
        landsat_df['platform'] = 'landsat8'
        comb_df = pd.concat([comb_df, landsat_df], ignore_index = True)
    except Exception as e:
        print(e)



    try:
        landsatCollection7 =  ee.ImageCollection("LANDSAT/LE07/C02/T1_L2")\
                                        .filterDate(i_date, f_date)\
                                        .filter(ee.Filter.lte('CLOUD_COVER', CLOUD_FILTER))\
                                        .filterBounds(field)\
                                        .map(apply_cloud_mask_landsat)
        
        # landsatCollection = s2_sr_cld_col_eval = get_s2_sr_cld_col(field.geometry(), i_date, f_date).map(apply_cloud_mask_landsat)



        ### Crop to the AOI
        landsatCropped7 = wrapCropToField(landsatCollection7, field)
        ### Add NDVI
        landsatCropped7 = landsatCropped7.map(addIndicesLandsat7)
        ### Convert to list so we can extract the data more easily
        landsatList7 = landsatCropped7.toList(landsatCropped7.size())
        ### Reduce to the median of the field
        landsatCropped7 = wrapReduceMedian(landsatList7, field, ['ndvi', 'gcvi',  'bsi', 'evi'])
        ### Retrieve the information from GEE
        landsat_info7 = landsatCropped7.getInfo()
        ### Convert to a data frame
        landsat_df7 = pd.DataFrame(landsat_info7)
        ### Convert timestart to datetime
        landsat_df7['datetime'] = pd.to_datetime(landsat_df7['time'], unit='ms')
        landsat_df7['fieldName'] = filepath 
        landsat_df7['year'] = pd.DatetimeIndex(landsat_df7['datetime']).year
        landsat_df7['platform'] = 'landsat7'
        comb_df = pd.concat([comb_df, landsat_df7], ignore_index = True)
    except Exception as e:
        print(e)



# Here we export the data frame to a csv file
comb_df = comb_df[['fieldName', 'platform', 'year', 'datetime', 'ndvi','bsi', 'evi', 'gcvi']]
comb_df.to_csv(path_or_buf= outpath, index = False)



