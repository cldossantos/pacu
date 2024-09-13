
## 2024-09-13

- Version 0.1.35:
 
- Caio made improvements to the function that guesses units. Now, the function uses the georeferenced information contained in the sf object to guess the units of distance between points. I believe this will fix the problem with guessing units when the interval between measurements is very smalle (e.g, 0.2s).

- I plan on make improvements to the guessing of other variables as well.


## 2024-08-31

- We have finally cleared out all the latex errors from the documentation. Most of them were due to the vegetation index formulas in the `pa_compute_vi()` function documentation. 


## 2024-08-20

- Caio tested the pacu workflow using the data from one of the fields that Peter Kyveryga shared with us (JF2001). The original data set did not have the swath or interval between measurements but those could be extracted from the manuscript shared with us. The nitrogen response we got is really clean. The results of this test are in "./test-deprecated/testing-kyveryga-dataset.R". 

- Caio started developing the pa_trial() function. It is preliminary but we can see in the "./test-deprecated/testing-mizzou-difm.R" that processing the as applied treatment data has benefits in terms of reducing the noise of the data.

- Something important about the pa_trial() function is that, contrary to the pa_yield() function, when there is overlap between polygons, the polygons values should be added together. This will ensure, for example, that if the planter passes more than once on the same spot, those extra seed will be accounted for.

## 2024-08-16

- Caio included the concaveman package in the "imports" section of the description


## 2024-08-13

- Caio moved the notes from the README to the "package-notes" file

- Caio checked the vignettes for typos and consistency. 

## 2024-08-01

- Caio updated the vignettes.

## 2024-08-09

- Version 0.1.33: 

1) Cleaned the warnings from the devtools::check() function

2) Updated the examples and descriptions of all functions

## 2024-07-31

- Version 0.1.32:

1) pa_get_vi_stats() and pa_compute_vi() now return objects of class "veg.index". The pa_plot() function now has a method to plot objects of class "veg.index". It tries to identify whether it is a raster or a vector object and plot according to those. 

2) there's a summary method for the "veg.index" function now that allows users to go from the raster data to vector data. Ideally, this would allow users to superimpose a trial design to an object produced by pa_compute_vi() and get data on a plot level.

3) the objects that came out of the pa_get_vi_stats() used to have class "sftime". To be more consistent with the rest of the package and the methods used, I changed it to "stars". This eliminates a dependency and provides consistency with objects coming out of other functions as well. 



## 2024-07-25

- Next steps:

1) s3 class and methods for plotting objects from pa_request_vi()

2) s3 method for extracting vegetation index values by simple features. This is meant to make it easier for people to use the package.

3) update the vignettes 


 - Version 0.1.31:

1) pa_compute_vi() now returns a stars object with a time component. This is meant to allow for spatial-temporal analysis. Previously, it would return a list of stars objects without any time component associated. The date of the satellite image had to be extracted from the filename.

2) pa_browse_dataspace() now limits the product name to MSIL2A. This is because the functions that process the data are all meant for this sentinel product. In the future we can add support to more products but, for now, this is meant to prevent unintended behavior of the function.

3) .pa_crop_s2_to_aoi() now also keeps the metadata file. For now, we use this to timestamp the images but there is potential to use it for more.

## 2024-07-16

- Caio's notes:

Version 0.1.30:

1)  Added the concaveman package to the suggested packages and made its method the default for the .pa_field_boundary function. It works really well and really fast.

2)  The pa_check_yield functin outputs now the number of points, the coordinates of the field centroid, field area in hectares and acres, number of NAs, number of values outside of the 3SD range. This function is looking really good now, I believe. Maybe I need to do a little more work on the warning messages.



## 2024-07-15

- Caio's notes:

Version 0.1.29:

1) The pa_yield function now will return points when the algorithm is simple and the prediction grid is null. 

2) Removed the step that generated boundaries automatically. It was slowing down the process without bringing much benefit. Now, this step only happens automatically when we want to clean field edges and the argument boundary is NULL.

3) Fixed a bug that was preventing arguments such as "maxdist" from being passed to the krige function when predicting in parallel. This has sped up the kriging process a lot.

4) I will now work on improving the pa_check_yield

## 2024-07-13

-   Caio's notes:

Version: 0.1.28

1)  The pa_yield function now will also look for "mass" and "time"
    columns. When it cannot find the "flow" column, it can go directly
    to the "mass", as the flow is simply a means to get to "mass". When
    the function cannot find the "interval" column, it will look for the
    "time" column and estimate the interval from the time stamp. This is
    tricky though, as for the MO data, this generates some negative
    intervals.

2)  Future changed to the "pa_check_yield" function are to work on the
    improvements that Fernando has suggested on 07/10.

3)  Future changes to the "pa_yield" function are to remove the
    automatic grid from the simple algorithm and update the
    documentation to explain the different behavior when using the two
    algorithms.

## 2024-07-12

-   FEM: I suggest that for future entries we keep track of the package
    version. This is why I bump it up every once in a while. We are at
    0.1.27

## 2024-07-11

-   Caio's notes:

1)  I wrote a test './tests/testing-memory-allocation.R' to track the
    time and RAM memory of the pa_yield function as the number of cores
    increase. The results below show that memory consumption increases
    as the number of cores increases. However, we should be able to run
    the code with 13 cores, if memory was the only problem. For every
    additional code, we add about 400 Mb of RAM consumption. For some
    reason, when we increase processing to 13 cores, the computer
    crashes.

```         
------------------------
core   time min.mem mem.used
---    ---    ---    ---
 1 12.195 19.478 12.522
 2  6.781 18.612 13.388
 3  4.981 18.224 13.776
 4   4.26 17.715 14.285
 5   3.83 17.413 14.587
 6  3.595  16.93  15.07
 7  3.476 16.536 15.464
 8  3.482 16.182 15.818
 9  3.467 15.758 16.242
10  3.483 15.411 16.589
11  3.467     15     17
12  3.503 14.669 17.331
------------------------
```

## 2024-07-10

-   Caio's notes:

1)  We had written a function to solve polygon boundaries that worked
    faster than the previous one. However, that function can fail
    sometimes and I could not figure out the reason why. I have reverted
    back to the previous function. Having the right answer is better
    than having a fast one.

2)  I have been testing the memory allocation and it seems that the
    problem with memory exhaustion in Windows is not exactly with
    Kriging. I am still investigating.

3)  I have been thinking about the idea of trying to be consistent with
    what a gam/lm model would do. If we do not supply "newdata" or a
    "prediction grid", it would simply return predictions for the same
    points of the sample. I absolutely love this idea for the simple
    algorithm. However, for ritas, the last three steps will deal with
    polygons and require some level of smoothing. Is it better to return
    the points only when using the simple?

4)  I will work on Fernando's suggestions this week.

-   FEM: Testing the 'pa_check_yield' function. Some suggestions for
    improvement:
    1)  Print at the beginning the size of the field. Number of points,
        area (acre/hectare) Potentially, include the location (as an
        example, this is done in the apsimx::get_isric_soil_profile
        function). The size of the field and the number of points might
        be useful for processing requirements later. So object size
        would be useful.
    2)  Check that latitude and longitude are in correct ranges, etc.
    3)  Given that we are testing names after converting to lower case,
        an object could have columns named: yield, Yield, YIELD, etc.
        and this could be a problem. I was able to break the code by
        adding a column 'yield' as in "x$yield <- x$DRY_BU_AC"
    4)  Add minimum and maximum to the tables
    5)  Add a count of observations +/- 2 and 3 SDs which might suggest
        cleaning is needed

## 2024-07-06

-   We are making a lot of changes but there's one that I want to make
    sure to document. I have enabled kriging paralellization once again
    for testing purposes

## 2024-07-06

-   Fernando's thoughts on testing the 'pa_yield' function:

-   For some datasets cleaning based on standard deviation might not
    work well. I used a simple threshold of removing yields of less than
    a given arbitrary value and this worked better than using sd. For
    example, you can use less than 200 bu/ac for soybean and less than
    500 bu/ac for corn. This removes really high values in the 1000s.
    Perhaps the way to deal with this is to add a couple of arguments
    such as 'lower.limit' and 'upper.limit' which the user could
    perform. I think I prefer to separate the tasks in two step and
    remove them from options in the 'pa_yield' step.

-   The algorithm 'simple' performs a somewhat arbitrary grid
    interpolation. It would be better to have a default algorithm called
    'none' which does not perform any interpolation at all. A user might
    still be interested in correcting moisture or converting units
    without the interpolation. An optional 'grid' argument would allow
    the user to create a simple grid with some simple argument (like an
    integer) or pass a grid using 'st_make_grid' for example.

-   GAMS are very fast and computationally efficient. They are worth
    incorporating as a smooth method. The 'formula' argument could also
    add the effect of covariates.

-   I tested having 'data.columns' with variable 'moisture' (Even though
    it was not in the data.frame) and this created a problem (error
    message) with units.

-   I propose that we have a recommended workflow of 1) checking yield
    (pa_check_yield),

    2)  cleaning yield (pa_clean_yield), 3) processing it (pa_yield), 4)
        plotting it or summarizing it (pa_plot, print, summary).
        Function .pa_clean_yield_monitor needs to be exported. Function
        pa_check_yield needs to be created. It should return an object
        of class 'yield'. While checking it should not make changes but
        instead suggest units by default. Perhaps an optional argument
        will attempt to automatically set units. This also means that we
        should remove the 'clean' arguments in the 'pa_yield' function.

-   Small change to 'pa_yield' for better display of progress bar. A
    note on this: I removed the new line '\n' from a lot of 'cat'
    messages. This is rarely needed. If the messages are not displayed
    in a compact way. Something else needs to change. I moved the
    progress bar after the 'get_variable' operations. The rationale is
    that the first step is quick. The problem with initializing the
    progress bar before this step is that it is not displayed nicely.

## 2024-07-01

-   Caio updated the documentation of the "pa_plot" function.

-   Caio transitioned all raster operations from terra to stars. This
    transition has allowed us to register S3 methods for all objects
    returned by the package.

-   Caio has implemented the plotting of the package objects using the
    "tmap" plotting engine. This allows for really nice plots and allows
    for a very easy transition between interactive and static maps.

## 2024-06-28

### Caio's comments

-   I have been exploring the tmap plotting engine to use as a default
    for the package. It allows for "production-ready" maps and has the
    options to make interactive plots as well.
-   The terra package works with S4 classes. Unfortunately, I will have
    to either move the raster operations from terra to stars, or figure
    out a way of working with S4 and S3 classes at the same time. The sf
    author made some good points here:
    <https://github.com/r-spatial/stars/issues/633>

## 2024-06-27

### Caio's comments

-   Upon reading more about the S3 classes, I believe this is the way to
    go to specify generic functions in the pacu package. Part of this is
    seeing how several packages that have implemented S4 classes, such
    as sp, have move away from it in more recent implementations. I
    believe the S3 class should work well for our purposes.

-   Another idea I've had would be to move the weather plotting
    functions to the apsimx package and adpat the functions in the
    apsimx package that retrieve weather to accept and area of interest.
    This way, we can remove the weather part from pacu. This would also
    allow us to create generic functions with "plot", instead of having
    to make a distinctive function "pa_plot".

## 2024-06-25

-   Uppon testing, Caio narrowed down the problem with the
    Hord_F104_2022_Soybean field to the guessing of units. The algorithm
    is guessing that the units of distance are in meters. Since they are
    actually in ft, this causes an overestimation of the overlap between
    polygons. This results in NAs because there is so much overlap
    between the polygons.

-   The functions that guesses units is extremely simple. There is room
    for improvement there. In the Hord_f104 example, the logging
    interval of the yield monitor was 0.2 seconds. Therefore, the
    displacement between one point and the next was small, leading the
    function to guess the units were meters. One idea is to use the
    geo-referenced information to measure the distance and compare it to
    the distance in the data set. This might be a more robust approach.

-   Caio added column/unit guessing to the simple algorithm as well

-   Caio will work on the "pa_yield_check" function and on how to use
    the S3 classes to implement generic functions within the package.

-   Caio will also look into how to restrict memory use when
    parallelizing the code.

-   About the unit.system defaulting to the metric system, I thought
    that made sense at first. We could make it so people would have to
    choose but I believe that would be an additional inconvenience. I
    could write it in the documentation that the algorithm defaults to
    the metric system if the unit system is no specified.

## 20:24-06-22

Fernando changes/ideas:

\- Propose the use of S3 classes. For example, the 'pa_yield' function
should return an object of class 'yield' or 'pa_yield'. Then we can
write generics for this specific class.

\- Propose the definition of a generic called 'pa_plot'. We can then
write specific plotting functions for the package. The reason for
avoiding 'plot' is that the apsimx package already has a 'plot.met', but
here we want different plots for 'met' objects. An alternative would be
to not include any 'weather' related code here and implement it all in
'apsimx. An idea for the future, I think.

\- Many small changes some minor (code formatting). Other changes
related to the order of operations in 'pa_yield'

\- Currently testing cores argument under Windows. We should probably
migrate to the future package. Parallel execution under RStudio is
unstable (the future package issues a warning if trying to run parallel
processes from RStudio). Using 10 cores on a 20 (logical, but still 14
physical) core laptop resulted in corrupted memory. Currently using 4
cores and it seems to be running. Perhaps, initially we need to restrict
the number of cores to less than half of the physical cores.

\- After testing this, I suspect that the problem is largely about
memory and not about cores. I will look into restricting memory instead
of cpu. This is related to the size of the object we are processing and
not to the operations.

\- When testing the function with field Hord_F104_2022_Soybean_Yield the
result is always NA. There is something wrong somewhere and I'll try to
figure it out. The problematic step is the apporting. It returns all
'NAs'. Is the problem line 419 in 'pacu_internal.R'? The function
'.pa_areal_weighted_average' sets 'var' to NA for some reason.

\- When running algorithm 'ritas' variable names are being guessed, but
when running 'simple' it does not happen. I don't know why this makes
sense. Probably we can add 'guessing' to 'simple' too.

\- We should create a function called 'pa_yield_check' to catch issues
before running 'pa_yield'. The file 'Hord_F104_2022_Soybean_Yield' has
yields which are completely unrealistic. A user might run 'pa_yield'
without cleaning the data first. However, we should issue a warning.
Function 'pa_yield_check' could also check for any common issues that we
can identify. For example, catch GPS coordinate jumps, missing data,
etc.

\- I ran the 'simple' algorithm with inputs in the 'standard' system and
I did not choose the 'unit.system'. I think most users will be surprised
to see the results in 'Mg/ha'. Maybe we should pick the 'output' units
based on the input units. So I would have this argument as 'NULL' and
either 'guess' it based on 'input' units with a message or stop with an
error and force the user to pick one. I'm leaning toward this last
option.

\- The smooth argument should also be NULL by default. The reason is
that 'ritas' does perform smoothing. In this case it is fine if the
smooth method is not needed.

\- Testing basic function arguments. Added a check to make sure that
'data.columns' are present in input 'names'. Additional check for
data.units is needed

\- Added some 'na.rm' to calculations. For example in moisture
calculation step.

-   There is inconsistent adjustment of moisture. In file
    'testing-hord22.r' for example 'ym1' there is an adjustment of
    moisture to 0% first and then to field average. - In testing-hord22
    ym4 does not work. It returns an error when fitting the variogram.
    The error message is related to 'addAttrToGeom'. It is possible that
    this is because apport returns all NAs.

## 2024-06-17

**Changes:**

-   Caio made improvements to the ".pa_chop_polygons" function to
    improve speed

**Notes:**

-   Using the profiling library called "profvis", Caio was able to
    identify bottlenecks in the ".pa_chop_polygons". To chop 500
    polygons, the function used to take on average 8.7s. Now, it takes
    3.9s. Hopefully this will make the ritas algorithm faster.

## 2024-06-07

**Changes:**

-   Caio added a progress bar to the pa_yield function.

-   Caio added the "on.exit(stopClust(cl))" line to all function that
    include some sort of parallelization. This ensures that the cluster
    will be terminated even if the function is escaped or returns an
    error.

-   Caio added another environment variable called
    "run.experimental.tests". This will be used to toggle tests that are
    still under development. The variable "run.local.tests" will be used
    to toggle tests that are supposed to be working in an automated way
    using the "testthat" library.

## 2024-06-05

**Changes:**

-   Caio added the argument "remove.crossed.polygons" to the pa_yield
    function. This is intended to remove harvest polygons that crossed
    between different experimental units. This helps with not diluting
    the treatment effects by harvesting one plot or another.

**Notes:**

-   The simulations seem to be working well. The ritas algorithm is able
    to recover the simulated nitrogen response quite nicely. Even at
    high noise levels.

## 2024-05-10

**Changes:**

-   Caio changed the "pa_make_vehicle_polygons" function to use two
    distances when building polygons. The distance indexed by "i" is
    understood as the distance between the previous point and the
    current point (i). The distance indexed by "i + 1" is understood as
    the distance between the current point and the next. This allows for
    flexibility when building the polygons and decreases some of the
    artificial overlap between the polygons. The previous function is
    still in "pacu_internal", in case we decide to revert this change.

## 2024-05-03

**Notes:**

-   Upon reading Cressie (2007) <DOI:10.1007/s11004-005-9022-8>, Caio
    realized that the correction proposed by Yammamoto et al (2007) was
    inadequate for block kriging

**Changes:**

-   Caio update the yield monitor vignette

-   Caio added the argument "krige.log.scale" to the "pa_yield_map"
    function. When FALSE, kriging is done in the data scale. When TRUE,
    kriging is done using the **gstat::krigeTs** function.

## 2024-05-02

**Changes:**

-   Caio included the correction proposed by Yammamoto et al (2007) in
    the backtransformation of the kriged values from log to the data
    scale

-   Caio update the satellite data vignette.

-   Caio changed the "plot_ndvi_cuve" function to "plot_vi_curve". It is
    more general now and takes an agument for index values and another
    one for date.

-   Caio made changes to the "pa_get_vi_stats" function. There is a new
    argument "by.feature". The function now accepts multiple polygons
    and when "by.feature" is TRUE, the function will retrieve the vi
    stats for each polygon. When "by.feature" is FALSE, the function
    will return the vi stats for all of the polygons combined.

-   The "pa_get_vi_stats" function now returns an object of class
    "sftime". This is intended to make it easy to manipulate these data
    since they have a time component. It is also easier to visualize the
    data.

## 2024-04-25

**Notes:**

-   Caio needs to work on the documentation of the "pa_yield_map"
    function. It is outdated.

-   The IDW function cannot handle predictors in the formula. Caio added
    an error message to handle that.

**Changes:**

-   Caio added moisture as an attribute returned by the "pa_yield_map"
    function

-   Caio replaced the argument crop in "pa_yield_map" by the argument
    "lbs.per.bushel". The idea is to make this more general and
    adaptable to all crops.

-   Caio included the variance of the kriged predictions in the output
    when "smooth.method" is equal to "krige".

-   Caio added the option to correct for moisture in the "pa_yield_map"
    function. When no moisture correction is supplied, the function
    defaults to the average moisture in the data.

-   Caio changed the "pa_yield_map" function to return a list containing
    a "yield.map" object and a "variogram.model" object. Although the
    "variogram.model" is only relevant when "smooth.method" is "krige",
    the function returns a null object under the "variogram.model" slot
    for consistency.

## 2024-04-24

**Notes:**

-   Add lb/bu in the pa_yield_map

-   Error in "pa_yield_map" when using idw and formula z \~ x + y

**Changes:**

-   Caio implemented unit conversions inside of the "pa_yield_map"
    function. Relevant units are installed using the "units" package whe
    the package is loaded.

-   Caio added the arguments "unit.system" and "crop" to the
    "pa_yield_map" function to help keep track of the units.

-   Caio added a warning stating that the "cores" argument has no effect
    when "algorithm" is simple.

## 2024-04-23

**Meeting notes:**

-   Differences between IDW and Kriging (gis exchange)

-   When using ritas, we can have the verbose spell out ritas steps

-   Include a warning that simple method does not use the cores argument
    at all

-   Look into the Yield Editor software and paper

-   We can look into how to adjust for lag. It could be an argument that
    by default would be none, but could be a number in seconds that
    adjusts the data

-   Moisture adjustment argument

-   Units that will be reported. Arguments and function for those.

-   Comma in line 93 of the yield map function. Make sure it works with
    or without.

-   Include the cardinal date prediction models into this package
    **someday**.

## 2024-04-22

**Changes:**

1)  Removed the parallelization from kriging.

2)  Added argument 'smooth.method' to "pa_yield_map"

3)  Added ellipsis to pass arguments to krige and idw

4)  Added the argument scale.cellsize to pa_yieldmap

**Ideas**

1)  Remove the parallelization for kriging (it is fatal in Windows) this
    is inside the pa_predict function.

2)  Add an argument that could be 'smooth.method' with options such as
    'none', 'idw', 'krige' and with possibilities for using other
    kriging algorithms (such as the one implemented in the 'snapKrige'
    package),

3)  Add arguments explicitly or using the '...' method for controlling
    additional arguments. For example, to allow for changing 'nmin',
    'nmax', 'nsim', 'maxDist', etc. 4) Add an argument for changing the
    size of the prediction cell. This could be a scaling factor, such as
    'scale.cellsize'. You would use the factor to either make larger or
    smaller cell sizes, so it could have a default of 1. I think the
    issues we see in Basswood (prairie strips disappearing) could be
    solved by using a smaller prediction cell size.

## 2024-04-19

**Changes::**

-   Caio replaced all the "compute\_\*\*\*i" functions with the
    "compute_vi" function. Instead of having a separate function for
    each index, the index to be calculated is now an argument.

**Notes:**

-   Caio identified a possible bug in the "compute\_\*\*\*i" functions.
    It seems that for some of the older satellite files, the band names
    might not include a pixel resolution specification. It would be good
    to add an attempt to identify the band name without the pixel
    resolution specification in case no bands are found.

## 2024-04-18

**Meeting notes:**

-   Include Landsat data in the stats API

-   Cores is crashing windows. We might want to limit the number of
    cores in different operating systems.

-   Add argument to adjust moisture in the yield map function. Put the
    default at the average observed moisture.

-   We might want to add a "yield monitor" class to the pacu objects and
    handle plotting differently.

-   We can try alternative packages or methods for Kriging.

## 2024-04-17

-   Fernando has made a few changes to pa_yield_map function. Stop if
    not 'data.columns' is specified and use sf::st_aggregate.sf instead
    of :::. The form ::: should never be used inside packages unless you
    are borrowing functions from another package which are not exported,
    but this is not good practice in general.

## 2024-04-16

**Notes:**

-   We have shared a version of pacu with the DIFM team.

-   Caio still needs to work on the package documentation.

## 2024-04-11

**Notes:**

-   Caio tested the algorithm on the "Wendt 2018" field from the DIFM,
    it seems like we were able to reduce the noise of the data
    considerably. However, not enough to get a significant relationship
    between yield and seeding density.

**Changes:**

-   Caio has included a new function to generate the field boundary from
    the collected points. Although this might seem trivial, it was
    harder than anticipated. This is important because, regardless of
    the method used to generate the yield maps (ritas or simple), the
    covered area remais the same.

-   Caio has included an option in "pa_yield_map" so that users can
    supply a boundary geometry.

-   Caio implemented the option to clean the raw data based on standard
    deviation and distance from the field edge.

## 2024-04-10

**Notes:**

-   Caio tested the "pa_yield_map" function with the combinations of
    krige an algorithm arguments. It seems like everything is working as
    it should.

**Changes:**

-   Caio added an option to "pa_yield_map" that controls weather or not
    to krige. Also, added an option called "simple" to algorithm. This
    runs a simpler algorithm than RITAS.

## 2024-04-09

**Notes:**

-   Caio tested the "pa_yield_map" function with the defaults in 38 of
    the Basswood data sets. Upon vision inspection, 35 of the resulting
    yield maps looked great. There are two yield maps (2015 orbweaver
    north and south) in which the unit guessing did not work. The
    combine was moving slower, so the distance between points is
    smaller, resulting in the function guessing meters instead of feet.
    Additionally, the 2008 orbweaver yield map shows a north-south
    pattern that seems artificial.

-   Caio is testing the "pa_yield_map" function on the DIFM field
    "Nelson Dougshome 2017". The idea is to use the same framework to
    aggregate both the yield and the treatment maps.

-   Caio still needs to split the vignette in three and add simpler
    options for yield maps.

## 2024-04-04

**Notes:**

-   Caio tested the unit specification option with the script
    "testing-ritas-units.R". It seems like everything is working as it
    should.

-   Caio still needs to include a simpler procedure for yield mapping
    and split the vignette in three parts.

**Changes:**

-   Caio added the option to specify units and column names in the
    "pa_yield_map" function.

## 2024-04-03

**Notes:**

-   Caio tested the RITAS algorithm on all of the Basswood raw yield
    files. It seems like it worked really well. Results in
    "pacu/tests-results/basswood-ritas".

-   Caio added the Basswood data in "pacu/tests-data". The idea is to
    not ship this data with the package but have it locally so we can
    test the package when we make changes. Please let me know if that is
    not adequate.

-   It would be a good idea to include an argument in the functions
    related to yield maps in which the user could specify the units, so
    there would be no need to guess them. This would also allow users to
    fix things if we've guessed wrong.

-   Caio's next tasks will be to add options for simpler procedures in
    the yield map function, allow for unit specification in the yield
    map functions, and to split the vignette in three different
    vignettes.

**Changes:**

-   Caio added a wrapper function for creating yield maps and an option
    to parallelize the kriging process.

-   Caio wrote a function that reads the dictionary in the
    "inst/extdata" folder and tries to guess which columns in the raw
    data represents crop flow, distance, and other necessary columns.

-   Caio wrote a function that reads a given variable's values and
    attempts to guess the units of the variable. To some extent, the
    "units" package was added to the "yield map" functions to help
    enforce units.

## 2024-04-02

**Changes:**

-   FEM added vignette 'pacu_weather.Rmd' the idea is to have dedicated
    vignettes for the different components of the package. One for
    weather, one for images and one for yield monitor related
    operations. I have not deleted weather stuff from the main
    'pacu.Rmd' vignette. I will let Caio do this.

-   FEM fixed minor issue in documentation of 'pa_get_weather_shp'.
    Added 'ggplot2::' and 'patchwork::' calls in the weather_summary.R
    file

## 2024-03-29

**Changes:**

-   Caio included "pa\_" at the beginning of all function names

-   Caio changed "request_vi_stats" to "pa_get_vi_stats"

## 2024-03-28

Suggestions and changes

**Suggestions**

-   Rename all functions starting with 'pa\_'. This is something many
    packages adopt to simplify auto-complete. Since it is early in the
    package development, I think we should do it.

-   how about renaming 'request_vi_stats' to 'pa_get_vi_stats'. I use
    'get\_' in the apsimx package because it is short and easier to type

**Changes** \* FEM deleted the 'hello.R' file \* Changed version to
0.1.1 \* changed documentation from 'clear_yield_monitor' to
'clean_yield_monitor' \* corrected minor spelling mistakes in
documentation \* In function 'aggregate_design_polygons' I changed
variable 'tgt.cols'. It was in the body of the function and the argument
would have never had an effect. If we want those columns to be used as
defaults. It can be placed in the function arguments. This is what I
did, Caio change it if it is wrong.
