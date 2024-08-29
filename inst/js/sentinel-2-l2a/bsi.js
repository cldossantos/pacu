//VERSION=3
function setup() {
  return {
    input: [{
      bands: [
        "B02",
        "B08",
        "B11",
        "B04",
        "SCL",
        "dataMask"
      ]
    }],
    output: [
      {
        id: "data",
        bands: 1
      },
      {
        id: "dataMask",
        bands: 1
      }]
  }
}

function evaluatePixel(samples) {
    let     bsi = ((samples.B11 + samples.B04) - (samples.B08 + samples.B02)) / ((samples.B11 + samples.B04) + (samples.B08 + samples.B02))

    var noInterferanceMask = 1
    if (samples.SCL >= 6 || samples.SCL <= 3 ){
        noInterferanceMask = 0
    }

    return {
        data: [bsi],
        dataMask: [samples.dataMask  * noInterferanceMask]
    }
}
