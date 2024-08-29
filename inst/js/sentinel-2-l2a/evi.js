//VERSION=3
function setup() {
  return {
    input: [{
      bands: [
        "B04",
        "B08",
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
    let evi = 2.5 * (samples.B08 - samples.B04) / (samples.B08 + (2.4 * samples.B04) + 1)

    var noInterferanceMask = 1
    if (samples.SCL >= 6 || samples.SCL <= 3 ){
        noInterferanceMask = 0
    }

    return {
        data: [evi],
        dataMask: [samples.dataMask *  noInterferanceMask]
    }
}
