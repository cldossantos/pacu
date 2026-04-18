//VERSION=3
function setup() {
  return {
    input: [{
      bands: [
        "B05",
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
    let ndvi = (samples.B08 - samples.B05)/(samples.B08 + samples.B05)

    var validNDVIMask = 1
    if (samples.B08 + samples.B05 == 0 ){
        validNDVIMask = 0
    }

    var noInterferanceMask = 1
    if (samples.SCL >= 6 || samples.SCL <= 3 ){
        noInterferanceMask = 0
    }

    return {
        data: [ndvi],
        dataMask: [samples.dataMask * validNDVIMask * noInterferanceMask]
    }
}
