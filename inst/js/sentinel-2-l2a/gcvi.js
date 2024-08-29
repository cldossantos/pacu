//VERSION=3
function setup() {
  return {
    input: [{
      bands: [
        "B03",
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
    let gcvi = (samples.B08 / samples.B03) - 1


    var noInterferanceMask = 1
    if (samples.SCL >= 6 || samples.SCL <= 3 ){
        noInterferanceMask = 0
    }

    return {
        data: [gcvi],
        dataMask: [samples.dataMask  * noInterferanceMask]
    }
}
