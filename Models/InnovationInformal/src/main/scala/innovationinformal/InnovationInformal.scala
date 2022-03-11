package innovationinformal

case class InnovationInformal(
                               firmsNumber: Int,
                               largestFirmSize: Int,
                               firmSizeScaling: Double
                             ) {

}


object InnovationInformal {




  def modelSetup(): ModelState = {

    ModelState()
  }


  def modelStep(state: ModelState): ModelState = {

    ModelState()
  }


  def run(): ModelResult = ModelResult()

}


