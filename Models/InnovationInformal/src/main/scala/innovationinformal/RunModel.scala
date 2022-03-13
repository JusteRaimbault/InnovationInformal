package innovationinformal

import scala.util.Random

object RunModel extends App {

  implicit val rng: Random = new Random

  val genomeSize = 10

  val model = InnovationInformal(
    firmsNumber = 10,
    largestFirmSize = 100,
    firmSizeScaling = 1.0,
    genomeSize = genomeSize,
    timeSteps = 100,
    crossOverProba = 0.2,
    crossOverShare = 0.5,
    mutationProba = 0.01,
    mutationAmplitude = 1.0,
    currentProductShare = 0.5,
    interactionProba = 0.00001, // transition between 1e-4 and 1e-5 at decay 100
    distanceDecay = 100.0,
    InnovationInformal.randomGeneralizedRastrigin(genomeSize)
  )

  val res: ModelResult = InnovationInformal.run(model)

  println("Best fitnesses tf = "+res.bestFitnesses.last)
  println("Fitness ineq tf = "+res.fitnessDifferences.last)
  println("Fitness entropy tf = "+res.fitnessEntropies.last)
  println("Diversity tf = "+res.productDiversities.last)
  println("interactionIntensity = "+res.interactionIntensity)

}
