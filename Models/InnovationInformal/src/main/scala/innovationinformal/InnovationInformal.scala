package innovationinformal

import scala.util.Random

case class InnovationInformal(
                               firmsNumber: Int,
                               largestFirmSize: Int,
                               firmSizeScaling: Double,
                               genomeSize: Int,
                               timeSteps: Int,
                               crossOverProba: Double,
                               crossOverShare: Double,
                               mutationProba: Double,
                               mutationAmplitude: Double,
                               currentProductShare: Double,
                               interactionProba: Double,
                               distanceDecay: Double,
                               fitness: InnovationInformal.Fitness
                             ) {

}


object InnovationInformal {

  sealed trait Fitness {
    def genome(rng: Random): Seq[Double]
    def fitness(g: Seq[Double]): Double
  }

  case class GeneralizedRastriginFitness(m: Array[Array[Double]], genomeSize: Int) extends Fitness {
    override def genome(rng: Random): Seq[Double] = Seq.fill(genomeSize)(10.0*(rng.nextDouble()-0.5)) // in [-5,5]
    override def fitness(g: Seq[Double]): Double = {
      val res = m.map{row =>
        row.zip(g).map{case (mij, gj)=>
          mij*(gj*gj - 10.0*math.cos(2.0*math.Pi*gj))
        }.sum
      }.sum
      -1.0*res
    }
  }

  def randomGeneralizedRastrigin(size: Int)(implicit rng: Random): GeneralizedRastriginFitness = GeneralizedRastriginFitness(
    Array.fill(size,size)(rng.nextDouble()),
    size
  )



  def modelSetup(firmsNumber: Int, largestFirmSize: Int, firmSizeScaling: Double, fitness: Fitness)(implicit rng: Random): ModelState = {
    // size of firms: rank size law
    val sizes: Seq[Int] = (1 to firmsNumber).map(i => math.round(largestFirmSize.toDouble*math.pow(i.toDouble, -firmSizeScaling)).toInt)

    println("Company sizes = "+sizes)

    val employees: Seq[Employee] = (1 to sizes.sum).map{i =>
      //Employee.randomEmployee(i, genome)
      Employee(i, fitness.genome(rng))
    }
    val firms = Iterator.iterate((Seq.empty[Firm], employees, 0)){
      case (firms, employees, sizeIndex) =>
        val (newfirm, remainingEmployees) =  Firm.randomFirm(sizes(sizeIndex), employees, fitness)
        (firms++Seq(newfirm), remainingEmployees, sizeIndex+1)
    }.takeWhile(_._3<firmsNumber).toSeq.last._1

    //println(firms.map{_.employees.map(_.currentIdeas.size)})

    ModelState(firms, 0.0, 0)
  }





  def modelStep(state: ModelState, model: InnovationInformal)(implicit rng: Random): ModelState = {
    import model._

    //println("Step "+(state.time + 1))
    //println(state.firms.map{_.employees.size}.sum)

    // innovate within firms
    val innovFirms = state.firms.map { f =>
      Firm.innovate(f, crossOverProba, crossOverShare, mutationProba, mutationAmplitude, currentProductShare, fitness.fitness)
    }
    //println(innovFirms.flatMap{_.employees.map(_.currentIdeas.size)}.max)
    //println(innovFirms.map{_.employees.size}.sum)

    // exchange knowledge: spatial interaction potential SiSj exp(-d_ij) * interactionsIntensity
    // equivalent to: for each pair of employee, proba interactionIntensity*exp(-d_ij) of exchanging?
    // use copy heuristic (not symmetric: we do not learn the same thing from the other)
    val (exchangedFirms, interactionIntensity) = Firm.exchangeInformal(innovFirms, interactionProba, distanceDecay, crossOverShare)
    //println(innovFirms.flatMap{_.employees.map(_.currentIdeas.size)}.max)
    //println(exchangedFirms.map{_.employees.size}.sum)

    state.copy(firms = exchangedFirms, interactionIntensity = interactionIntensity, time = state.time + 1)
  }


  def run(model: InnovationInformal)(implicit rng: Random): ModelResult = {
    import model._
    val initialState = modelSetup(firmsNumber, largestFirmSize, firmSizeScaling, fitness)
    def f(s: ModelState): ModelState = modelStep(s, model)
    val states = Iterator.iterate(initialState)(f).takeWhile(_.time <= model.timeSteps).toSeq
    ModelResult(states)
  }

}


