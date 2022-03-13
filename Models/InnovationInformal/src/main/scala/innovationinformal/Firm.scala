package innovationinformal

import innovationinformal.InnovationInformal.Fitness

import scala.jdk.CollectionConverters.IterableHasAsJava
import scala.util.Random
import org.apache.commons.math3.random.RandomDataGenerator

case class Firm(
                 /**
                  * Geographical location of the firm
                  */
               location: (Double, Double),

                 /**
                  * employees
                  */
               employees: Seq[Employee],

                 /**
                  * genome of the current product
                  */
               currentProduct: Seq[Double],

               currentFitness: Double
               ){

}

object Firm {

  /**
   * Firm with random location, employees and product
   *  return firm and remaining employees
   * @param size firm size
   * @param allEmployees all employee ids
   * @return
   */
  def randomFirm(size: Int, allEmployees: Seq[Employee], fitness: Fitness)(implicit rng: Random): (Firm, Seq[Employee]) = {
    val loc = (100.0*rng.nextDouble(), 100.0*rng.nextDouble())
    // sample without replacement
    val sampler = new RandomDataGenerator()
    sampler.reSeed(rng.nextLong())
    val employees: Seq[Employee] = sampler.nextSample(allEmployees.asJavaCollection, size).map(_.asInstanceOf[Employee]).toSeq
    // random product at the beginning: take the first
    val product = employees.head.currentIdeas
    // remaining employees - sampling not optimal
    val remaining = allEmployees.filter(!employees.contains(_))
    (Firm(loc, employees, product, fitness.fitness(product)), remaining)
  }


  /**
   * Innovation within a firm
   * @param firm firm
   * @param crossOverProba crossoverproba for one individual
   * @param crossOverShare share of genome being copied
   * @param mutationProba mutation proba for each gene of each individual
   * @param mutationAmplitude amplitude of the uniform mutation
   * @param currentProductShare share of individuals copying the best genome
   * @param fitness fitness function
   * @param rng rng
   * @return
   */
  def innovate(firm: Firm,
               crossOverProba: Double,
               crossOverShare: Double,
               mutationProba: Double,
               mutationAmplitude: Double,
               currentProductShare: Double,
               fitness: Seq[Double] => Double
              )(implicit rng: Random): Firm = {
    // each employee has crossOverProba chances of exchanging ideas with an other at random
    // to simplify, copy a random sequence of the other genome
    val exchangedIdeas = firm.employees.map{e =>
      if (rng.nextDouble()<crossOverProba){
        val copied = firm.employees(rng.nextInt(firm.employees.length)).currentIdeas
        e.copy(currentIdeas = e.currentIdeas.zip(copied).map{case (s,o) => if (rng.nextDouble()<crossOverShare) o else s})
      }
      else e
    }


    // mutate ideas (uniform with fixed amplitude)
    val mutatedIdeas = exchangedIdeas.map{e =>
      e.copy(currentIdeas = e.currentIdeas.map{i => if (rng.nextDouble()<mutationProba) i - mutationAmplitude/2.0 + mutationAmplitude/2.0*rng.nextDouble() else i})
    }

    // find best idea
    val fitnessesAndIndivs = mutatedIdeas.map(e => (fitness(e.currentIdeas), e))
    val best = fitnessesAndIndivs.maxBy(_._1)
    val finalIndivs = fitnessesAndIndivs.map{case (_,e) =>
      if (e==best._2) e
      else {
        if (rng.nextDouble()< currentProductShare) e.copy(currentIdeas = best._2.currentIdeas)
        else e
      }
    }

    firm.copy(employees = finalIndivs, currentProduct = best._2.currentIdeas, currentFitness = best._1)
  }


  /**
   *
   *
   *  rq: implementation not optimal, could draw random pair in an other way - ok with reasonable firm numbers and sizes
   * @param firms firms
   * @return
   */
  def exchangeInformal(firms: Seq[Firm],
                       interactionProba: Double,
                       distanceDecay: Double,
                       crossOverShare: Double
                      )(implicit rng: Random): (Seq[Firm], Double) = {
    var totalInteractions = 0
    var potentialInteractions = 0
    (firms.map{fi =>
      val updatedEmployees = fi.employees.map{ei =>
          val newIdeas = ei.currentIdeas.toArray // mutable locally
          firms.foreach { fj =>
            val expdij = math.exp(-1.0*math.sqrt(math.pow(fi.location._1-fj.location._1, 2.0)+math.pow(fi.location._2-fj.location._2, 2.0))/distanceDecay)
            if (fj != fi) {
              potentialInteractions = potentialInteractions + fi.employees.size*fj.employees.size
              fj.employees.foreach { ej =>
                if (rng.nextDouble() < interactionProba * expdij) {
                  ej.currentIdeas.zipWithIndex.foreach { case (i, ind) => if (rng.nextDouble() < crossOverShare) newIdeas(ind) = i }
                  totalInteractions = totalInteractions + 1
                }
              }
            }
          }
          ei.copy(currentIdeas = newIdeas.toSeq)
          }
      fi.copy(employees = updatedEmployees)
      },
      totalInteractions.toDouble/potentialInteractions.toDouble)
    }





}


