package innovationinformal

import scala.util.Random

case class Employee(
                     /**
                      * unique id - used to map firms to employees
                      */
                   id: Int,

                     /**
                      * genome of current ideas of the employee
                      */
                   currentIdeas: Seq[Double]
                   ){

}

object Employee {

  def apply(i: Int, genomeFunction: Int => Seq[Double]): Employee = Employee(i,genomeFunction(i))

  /*
  def randomEmployee(i: Int, genome: (Random, Int)=> Seq[Double])(implicit rng: Random): Employee = Employee(
    i,
    Seq.fill(genomeSize)(rng.nextDouble())
  )*/


}
