package innovationinformal

case class Employee(
                     /**
                      * unique id - used to map firms to employees
                      */
                   id: Int,

                     /**
                      * genome of current ideas of the employee
                      */
                   currentIdeas: Seq[Double]
                   )
