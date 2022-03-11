package innovationinformal

case class Firm(
                 /**
                  * Geographical location of the firm
                  */
               location: (Double, Double),

                 /**
                  * employees ids
                  */
               employeesIds: Seq[Int],

                 /**
                  * genome of the current product
                  */
               currentProduct: Seq[Double]
               )
