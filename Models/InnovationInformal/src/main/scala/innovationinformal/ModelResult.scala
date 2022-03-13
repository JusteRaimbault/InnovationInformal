package innovationinformal


/**
 *  Indicators?
 *   - innovation? -> fitness (could measure distance between genomes?)
 *   - diversity
 *   - inequality between firms -> ecart min max et entropie?~ captured in diversity (but a bit different)
 *   - interaction intensity
 *
 * @param states model states
 */
case class ModelResult(
                      states: Seq[ModelState]
                      ){
  
  def fitnesses: Array[Array[Double]] = states.map(_.firms.map(_.currentFitness).toArray).toArray

  def bestFitnesses: Array[Double] = states.map(_.firms.map(_.currentFitness).max).toArray

  def worseFitnesses: Array[Double] = states.map(_.firms.map(_.currentFitness).min).toArray

  def fitnessDifferences: Array[Double] = states.map{s => math.abs(s.firms.map(_.currentFitness).max - s.firms.map(_.currentFitness).min)/math.abs(s.firms.map(_.currentFitness).max)}.toArray

  def interactionIntensity: Double = states.drop(1).map(_.interactionIntensity).sum/states.size.toDouble

  /**
   * diversity of product across firms for all time steps
   * @return
   */
  def productDiversities: Array[Double] = states.map(ModelResult.productDiversity).toArray

  def fitnessEntropies: Array[Double] = states.map(ModelResult.fitnessEntropy).toArray

}


object ModelResult {

  /**
   * diversity of genomes?
   *   -> 1 - sum_pi2 ? avg across all genes? ~
   *   -> cosine similarity? rather than eucl distance?: avg cosine similarity across all pairs - or proximity (1 - cosinesim)/2
   * @param state model state
   * @return
   */
  def productDiversity(state: ModelState): Double = {
    val products = state.firms.map(_.currentProduct)
    val proximities = products.flatMap{pi =>
      val ni = Utils.norm(pi)
      products.map{pj =>
        val nj = Utils.norm(pj)
        (1 - (pi.zip(pj).map{case (pik,pjk) => pik*pjk}.sum/(ni*nj)))/2.0
      }
    }
    proximities.sum/proximities.size.toDouble
  }

  def fitnessEntropy(state: ModelState): Double = {
    val fitnesses: Seq[Double] = state.firms.map(_.currentFitness)
    val (mi, ma) = (fitnesses.min, fitnesses.max)
    //val normfitnesses = fitnesses.map(f => (f - mi)/(ma - mi))
    val normfitnesses = if (mi < 0.0) fitnesses.map(f => f - mi) else fitnesses // entropy % 0 (normalisation ma/mi gives always similar entropy)
    val s = normfitnesses.sum
    val p = normfitnesses.map(_ / s)
    val n = p.size.toDouble
    -1.0*p.map(pi => if(pi==0.0) 0.0 else pi*math.log(pi)).sum/(n*math.log(n))
  }

}

