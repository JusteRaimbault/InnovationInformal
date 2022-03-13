package innovationinformal

object Utils {

  def norm(s: Seq[Double]): Double = math.sqrt(s.map{si => si*si}.sum)


}
