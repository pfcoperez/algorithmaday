package org.pfcoperez.dailyalgorithm.applications

object MandelbrotSet {

  /**
   * A single iteration of the numeric method
   *  O(1)
   *
   * @return `None` if the point escapes the set,
   *         `Some(new complex state)` otherwise.
   */
  def iteration(
    zeroth: (Double, Double))(
    xy: (Double, Double)): Option[(Double, Double)] = {
    val (x, y) = xy
    val (x0, y0) = zeroth

    if (x * x + y * y >= 4.0) None
    else Some((x * x - y * y + x0, 2.0 * x * y + y0))
  }

  /**
   * Several iterations, it can be used to approximate
   *  Mandelbrot's set. The more iterations,the better
   *  the approximation is.
   *
   * O(nIterations)
   *
   * @zeroth Start point, e.g: (x,y) \in ([-2.5, 1], [-1,1]) area.
   * @nIterations Max depth to be check in the recursion.
   * @prevSt Starting point in the expliration
   * @return The state after `nIterations` or condition escape.
   *
   */
  def numericExploration(
    zeroth: (Double, Double),
    nIterations: Int,
    prevSt: Option[((Double, Double), Int)] = None): (Option[(Double, Double)], Int) = {

    val (prevPoint, prevIterations) = prevSt.getOrElse((0.0, 0.0) -> 0)

    def numericExploration(current: (Double, Double), it: Int): (Option[(Double, Double)], Int) =
      if (it == nIterations) (Some(current), prevIterations + it)
      else iteration(zeroth)(current) match {
        case Some(p) => numericExploration(p, it + 1)
        case _ => (None, prevIterations + it)
      }

    numericExploration(prevPoint, 0)

  }

}
