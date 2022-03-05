package nicoburniske.faxion.image

package object morph {
  val DIMENSION = 2

  val DEFAULT_STRUCTURING_ELEMENT: Set[(Int, Int)] = {
    val range = -DIMENSION to DIMENSION
    for {
      x <- range
      y <- range
    } yield (x, y)
  }.filter { case (x, y) => Math.sqrt(Math.pow(x, 2) + Math.pow(y, 2)) <= DIMENSION - 1 }.toSet
}
