package nicoburniske.faxion.image

package object morph {
  val DIMENSION = 2

  val DEFAULT_STRUCTURING_ELEMENT: Set[(Int, Int)] = {
    val range = -DIMENSION to DIMENSION
    for {
      x <- range
      y <- range
    } yield (x, y)
  }.toSet
}
