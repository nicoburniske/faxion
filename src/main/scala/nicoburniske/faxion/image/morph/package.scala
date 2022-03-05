package nicoburniske.faxion.image

package object morph {
  val DIMENSION                                    = 2
  val DEFAULT_STRUCTURING_ELEMENT: Set[(Int, Int)] = circleElem(DIMENSION)

  def squareElem(distanceFromCenterToEdge: Int): Set[(Int, Int)] = {
    val range = -distanceFromCenterToEdge to distanceFromCenterToEdge
    val res   = for {
      x <- range
      y <- range
    } yield (x, y)
    res.toSet
  }

  def circleElem(radius: Int): Set[(Int, Int)] = {
    squareElem(radius).filter {
      case (x, y) =>
        Math.sqrt(Math.pow(x, 2) + Math.pow(y, 2)) <= radius - 1
    }
  }

  def crossElement(distanceFromCenterToEdge: Int): Set[(Int, Int)] = {
    squareElem(distanceFromCenterToEdge).filter { case (x, y) => x == 0 || y == 0 }
  }
}
