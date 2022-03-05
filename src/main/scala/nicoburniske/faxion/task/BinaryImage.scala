package nicoburniske.faxion.task

import java.awt.Color

import com.sksamuel.scrimage.ImmutableImage

// 1 - foreground
// 0 - background
object BinaryImage {
  def apply(image: ImmutableImage): BinaryImage = {
    val data: Array[Boolean] = Array.ofDim(image.width * image.height)
    image.forEach { pixel =>
      val color = pixel.toColor.awt()
      val value =
        if (color == Color.BLACK)
          false
        else
          true
      data(pixel.x + pixel.y * image.width) = value
    }
    BinaryImage(data, image.width, image.height)
  }
}

case class BinaryImage(data: Array[Boolean], width: Int, height: Int) {
  val getOption = data.lift

  def getOption(x: Int, y: Int): Option[Boolean] = {
    getOption(tupleToIndex(x, y))
  }

  def tupleToIndex(x: Int, y: Int): Int                   = y * width + x
  def tupleToIndex(tuple: (Int, Int)): Int                = tupleToIndex(tuple._1, tuple._2)
  def indexToTuple(index: Int): (Int, Int)                = (index % width, index / width)
  def addTuples(a: (Int, Int), b: (Int, Int)): (Int, Int) = {
    val l = a._1 + b._1
    val r = a._2 + b._2
    l -> r
  }

  def dilate(se: Set[(Int, Int)]): BinaryImage = {
    val newData: Array[Boolean] = Array.ofDim(data.length)
    for {
      index   <- data.indices
      point    = indexToTuple(index)
      hasWhite = se.map(addTuples(point, _)).map(tupleToIndex).flatMap(getOption).exists(identity)
    } newData(index) = hasWhite
    // (3, 3) 4x5 ->
    this.copy(data = newData)
  }
}
