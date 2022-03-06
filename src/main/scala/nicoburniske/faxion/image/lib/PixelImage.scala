package nicoburniske.faxion.image.lib

import cats.effect.Async
import cats.syntax.all._
import cats.{Applicative, Parallel}

import scala.reflect.ClassTag

/**
 * a basic pixel-based image, access through pixel coordinates (x, y)
 */
class PixelImage[@specialized A](
    val domain: PixelImageDomain,
    val accessMode: AccessMode[A],
    val f: (Int, Int) => A)
    extends ((Int, Int) => A) {

  val width: Int  = domain.width
  val height: Int = domain.height
  val length: Int = domain.length

  /**
   * access image at (x, y)
   */
  def apply(x: Int, y: Int): A =
    if (domain.isDefinedAt(x, y)) valueAt(x, y) else accessMode.outsideAccess(x, y, this)

  /**
   * direct raw access of image value at (x, y) - unchecked!
   */
  def valueAt(x: Int, y: Int): A = f(x, y)

  /**
   * change the access mode (boundary behaviour, outside access) of this image
   */
  def withAccessMode(accessMode: AccessMode[A]): PixelImage[A] = new PixelImage(domain, accessMode, f)

  /**
   * apply a generic filter to this image - always non-destructive
   */
  def filter[@specialized B](imageFilter: ImageFilter[A, B]): PixelImage[B] = imageFilter.filter(this)

  /**
   * transpose image, exchange x and y
   */
  def transposed: PixelImage[A] = PixelImage.view(domain.transpose, (x, y) => this(y, x))

  /**
   * selected row as image, image view only
   */
  def row(y: Int): PixelImage[A] = PixelImage.view(width, 1, (x, _) => this(x, y))

  /**
   * selected column as image, image view only
   */
  def col(x: Int): PixelImage[A] = PixelImage.view(1, height, (_, y) => this(x, y))

  /**
   * buffer the image contents with an Array
   */
  def buffer(implicit tag: ClassTag[A]): PixelImage[A] = {
    val data = new Array[A](domain.length)
    domain.indices.foreach(i => data(i) = this(domain.x(i), domain.y(i)))
    new ArrayImage[A](domain, accessMode, data)
  }

  /**
   * get all values as data Array (linearization strategy depends on domain, creates a copy)
   */
  def toArray(implicit tag: ClassTag[A]): Array[A] = values.toArray

  /**
   * create a new ImageBuffer initialized with this image (does not change this image)
   */
  def toBuffer(implicit tag: ClassTag[A]): ImageBuffer[A] = ImageBuffer(domain, toArray)

  /**
   * apply a function to each pixel
   */
  def map[B](f: A => B)(implicit tag: ClassTag[B]): PixelImage[B] = mapLazy(f).buffer

  /**
   * apply a function to each pixel
   */
  def mapLazy[B](f: A => B): PixelImage[B] = PixelImage.view(domain, (x, y) => f(this.f(x, y)))

  /**
   * apply a function to each pixel, has access to location of pixel
   */
  def mapWithIndex[B](f: (A, Int, Int) => B)(implicit tag: ClassTag[B]): PixelImage[B] =
    PixelImage(domain, (x, y) => f(this(x, y), x, y))

  /**
   */
  def parMap[F[_]: Parallel: Async, B](f: A => B)(implicit tag: ClassTag[B]): F[PixelImage[B]] = {
    Applicative[F].pure(mapLazy(f))
  }

  /**
   * zip two images together into a single tupled image
   */
  def zip[B](other: PixelImage[B]): PixelImage[(A, B)] = {
    require(domain == other.domain)
    PixelImage.view(domain, (x, y) => (this(x, y), other(x, y)))
  }

  /**
   * zip pixel values with their location, yields (Color, (x, y))
   */
  def zipWithIndex: PixelImage[(A, (Int, Int))] = {
    PixelImage.view(domain, (x, y) => (this(x, y), (x, y)))
  }

  /**
   * iterator of all values in image
   */
  def values: Iterator[A] = domain.indices.iterator.map(i => this(domain.x(i), domain.y(i)))

  /**
   * view this image with another domain
   */
  def withDomain(domain: PixelImageDomain): PixelImage[A] = {
    require(domain.width == width && domain.height == height, "domain size does not match")
    PixelImage.view(domain, (x, y) => this(x, y))
  }

  /**
   * fast folding: use for summing without boxing
   */
  def foldLeft[@specialized B](init: B)(f: (B, A) => B): B = {
    var acc = init
    var i   = 0
    while (i < length) {
      acc = f(acc, this(domain.x(i), domain.y(i)))
      i += 1
    }
    acc
  }

  val isBuffered = false

  override def toString: String = "PixelImage(" + domain + ")"

  // correct equality with respect to underlying data
  override def equals(other: Any): Boolean = other match {
    case pi: PixelImage[A] => domain == pi.domain && (values sameElements pi.values)
    case _                 => false
  }

  // correct hashCode with respect to underlying data
  override def hashCode: Int = domain.hashCode() * 43 + values.toIndexedSeq.hashCode()
}

object PixelImage {

  /**
   * create a PixelImage from a function (creates buffered ArrayImage)
   */
  def apply[A: ClassTag](domain: PixelImageDomain, f: (Int, Int) => A): PixelImage[A] = {
    view(domain, f).buffer
  }

  /**
   * create a PixelImage from a function (creates buffered ArrayImage)
   */
  def apply[A: ClassTag](
      domain: PixelImageDomain,
      f: (Int, Int) => A,
      accessMode: AccessMode[A]): PixelImage[A] = {
    view(domain, f).buffer.withAccessMode(accessMode)
  }

  /**
   * create a PixelImage from a function (creates buffered ArrayImage), default domain
   */
  def apply[A: ClassTag](width: Int, height: Int, f: (Int, Int) => A): PixelImage[A] = {
    val domain = PixelImageDomain(width, height)
    PixelImage(domain, f)
  }

  /**
   * create a PixelImage from a function (creates buffered ArrayImage), default domain
   */
  def apply[A: ClassTag](
      width: Int,
      height: Int,
      f: (Int, Int) => A,
      accessMode: AccessMode[A]): PixelImage[A] = {
    val domain = PixelImageDomain(width, height)
    view(domain, f).buffer.withAccessMode(accessMode)
  }

  /**
   * create a PixelImage with given data
   */
  def apply[A: ClassTag](domain: PixelImageDomain, data: Array[A]): PixelImage[A] = {
    require(data.length == domain.length, "data array is of wrong size for image domain")
    new ArrayImage[A](domain, AccessMode.Strict(), data)
  }

  /**
   * create a PixelImage with given data
   */
  def apply[A: ClassTag](domain: PixelImageDomain, data: IndexedSeq[A]): PixelImage[A] = {
    require(data.length == domain.length, "data array is of wrong size for image domain")
    new ArrayImage[A](domain, AccessMode.Strict(), data.toArray)
  }

  /**
   * create image view (lazy image) with default domain
   */
  def view[A](width: Int, height: Int, f: (Int, Int) => A): PixelImage[A] = {
    val domain = PixelImageDomain(width, height)
    view(domain, f)
  }

  /**
   * create image view (lazy image) with domain
   */
  def view[A](domain: PixelImageDomain, f: (Int, Int) => A): PixelImage[A] =
    new PixelImage(domain, AccessMode.Functional(f), f)

  /**
   * create a PixelImage on the basis of an existing image, preserves domain and access mode
   */
  def fromTemplate[A: ClassTag](image: PixelImage[A], f: (Int, Int) => A): PixelImage[A] = {
    view(image.domain, f).buffer.withAccessMode(image.accessMode)
  }

  /**
   * create an image from a template, preserves accessMode and concrete image type
   */
  def fromTemplate[A: ClassTag](
      image: PixelImage[A],
      width: Int,
      height: Int,
      f: (Int, Int) => A): PixelImage[A] = {
    val accessMode = image.accessMode
    val domain     = image.domain match {
      case _: ColumnMajorImageDomain => ColumnMajorImageDomain(width, height)
      case _: RowMajorImageDomain    => RowMajorImageDomain(width, height)
    }
    view(domain, f).buffer.withAccessMode(accessMode)
  }

  /**
   * implicitly added image operators if ColorSpaceOperations are available
   */
  object implicits {

    // operators
    implicit class ImageWithOperators[A: ClassTag](image: PixelImage[A])(
        implicit ops: ColorSpaceOperations[A]) {

      private def applyOperation2[B, C: ClassTag](other: PixelImage[B])(f: (A, B) => C): PixelImage[C] = {
        require(image.domain.size == other.domain.size, "images must be of same size for math operations")
        PixelImage(image.domain, (x, y) => f(image(x, y), other(x, y)))
      }

      def +(other: PixelImage[A]): PixelImage[A] = applyOperation2(other)(ops.add)

      def x(other: PixelImage[A]): PixelImage[A] = applyOperation2(other)(ops.multiply)

      def multiply(other: PixelImage[A]): PixelImage[A] = applyOperation2(other)(ops.multiply)

      def *(other: PixelImage[Double]): PixelImage[A] = applyOperation2(other)(ops.scale)

      def *:(other: PixelImage[Double]): PixelImage[A] = applyOperation2(other)(ops.scale)

      def /(other: PixelImage[Double]): PixelImage[A] =
        applyOperation2(other)((a, l) => ops.scale(a, 1.0f / l))

      def dot(other: PixelImage[A]): Double = applyOperation2(other)(ops.dot).values.sum

      def normSq: Double = image.values.map(ops.normSq).sum

      def norm: Double = math.sqrt(normSq)

      def zero: PixelImage[A] =
        PixelImage.view(image.domain, (_, _) => ops.zero).withAccessMode(image.accessMode)
    }
  }
}

private class ArrayImage[A: ClassTag](
    override val domain: PixelImageDomain,
    override val accessMode: AccessMode[A],
    private val data: Array[A])
    extends PixelImage[A](domain, accessMode, (x: Int, y: Int) => data(domain.index(x, y))) {
  require(data.length == domain.length, "domain and array differ in size")

  /**
   * direct raw access of image value at (x, y)
   */
  override def valueAt(x: Int, y: Int): A = data(domain.index(x, y))

  /**
   * buffer the image contents with an Array
   */
  override def buffer(implicit tag: ClassTag[A]): PixelImage[A] = this

  /**
   * change the access mode (boundary behaviour, outside access) of this image
   */
  override def withAccessMode(accessMode: AccessMode[A]): PixelImage[A] =
    new ArrayImage[A](domain, accessMode, data)

  /**
   * apply a function to each pixel
   */
  override def map[B](f: A => B)(implicit tag: ClassTag[B]): PixelImage[B] = {
    val newData = data.map(f)
    new ArrayImage[B](domain, accessMode = accessMode.map(f), data = newData)
  }

  // TODO: Test this.
  override def parMap[F[_]: Parallel: Async, B](f: A => B)(implicit tag: ClassTag[B]): F[PixelImage[B]] = {
    def handleAssignment(newArray: Array[B], index: Int): F[Unit] = {
      for {
        index  <- Async[F].pure(index)
        atIndex = this(domain.x(index), domain.y(index))
        _      <- Async[F].delay(newArray(index) = f(atIndex))
      } yield ()
    }

    for {
      newArray <- Async[F].delay(new Array[B](this.data.length))
      _        <- Parallel.parTraverse(data.indices.asInstanceOf[Seq[Int]])(handleAssignment(newArray, _))
    } yield new ArrayImage[B](domain, accessMode.map(f), newArray)
  }

  /**
   * get all values as data Array (linearization strategy depends on domain, creates a copy)
   */
  override def toArray(implicit tag: ClassTag[A]): Array[A] = data.clone()

  override def toString: String = "ArrayImage(" + domain + ")"

  override val isBuffered = true

  // correct equality with respect to underlying data array
  override def equals(other: Any): Boolean = other match {
    case ai: ArrayImage[A] => domain == ai.domain && data.sameElements(ai.data)
    case pi: PixelImage[A] => super.equals(pi)
    case _                 => false
  }

  // correct hashCode with respect to underlying data array
  override def hashCode: Int = domain.hashCode() * 43 + data.hashCode()
}
