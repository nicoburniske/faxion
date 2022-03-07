package nicoburniske.faxion.image.lib

import java.awt.Color

case class RGB(r: Double, g: Double, b: Double) {
  def isInBounds: Boolean = r >= 0.0 && r <= 1.0 && g >= 0.0 && g <= 1.0 && b >= 0.0 && b <= 1.0

  lazy val toIntRGB: IntRGB = IntRGB(this)

  def sum: Double = r + g + b

  /**
   * clamp all values to valid range [0, 1]
   */
  def clamped: RGB = {
    def clamp(f: Double): Double = math.min(1.0, math.max(0.0, f))
    if (!isInBounds)
      RGB(clamp(r), clamp(g), clamp(b))
    else
      this
  }

  /**
   * average intensity value
   */
  def gray: Double = sum / 3.0

  /**
   * l2 norm of color rgb values
   */
  def norm: Double = math.sqrt(math.pow(r, 2) + math.pow(g, 2) + math.pow(b, 2))

  def luminance: Double = 0.3 * r + 0.59 * g + 0.11 * b

  /**
   * addition of another RGB
   */
  def +(other: RGB): RGB = new RGB(r + other.r, g + other.g, b + other.b)

  /**
   * subtraction of another RGB
   */
  def -(other: RGB): RGB = new RGB(r - other.r, g - other.g, b - other.b)

  /**
   * scaling with scalar number
   */
  def *(f: Double): RGB = new RGB(r * f, g * f, b * f)

  /**
   * scaling with scalar number
   */
  def *:(f: Double): RGB = new RGB(r * f, g * f, b * f)

  /**
   * scaling with scalar number
   */
  def /(f: Double): RGB = this * (1.0f / f)

  /**
   * dot product
   */
  def dot(other: RGB): Double = r * other.r + g * other.g + b * other.b

  /**
   * component-wise multiplication
   */
  def x(other: RGB): RGB = RGB(r * other.r, g * other.g, b * other.b)

  /**
   * component-wise division
   */
  def /(other: RGB): RGB = RGB(r / other.r, g / other.g, b / other.b)

  /**
   * applies f to all channels
   */
  def map(f: Double => Double): RGB = new RGB(f(r), f(g), f(b))

  /**
   * blend with RGBA: RGBA pixel overlay, this color as basis
   */
  def blend(color: RGBA): RGB = {
    val a  = color.a
    val na = 1.0f - color.a
    new RGB(na * r + a * color.r, na * g + a * color.g, na * b + a * color.b)
  }

  /**
   * convert to RGBA with full opacity
   */
  def toRGBA: RGBA = RGBA(r, g, b, 1.0)

  /**
   * convert to Tuple
   */
  def toTuple: (Double, Double, Double) = (r, g, b)

  /**
   * convert to AWT default color expects a clamped color value
   */
  def toAWTColor: Color = new java.awt.Color(r.toFloat, g.toFloat, b.toFloat)
}

object RGB {

  val White: RGB = RGB(1.0, 1.0, 1.0)
  val Black: RGB = RGB(0.0, 0.0, 0.0)

  def apply(color: RGBA): RGB                = new RGB(color.r, color.g, color.b)
  def apply(gray: Double): RGB               = new RGB(gray, gray, gray)
  def apply(tuple: (Double, Double, Double)) = new RGB(tuple._1, tuple._2, tuple._3)
  def apply(awtColor: Color)                 =
    new RGB(fromInt8(awtColor.getRed), fromInt8(awtColor.getGreen), fromInt8(awtColor.getBlue))

  implicit object RGBOperations extends ColorSpaceOperations[RGB] {

    /**
     * add two pixels
     */
    override def add(pix1: RGB, pix2: RGB): RGB = pix1 + pix2

    /**
     * scalar multiplication
     */
    override def scale(pix: RGB, l: Double): RGB = pix * l

    /**
     * dot product
     */
    override def dot(pix1: RGB, pix2: RGB): Double = pix1.dot(pix2)

    /**
     * channel-wise multiplication
     */
    override def multiply(pix1: RGB, pix2: RGB): RGB = pix1 x pix2

    /**
     * zero element
     */
    override def zero: RGB = Black

    override val dimensionality = 3
  }

  // private def toInt8(value: Double): Int = (value * 255.0).toInt

  private def fromInt8(intValue: Int): Double = intValue / 255.0
}
