package nicoburniske.faxion.image.lib

import java.awt.Color

case class RGBA(r: Double, g: Double, b: Double, a: Double) {

  def isInBounds: Boolean =
    r >= 0.0 && r <= 1.0 && g >= 0.0 && g <= 1.0 && b >= 0.0 && b <= 1.0 && a >= 0.0 && a <= 1.0

  /**
   * clamp all values to valid range [0, 1]
   */
  def clamped: RGBA = {
    def clamp(f: Double): Double = math.min(1.0, math.max(0.0, f))
    if (!isInBounds)
      RGBA(clamp(r), clamp(g), clamp(b), clamp(a))
    else
      this
  }

  /**
   * average intensity value
   */
  def gray: Double = (r + g + b) / 3.0

  /**
   * applies f to all channels
   */
  def map(f: Double => Double): RGBA = new RGBA(f(r), f(g), f(b), f(a))

  /**
   * addition
   */
  def +(other: RGBA): RGBA = RGBA(r + other.r, g + other.g, b + other.b, a + other.a)

  /**
   * subtraction
   */
  def -(other: RGBA): RGBA = RGBA(r - other.r, g - other.g, b - other.b, a - other.a)

  /**
   * scaling
   */
  def *(f: Double): RGBA = RGBA(r * f, g * f, b * f, a * f)

  /**
   * scaling with scalar number
   */
  def *:(f: Double): RGBA = new RGBA(r * f, g * f, b * f, a * f)

  /**
   * scaling
   */
  def /(f: Double): RGBA = this * (1.0 / f)

  /**
   * dot product
   */
  def dot(other: RGBA): Double = r * other.r + g * other.g + b * other.b + a * other.a

  /**
   * component-wise multiplication
   */
  def x(other: RGBA): RGBA = RGBA(r * other.r, g * other.g, b * other.b, a * other.a)

  /**
   * component-wise division
   */
  def /(other: RGBA): RGBA = RGBA(r / other.r, g / other.g, b / other.b, a / other.a)

  /**
   * convert to RGB
   */
  def toRGB: RGB = RGB(r, g, b)

  /**
   * convert to Tuple
   */
  def toTuple: (Double, Double, Double, Double) = (r, g, b, a)

  /**
   * apply an operation to RGB part, keep alpha channel
   */
  def mapRGB(f: RGB => RGB): RGBA = RGBA(f(toRGB), a)

  /**
   * remove transparency
   */
  def noAlpha: RGBA = copy(a = 1.0)

  /**
   * treat non-opaque pixels as not available
   */
  def toOptionRGB: Option[RGB] = if (a > 0.99) Some(toRGB) else None

  /**
   * blending of two colors: this over that (see "over" in alpha blending)
   */
  def over(that: RGBA): RGBA = {
    val ma = 1.0 - a
    val ao = a + that.a * ma
    RGBA(r * a + that.r * that.a * ma, g * a + that.g * that.a * ma, b * a + that.b * that.a * ma, ao)
  }

  /**
   * convert to AWT default color expects a clamped color value
   */
  def toAWTColor: Color = new java.awt.Color(r.toFloat, g.toFloat, b.toFloat, a.toFloat)
}

object RGBA {

  val White: RGBA = RGBA(1.0, 1.0, 1.0, 1.0)
  val Black: RGBA = RGBA(0.0, 0.0, 0.0, 1.0)

  val WhiteTransparent: RGBA = RGBA(1.0, 1.0, 1.0, 0.0)
  val BlackTransparent: RGBA = RGBA(0.0, 0.0, 0.0, 0.0)

  def apply(color: RGB): RGBA                        = new RGBA(color.r, color.g, color.b, 1.0)
  def apply(color: RGB, a: Double): RGBA             = new RGBA(color.r, color.g, color.b, a)
  def apply(r: Double, g: Double, b: Double): RGBA   = new RGBA(r, g, b, 1.0)
  def apply(gray: Double): RGBA                      = new RGBA(gray, gray, gray, 1.0)
  def apply(gray: Double, a: Double): RGBA           = new RGBA(gray, gray, gray, a)
  def apply(tuple: (Double, Double, Double, Double)) = new RGBA(tuple._1, tuple._2, tuple._3, tuple._4)
  def apply(awtColor: Color): RGBA                   =
    RGBA(
      fromInt8(awtColor.getRed),
      fromInt8(awtColor.getGreen),
      fromInt8(awtColor.getBlue),
      fromInt8(awtColor.getAlpha))

  implicit object RGBAOperations extends ColorSpaceOperations[RGBA] {

    /**
     * add two pixels
     */
    override def add(pix1: RGBA, pix2: RGBA): RGBA = pix1 + pix2

    /**
     * scalar multiplication
     */
    override def scale(pix: RGBA, l: Double): RGBA = pix * l

    /**
     * dot product
     */
    override def dot(pix1: RGBA, pix2: RGBA): Double = pix1 dot pix2

    /**
     * channel-wise multiplication
     */
    override def multiply(pix1: RGBA, pix2: RGBA): RGBA = pix1 x pix2

    /**
     * zero element
     */
    override def zero: RGBA = BlackTransparent

    override val dimensionality = 4
  }

  // private def toInt8(value: Double): Int = (value * 255.0).toInt

  private def fromInt8(intValue: Int): Double = intValue / 255.0
}
