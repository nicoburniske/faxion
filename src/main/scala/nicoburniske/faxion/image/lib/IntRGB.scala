package nicoburniske.faxion.image.lib

import java.awt.Color

case class IntRGB(r: Int, g: Int, b: Int) {

  def isChannelInBounds(channel: Int): Boolean = channel >= 0 && channel <= 255
  def isInBounds: Boolean                      = isChannelInBounds(r) && isChannelInBounds(g) && isChannelInBounds(b)
  def sum: Int                                 = r + g + b

  def gray: Int      = sum / 3
  def luminance: Int = ((0.3 * r) + (0.59 * g) + (0.11 * b)).toInt

  /**
   * Clamp pixel so each channel is within range 0 to 255 (inclusive)
   */
  def clamp(): IntRGB = {
    def clampChannel(channel: Int): Int = if (channel > 255) 255 else if (channel < 0) 0 else channel
    IntRGB(clampChannel(r), clampChannel(g), clampChannel(b))
  }

  /**
   * Addition with another IntegerRGB
   */
  def +(other: IntRGB): IntRGB = IntRGB(r + other.r, g + other.g, b + other.b)

  /**
   * Subtracting another IntegerRGB
   */
  def -(other: IntRGB): IntRGB = IntRGB(r - other.r, g - other.g, b - other.b)

  /**
   * Scaling with a scalar
   */
  def *(f: Double): IntRGB = IntRGB((r * f).toInt, (g * f).toInt, (b * f).toInt)

  /**
   * Scaling with a scalar
   */
  def *:(f: Double): IntRGB = IntRGB((r * f).toInt, (g * f).toInt, (b * f).toInt)

  /**
   * Inverse scaling with a scalar
   */
  def /(f: Double): IntRGB = this * (1 / f)

  /**
   * Dot product with another IntegerRGB
   */
  def dot(other: IntRGB): Int = r * other.r + g * other.g + b * other.b

  /**
   * Component-wise multiplication with another IntegerRGB
   */
  def x(other: IntRGB): IntRGB = IntRGB(r * other.r, g * other.g, b * other.b)

  /**
   * Component-wise division by another IntegerRGB
   */
  def /(other: IntRGB): IntRGB =
    IntRGB(r / other.r, g / other.g, b / other.b)

  /**
   * Applies f to all channels
   */
  def map(f: Int => Int): IntRGB = IntRGB(f(r), f(g), f(b))

  /**
   * Convert to a Double RGB with values 0 <= i <= 1
   */
  def toRGB: RGB = RGB(r / 255.0, g / 255.0, b / 255.0)

  def toTuple: (Int, Int, Int) = (r, g, b)

  /**
   * Convert to AWT default color, expects a clamped color value
   */
  def toAWTColor: Color = new Color(clamp().r, clamp().g, clamp().b)

}

object IntRGB {

  val White: IntRGB = IntRGB(255, 255, 255)
  val Black: IntRGB = IntRGB(0, 0, 0)

  def apply(color: IntRGB): IntRGB          = new IntRGB(color.r, color.g, color.b)
  def apply(color: RGB): IntRGB             =
    new IntRGB((color.r * 255).toInt, (color.g * 255).toInt, (color.b * 255).toInt)
  def apply(gray: Int): IntRGB              = new IntRGB(gray, gray, gray)
  def apply(tuple: (Int, Int, Int)): IntRGB = new IntRGB(tuple._1, tuple._2, tuple._3)
  def apply(awtColor: Color): IntRGB        =
    new IntRGB(awtColor.getRed, awtColor.getGreen, awtColor.getBlue)

  implicit object RGBOperations extends ColorSpaceOperations[IntRGB] {

    /**
     * Add to pixel values
     */
    override def add(p1: IntRGB, p2: IntRGB): IntRGB = p1 + p2

    /**
     * Scalar multiplication
     */
    override def scale(p: IntRGB, l: Double): IntRGB = p * l

    /**
     * Dot product TODO: Correct this to return an Int instead of a Double
     */
    override def dot(p1: IntRGB, p2: IntRGB): Double = (p1 dot p2).toDouble

    /**
     * Channel-wise multiplication
     */
    override def multiply(p1: IntRGB, p2: IntRGB): IntRGB = p1 x p2

    /**
     * Zero element
     */
    override def zero: IntRGB = Black

    override val dimensionality = 3

  }

}
