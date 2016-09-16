
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

    /** Restricts the integer into the specified range. */
    def clamp(v: Int, min: Int, max: Int): Int = {
      if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    // TODO implement using while loops
    var average_red:RGBA=0
    var average_green:RGBA=0
    var average_blue:RGBA=0
    var average_alpha:RGBA=0
    var invalid:Int=0
    var x_cor:Int=x-radius
    var y_cor:Int=y-radius
    while (y_cor<=y+radius)
      {
        while (x_cor<=x+radius){
          if (((0<=x_cor)&&(x_cor<src.width))&&((0<=y_cor)&&(y_cor<src.height))) {
            val pixel_value = src.apply(x_cor, y_cor)
            average_red+=clamp(red(pixel_value), 0, 255)
            average_green+=clamp(green(pixel_value), 0, 255)
            average_blue+=clamp(blue(pixel_value), 0, 255)
            average_alpha+=clamp(alpha(pixel_value), 0, 255)

          }else
            invalid+=1
          x_cor += 1
        }
        x_cor=x-radius
        y_cor+=1
      }

    average_red/=((2*radius+1)*(2*radius+1)-invalid)
    average_green/=((2*radius+1)*(2*radius+1)-invalid)
    average_blue/=((2*radius+1)*(2*radius+1)-invalid)
    average_alpha/=((2*radius+1)*(2*radius+1)-invalid)
    rgba(average_red,average_green,average_blue,average_alpha)
  }

}
