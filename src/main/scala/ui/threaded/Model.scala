package ui.threaded

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.image.BufferedImage
import java.awt.{AlphaComposite, Color}
import java.util.{Observable, Random}

import javax.swing.Timer


/**
  * A stochastic model that simulates the accumulation of image data.
  *
  * The model runs on a separate thread. A javax.swing.Timer is used for interim
  * updates so that the actionPerformed() method executes on the
  * event-dispatching thread. As this relies on the Observable's call to
  * update(), avoid calling notifyObservers() from any other thread. Note also
  * that image is final [JLS 17.5].
  */
object Model {
  val RATE = 25 // 25 Hz

  val ASPW = 5 // Aspect ratio width

  val ASPH = 3 // Aspect ratio height

  private val WIDE = 256
  private val HIGH = WIDE * ASPH / ASPW
  private val random = new Random
  private val clut = new Array[Color](16)

  var i = 0
  while ( {
    i < clut.length
  }) {
    val v = i * 16
    clut(i) = new Color(v, v, v)

    {
      i += 1; i - 1
    }
  }

}

class Model extends ImageModel with ActionListener with Runnable {
//  extends Observable with ActionListener with Runnable{
  final private val timer = new Timer(1000 / Model.RATE, this)
  final private val image = new BufferedImage(Model.WIDE, Model.HIGH, BufferedImage.TYPE_INT_ARGB)
  final private val g2d = image.createGraphics
  final private val raster = image.getRaster
  final private val ia = new Array[Int](4)
  private var fill = false
  private var index = 0

  override def run(): Unit = {
    reset()
    while ( {
      true
    }) {
      next()
      Thread.`yield`()
    }
  }

  private def next(): Unit = {
    if (fill) {
      g2d.setPaint(Model.clut({
        index += 1; index - 1
      }))
      if (index == Model.clut.length) index = 0
      g2d.fillRect(0, 0, Model.WIDE, Model.HIGH)
    }
    else {
      val dx = Math.abs(Model.random.nextGaussian)
      val dy = Math.abs(Model.random.nextGaussian)
      val x = (dx * Model.WIDE / 2.123d).toInt
      val y = (dy * Model.HIGH / 2.123d).toInt
      if (x < Model.WIDE && y < Model.HIGH) {
        raster.getPixel(x, y, ia)
        adjust(0)
        adjust(1)
        adjust(2)
        adjust(3)
        raster.setPixel(x, y, ia)
      }
    }
  }

  private def adjust(i: Int): Unit = {
    ia(i) += 1
    if (ia(i) > 255) ia(i) = 128
  }

  def getImage: BufferedImage = image

  def getTimer: Timer = timer

  def setFill(fill: Boolean): Unit = {
    this.fill = fill
    reset()
  }

  def reset(): Unit = {
    timer.stop()
    g2d.setComposite(AlphaComposite.Clear)
    g2d.fillRect(0, 0, Model.WIDE, Model.HIGH)
    g2d.setComposite(AlphaComposite.Src)
    val c = Color.getHSBColor(Model.random.nextFloat, 0.9f, 0.9f)
    val fa = c.getRGBComponents(null)
    g2d.setPaint(new Color(fa(0), fa(1), fa(2), 0.75f))
    g2d.fillRect(0, 0, Model.WIDE, Model.HIGH)
    timer.start()
  }

  override def actionPerformed(e: ActionEvent): Unit = {
    if (e.getSource eq timer) {
      this.setChanged()
      this.notifyObservers()
    }
  }
}
