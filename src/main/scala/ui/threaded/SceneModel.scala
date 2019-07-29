package ui.threaded

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.image.BufferedImage
import java.awt.{AlphaComposite, Color}
import java.util.Random

import javax.swing.Timer
import raytracer.Color.normalize
import raytracer.Scene


class SceneModel(scene: Scene) extends ImageModel
  with ActionListener with Runnable {
  final private val timer = new Timer(1000 / Model.RATE, this)
  final private val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
  final private val g2d = image.createGraphics
  final private val raster = image.getRaster
  private val random = new Random
  final private val colorParts = new Array[Int](4)

  def width = scene.camera.width
  def height = scene.camera.height

  override def run(): Unit = {
    reset()
    println("Start render...")
    scene.camera.render(scene.world) { (x, y, color) =>

      val r = normalize(color.red)
      val g = normalize(color.green)
      val b = normalize(color.blue)
      val a = 255
      val ca = Array(r, g, b, a)
      raster.setPixel(x, y, ca)
      Thread.`yield`()
    }
  }

  def getImage: BufferedImage = image

  def getTimer: Timer = timer

  def setFill(fill: Boolean): Unit = {
//    this.fill = fill
//    reset()
  }

  def reset(): Unit = {
    timer.stop()
    g2d.setComposite(AlphaComposite.Clear)
    g2d.fillRect(0, 0, width, height)
    g2d.setComposite(AlphaComposite.Src)
    val c = Color.black
    val fa = c.getRGBComponents(null)
    g2d.setPaint(new Color(fa(0), fa(1), fa(2), 0.75f))
    g2d.fillRect(0, 0, width, height)
    timer.start()
  }

  override def actionPerformed(e: ActionEvent): Unit = {
    if (e.getSource eq timer) {
      this.setChanged()
      this.notifyObservers()
    }
  }
}