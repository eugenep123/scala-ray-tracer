package ui

import java.awt.image.BufferedImage
import java.awt.{Dimension, Graphics}

import chapters.Chapter9
import javax.swing.JComponent
import raytracer.{Canvas, Color}

class CanvasComponent(width: Int, height: Int) extends JComponent {
  var canvas = Canvas(width, height)

  override def getPreferredSize = {
    new Dimension(canvas.width, canvas.height)
  }

  override def paintComponent(gcan: Graphics) = {
    super.paintComponent(gcan)
    val bufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    canvas.foreach { (x, y, color) =>
      bufferedImage.setRGB(x, y, color.toRGBA(1.0))
    }
    gcan.drawImage(bufferedImage, 0, 0, null)
  }

  def clear(): Unit = {
    canvas.fill(Color.Black)
    repaint()
  }

  def render(): Unit = {
    canvas.fill(Color.Black)
    Chapter9.render(canvas)
    repaint()
  }
}

