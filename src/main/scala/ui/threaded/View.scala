package ui.threaded

import java.awt.{AlphaComposite, Dimension, Graphics, Graphics2D}
import java.util.{Observable, Observer}

import javax.swing.JPanel


object View {
  private val WIDE = 600
  private val HIGH = WIDE * Model.ASPH / Model.ASPW
}

class View(var model: ImageModel) extends JPanel with Observer {
  this.setPreferredSize(new Dimension(View.WIDE, View.HIGH))
  model.addObserver(this)

  override protected def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)
    g.asInstanceOf[Graphics2D].setComposite(AlphaComposite.Src)
    g.drawImage(model.getImage, 0, 0, this.getWidth, this.getHeight, null)
  }

  override def update(o: Observable, arg: Any): Unit = {
    this.repaint()
  }
}
