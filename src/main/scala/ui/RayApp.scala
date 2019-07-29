package ui

import javax.swing._

object RayApp extends App {
  try {
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())
  } catch {
    case _: Exception => println("Cannot set look and feel, using the default one.")
  }

  val frame = new RayTracerFrame(800, 600)
  frame.repaint()
}
