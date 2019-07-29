package ui.threaded

import java.awt.image.BufferedImage
import java.util.Observable

import javax.swing.Timer


abstract class ImageModel extends Observable {
  def getImage: BufferedImage
  def getTimer: Timer
  def reset(): Unit
  def setFill(fill: Boolean): Unit


}


