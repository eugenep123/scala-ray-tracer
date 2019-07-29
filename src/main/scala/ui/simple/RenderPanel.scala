//package ui.simple
//
//import java.awt._
//import java.util.Formatter
//
//import javax.swing._
//import java.util.concurrent.{BlockingDeque, BlockingQueue}
//
//import javax.swing.JPanel
//
//class RenderPanel(commandQueue: BlockingQueue[RenderCommand]) extends JPanel {
//  this.setPreferredSize(new Dimension(RenderPanel.DefaultWidth, RenderPanel.DefaultHeight))
//
//
//  val gameThread: Thread = new Thread() {
//    override def run(): Unit = {
//      var running = true
//
//      try {
//        while (!Thread.currentThread.isInterrupted) {
//          val sample = commandQueue.poll()
//          consume(sample)
//
//        }
//      } catch {
//        case ie: InterruptedException =>
//          println("Consumer stopped with interrupt")
//      }
//    }
//  }
//
//}
//
//
//object RenderPanel {
//  val DefaultWidth = 800
//  val DefaultHeight = 800
//}