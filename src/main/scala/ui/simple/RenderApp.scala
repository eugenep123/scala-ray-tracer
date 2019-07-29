//package ui.simple
//
//import java.util.concurrent.BlockingQueue
//
//import javax.swing.JFrame
//
//object RenderApp extends App {
//
//
//  def showUi(queue: BlockingQueue[RenderCommand]) {
//    javax.swing.SwingUtilities.invokeLater(new Runnable() {
//      override def run(): Unit = { // Set up main window (using Swing's Jframe)
//        val frame = new JFrame("Scala Ray Tracer")
//        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
//        frame.setContentPane(new RenderPanel(queue))
//        frame.pack()
//        frame.setVisible(true)
//      }
//    })
//  }
//
//
//  // Load scene
//
//
//
//}
