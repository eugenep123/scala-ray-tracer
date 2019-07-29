//package ui.simple
//
//import java.awt._
//import java.util.Formatter
//
//import javax.swing._
//
///**
//  * One ball bouncing inside a rectangular box.
//  * All codes in one file. Poor design!
//  */
//// Extends JPanel, so as to override the paintComponent() for custom rendering codes.
//object BouncingBallSimple { // Container box's width and height
//  private val BOX_WIDTH = 640
//  private val BOX_HEIGHT = 480
//  private val UPDATE_RATE = 30 // Number of refresh per second
//
//  /** main program (entry point) */
//  def main(args: Array[String]): Unit = { // Run GUI in the Event Dispatcher Thread (EDT) instead of main thread.
//    javax.swing.SwingUtilities.invokeLater(new Runnable() {
//      override def run(): Unit = { // Set up main window (using Swing's Jframe)
//        val frame = new JFrame("A Bouncing Ball")
//        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
//        frame.setContentPane(new BouncingBallSimple)
//        frame.pack()
//        frame.setVisible(true)
//      }
//    })
//  }
//}
//
//class BouncingBallSimple() extends JPanel {
//  this.setPreferredSize(new Dimension(BouncingBallSimple.BOX_WIDTH, BouncingBallSimple.BOX_HEIGHT))
//  // Start the ball bouncing (in its own thread)
//  val gameThread: Thread = new Thread() {
//    override def run(): Unit = {
//      while ( {
//        true
//      }) { // Execute one update step
//        // Calculate the ball's new position
//        ballX += ballSpeedX
//        ballY += ballSpeedY
//        // Check if the ball moves over the bounds
//        // If so, adjust the position and speed.
//        if (ballX - ballRadius < 0) {
//          ballSpeedX = -ballSpeedX // Reflect along normal
//
//          ballX = ballRadius // Re-position the ball at the edge
//
//        }
//        else if (ballX + ballRadius > BouncingBallSimple.BOX_WIDTH) {
//          ballSpeedX = -ballSpeedX
//          ballX = BouncingBallSimple.BOX_WIDTH - ballRadius
//        }
//        // May cross both x and y bounds
//        if (ballY - ballRadius < 0) {
//          ballSpeedY = -ballSpeedY
//          ballY = ballRadius
//        }
//        else if (ballY + ballRadius > BouncingBallSimple.BOX_HEIGHT) {
//          ballSpeedY = -ballSpeedY
//          ballY = BouncingBallSimple.BOX_HEIGHT - ballRadius
//        }
//        // Refresh the display
//        repaint() // Callback paintComponent()
//
//        // Delay for timing control and give other threads a chance
//        try
//          Thread.sleep(1000 / BouncingBallSimple.UPDATE_RATE) // milliseconds
//
//        catch {
//          case ex: InterruptedException =>
//
//        }
//      }
//    }
//  }
//  gameThread.start() // Callback run()
//
//  // Ball's properties
//  private val ballRadius = 200 // Ball's radius
//
//  private var ballX = ballRadius + 50 // Ball's center (x, y)
//
//  private var ballY = ballRadius + 20
//  private var ballSpeedX = 3 // Ball's speed for x and y
//
//  private var ballSpeedY = 2
//
//  /** Custom rendering codes for drawing the JPanel */
//  override def paintComponent(g: Graphics): Unit = {
//    super.paintComponent(g) // Paint background
//
//    // Draw the box
//    g.setColor(Color.BLACK)
//    g.fillRect(0, 0, BouncingBallSimple.BOX_WIDTH, BouncingBallSimple.BOX_HEIGHT)
//    // Draw the ball
//    g.setColor(Color.BLUE)
//    g.fillOval((ballX - ballRadius).toInt, (ballY - ballRadius).toInt, (2 * ballRadius).toInt, (2 * ballRadius).toInt)
//    // Display the ball's information
//    g.setColor(Color.WHITE)
//    g.setFont(new Font("Courier New", Font.PLAIN, 12))
//    val sb = new StringBuilder
//    val formatter = new Formatter(sb.mkString)
//    formatter.format("Ball @(%3.0f,%3.0f) Speed=(%2.0f,%2.0f)", ballX, ballY, ballSpeedX, ballSpeedY)
//    g.drawString(sb.toString, 20, 30)
//  }
//}