package ui

import java.awt._
import java.awt.event._

import javax.swing._
import org.scalameter._


class RayTracerFrame(width: Int, height: Int) extends JFrame("Ray Tracer\u2122") {
  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  setSize(1024, 600)
  setLayout(new BorderLayout)

  val rightpanel = new JPanel
  rightpanel.setBorder(BorderFactory.createEtchedBorder(border.EtchedBorder.LOWERED))
  rightpanel.setLayout(new BorderLayout)
  add(rightpanel, BorderLayout.EAST)

  val controls = new JPanel
  controls.setLayout(new GridLayout(0, 2))
  rightpanel.add(controls, BorderLayout.NORTH)


  val stepbutton = new JButton("Render")
  stepbutton.addActionListener(new ActionListener {
    def actionPerformed(e: ActionEvent) {
      val time = measure {
        canvas.render()
      }
      updateInformationBox(time.value)
    }
  })
  controls.add(stepbutton)

  val clearButton = new JButton("Clear")
  clearButton.addActionListener(new ActionListener {
    def actionPerformed(e: ActionEvent) {
      canvas.clear()
    }
  })
  controls.add(clearButton)

  val info = new JTextArea("   ")
  info.setBorder(BorderFactory.createLoweredBevelBorder)
  rightpanel.add(info, BorderLayout.SOUTH)

  val mainMenuBar = new JMenuBar()

  val fileMenu = new JMenu("File")
  val exitMenuItem = new JMenuItem("Exit")
  exitMenuItem.addActionListener(new ActionListener {
    def actionPerformed(e: ActionEvent) {
      sys.exit(0)
    }
  })
  fileMenu.add(exitMenuItem)
  mainMenuBar.add(fileMenu)
  setJMenuBar(mainMenuBar)

  val canvas = new CanvasComponent(width, height)
  val scrollPane = new JScrollPane(canvas)
  add(scrollPane, BorderLayout.CENTER)
  setVisible(true)

  def updateInformationBox(time: Double) {
    info.setText(s"Time: $time")
  }


}