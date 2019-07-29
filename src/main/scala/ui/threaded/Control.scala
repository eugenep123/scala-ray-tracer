package ui.threaded

import java.awt.event.{ActionEvent, ActionListener, ItemEvent, ItemListener}
import java.awt.{BorderLayout, Component, EventQueue}

import javax.swing._
import javax.swing.event.{ChangeEvent, ChangeListener}

object Control {
  private val RESET = "Reset"

  def create(model: ImageModel): Unit = {
    val f = new JFrame("Thread Watch")
    f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

    val view = new View(model)
    val control = new Control(model)
    val panel = new JPanel(new BorderLayout)
    panel.add(view, BorderLayout.CENTER)
    panel.add(control, BorderLayout.SOUTH)
    f.add(panel)
    f.pack()
    f.setLocationRelativeTo(null)
    f.setVisible(true)
  }

  def start(model: ImageModel): Unit = {
    EventQueue.invokeLater(() => {
      create(model)
    })
  }
}

class Control(var model: ImageModel) extends JPanel with ActionListener with ChangeListener with ItemListener {

  final private val fillCheck = new JCheckBox("Fill")
  val label = new JLabel("Rate (Hz):")
  val speed = new JSpinner(new SpinnerNumberModel(Model.RATE, 1, 50, 1))

  this.addButton(Control.RESET)
  this.add(fillCheck)
  fillCheck.addItemListener(this)
  this.add(label)
  speed.setEditor(new JSpinner.NumberEditor(speed, "0"))
  speed.setMaximumSize(speed.getPreferredSize)
  speed.addChangeListener(this)
  this.add(speed)

  private def addButton(name: String): Unit = {
    val button = new JButton(name)
    button.setAlignmentX(Component.CENTER_ALIGNMENT)
    add(button)
    button.addActionListener(this)
  }

  override def actionPerformed(e: ActionEvent): Unit = {
    val cmd = e.getActionCommand
    if (Control.RESET == cmd) model.reset()
  }

  override def stateChanged(e: ChangeEvent): Unit = {
    val spinner = e.getSource.asInstanceOf[JSpinner]
    val value = spinner.getValue.asInstanceOf[Number]
    model.getTimer.setDelay(1000 / value.intValue)
  }

  override def itemStateChanged(e: ItemEvent): Unit = {
    val source = e.getItemSelectable
    val state = e.getStateChange == ItemEvent.SELECTED
    if (source eq fillCheck) model.setFill(state)
  }
}