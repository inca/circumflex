package ru.circumflex.me

import javax.swing._
import java.awt._
import javax.swing.event._
import java.awt.event._
import ru.circumflex._, core._

object GUITesterMain extends Application {
  new GUITester().setVisible(true)
}

class GUITester extends JFrame {
  val showHtml = new JCheckBox
  val metrics = new JLabel
  val src = new JEditorPane
  val out = new JEditorPane
  src.setFont(new Font("Monospaced", Font.PLAIN, 12))
  src.getDocument.addDocumentListener(new DocumentListener() {
    def process(e: DocumentEvent) = {
      val r = time(new MarkevenProcessor().toHtml(src.getText))
      val s = r._2
      if (showHtml.isSelected) {
        out.setText("<html><body>" + s + "</body></html>")
        out.setContentType("text/html")
      } else {
        out.setText(s)
        out.setContentType("text/plain")
      }
      metrics.setText("Input " + src.getText.length + " chars, " +
          "output " + out.getText.length + " chars, took " + r._1 + "ms.")
    }
    def changedUpdate(e: DocumentEvent): Unit = process(e)
    def removeUpdate(e: DocumentEvent): Unit = process(e)
    def insertUpdate(e: DocumentEvent): Unit = process(e)
  })
  showHtml.setText("Show HTML")
  showHtml.addActionListener(new ActionListener {
    def actionPerformed(e: ActionEvent): Unit = src.setText(src.getText)
  })
  out.setEditable(false)
  // layout stuff
  val srcPane = new JPanel
  srcPane.setLayout(new BorderLayout)
  srcPane.add(new JScrollPane(src))
  srcPane.setMinimumSize(new Dimension(200, 600))
  val outPane = new JPanel
  outPane.setLayout(new BorderLayout)
  outPane.add(showHtml, BorderLayout.NORTH)
  outPane.add(new JScrollPane(out), BorderLayout.CENTER)
  outPane.add(metrics, BorderLayout.SOUTH)
  outPane.setMinimumSize(new Dimension(200, 600))
  val splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, srcPane, outPane);
  splitPane.setDividerLocation(400)
  splitPane.setOneTouchExpandable(true)
  setSize(800, 600)
  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  add(splitPane)
}