package gui

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{BorderLayout, Color, GridLayout}
import javax.swing._
import javax.swing.border.EtchedBorder
import javax.swing.table.AbstractTableModel

import buparsing.BottomUpParsing
import grammar._
import llone.first.First
import llone.follow.Follow
import tdparsing.TopDownParsing

/**
  * Created by Dandoh on 10/10/16.
  */
object Parser {


  class ParserFrame extends JFrame("Chapter 3 : Parsing - Dandoh\u2122") {
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    setSize(960, 700)
    setResizable(false)
    setLayout(new BorderLayout)

    val rightpanel = new JPanel
    rightpanel.setBorder(BorderFactory.createEtchedBorder(border.EtchedBorder.LOWERED))
    rightpanel.setLayout(new BorderLayout)
    add(rightpanel, BorderLayout.WEST)

    val allControls = new JPanel
    allControls.setLayout(new BoxLayout(allControls, BoxLayout.Y_AXIS))
    rightpanel.add(allControls, BorderLayout.NORTH)


    // Initial selection
    val initSelectionControls = new JPanel
    initSelectionControls.setLayout(new GridLayout(0, 1))
    allControls.add(initSelectionControls)

    val initialSelectionGroup = new ButtonGroup()

    val initSelectionLabel = new JLabel("Parsing algorithm:")
    initSelectionControls.add(initSelectionLabel)

    val computeMode = new ActionListener {
      def actionPerformed(e: ActionEvent): Unit = {
        inputPanel.setVisible(false)
        remove(outputPanel)
        add(tablePanel, BorderLayout.CENTER)
        revalidate()
      }
    }

    val parseMode = new ActionListener {
      def actionPerformed(e: ActionEvent): Unit = {
        inputPanel.setVisible(true)
        remove(tablePanel)
        add(outputPanel, BorderLayout.CENTER)
        revalidate()
      }
    }


    val topDownParsingRadioButton = new JRadioButton("Top-down parsing")
    initSelectionControls.add(topDownParsingRadioButton)
    topDownParsingRadioButton.addActionListener(parseMode)

    val bottomUpParsingRadioButton = new JRadioButton("Bottom-up parsing")
    initSelectionControls.add(bottomUpParsingRadioButton)
    bottomUpParsingRadioButton.addActionListener(parseMode)

    val computeFirstRadioButton = new JRadioButton("Compute FIRST sets")
    initSelectionControls.add(computeFirstRadioButton)
    computeFirstRadioButton.addActionListener(computeMode)

    val computeFollowRadioButton = new JRadioButton("Compute FOLLOW sets")
    initSelectionControls.add(computeFollowRadioButton)
    computeFollowRadioButton.addActionListener(computeMode)

    initialSelectionGroup.add(bottomUpParsingRadioButton)
    initialSelectionGroup.add(topDownParsingRadioButton)
    initialSelectionGroup.add(computeFirstRadioButton)
    initialSelectionGroup.add(computeFollowRadioButton)

    topDownParsingRadioButton.setSelected(true)


    val grammarPanel = new JPanel
    grammarPanel.setLayout(new GridLayout(0, 1))
    allControls.add(grammarPanel)
    grammarPanel.add(new JLabel("Non-terminal (separated by space):"))
    val nonTerminalField = new JTextField()
    grammarPanel.add(nonTerminalField)
    grammarPanel.add(new JLabel("Terminal (separated by space): "))
    val terminalField = new JTextField()
    grammarPanel.add(terminalField)
    grammarPanel.add(new JLabel("Start symbol: "))
    val startSymbolField = new JTextField()
    grammarPanel.add(startSymbolField)
    grammarPanel.add(new JLabel("<html>Products: (Line by line e.g \"S -> a S\", <br> \"epsilon\" " +
      "for epsilon string)</html>"))

    // Initial means selection
    val productsField = new JTextArea(null, null, 6, 10)
    allControls.add(productsField)


    val inputPanel = new JPanel
    inputPanel.setLayout(new GridLayout(0, 1))
    allControls.add(inputPanel)

    val input = new JTextField()
    inputPanel.add(new JLabel("Input: (not separated by space)"))
    inputPanel.add(input)


    // Action Buttons
    val actionControls = new JPanel
    actionControls.setLayout(new GridLayout(0, 2))
    allControls.add(actionControls)

    val output = new JTextArea
    output.setEditable(false)
    val scroll = new JScrollPane(output)

    val outputPanel = new JPanel()
    outputPanel.setLayout(new BorderLayout)
    outputPanel.add(output, BorderLayout.CENTER)
    outputPanel.setBorder(BorderFactory.createEtchedBorder(border.EtchedBorder.LOWERED))


    val tablePanel = new JPanel()
    tablePanel.setLayout(new BorderLayout)
    tablePanel.setBorder(BorderFactory.createEtchedBorder(border.EtchedBorder.LOWERED))


    val startButton = new JButton("Start")
    actionControls.add(startButton)
    startButton.addActionListener(new ActionListener {
      def actionPerformed(e: ActionEvent): Unit = {

        val G = buildGrammar(nonTerminalField.getText,
          terminalField.getText,
          startSymbolField.getText,
          productsField.getText)
        // TODO


        if (bottomUpParsingRadioButton.isSelected) {
          output.setText("")
          val ip = input.getText.toCharArray.map(_.toString)
          val res = BottomUpParsing.parse(G)(ip)
          for (
            (cstate, position, alpha, beta) <- res._1.reverse
          ) {
            output.append((cstate, position, "#" + alpha.reverse.mkString, beta.mkString).toString() + "\n")
          }
          output.append(res._2.toString)

        } else if (topDownParsingRadioButton.isSelected) {
          output.setText("")
          val ip = input.getText.toCharArray.map(_.toString)
          val res = TopDownParsing.parse(G)(ip)
          for (
            (cstate, position, alpha, beta) <- res._1.reverse
          ) {
            output.append((cstate, position, alpha.reverse.mkString, beta.mkString).toString() + "\n")
          }
          output.append(res._2.toString)
        } else if (computeFirstRadioButton.isSelected) {
          tablePanel.removeAll()
          val computeFirstResult = First.compute(G).reverse
          val table = new JTable(new AbstractTableModel {
            val nonTerminals = G.N.toList

            override def getColumnName(col: Int): String = {
              if (col == 0) {
                ""
              } else {
                (col - 1).toString
              }
            }

            def getRowCount: Int = G.N.size

            def getColumnCount: Int = computeFirstResult.size + 1

            def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef = {
              if (columnIndex == 0) {
                nonTerminals(rowIndex)
              } else {
                computeFirstResult(columnIndex - 1)(nonTerminals(rowIndex)).mkString(", ")
              }
            }
          })

          table.setShowGrid(true)
          table.setGridColor(Color.GRAY)
          tablePanel.add(table, BorderLayout.CENTER)
          tablePanel.add(table.getTableHeader, BorderLayout.PAGE_START)
          revalidate()
        } else {
          tablePanel.removeAll()
          val firstSets = First.compute(G).head
          val (computeFollowResult, numRuleTwo) = Follow.compute(G)
          val displayResult = computeFollowResult.reverse
          val table = new JTable(new AbstractTableModel {
            val nonTerminals = G.N.toList

            override def getColumnName(col: Int): String = col match {
              case 0 => "Rule"
              case 1 => "Product"
              case _ => nonTerminals(col - 2)
            }

            def getRowCount: Int = computeFollowResult.size + 1

            def getColumnCount: Int = G.N.size + 2

            def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef = {


              if (columnIndex == 0) {
                if (rowIndex == 0) "1"
                else if (rowIndex > 0 && rowIndex <= numRuleTwo) {
                  "2"
                } else if (rowIndex < displayResult.size) {
                  "3"
                } else {
                  ""
                }
              } else if (columnIndex == 1) {
                if (rowIndex == 0) {
                  "Initialize"
                } else if (rowIndex < displayResult.size) {
                  val (product, significantNonTermial, followSets) = displayResult(rowIndex)
                  val (lhs, rhs) = product
                  "" + lhs + " -> " + rhs.mkString
                } else {
                  ""
                }
              } else {
                if (rowIndex < displayResult.size) {
                  val (product, significantNonTermial, followSets) = displayResult(rowIndex)
                  if (getColumnName(columnIndex) == significantNonTermial) {
                    followSets(significantNonTermial).mkString(", ")
                  } else {
                    ""
                  }
                } else {
                  computeFollowResult.head._3(getColumnName(columnIndex)).mkString(", ")
                }
              }

            }
          })

          table.setShowGrid(true)
          table.setGridColor(Color.GRAY)
          tablePanel.add(table, BorderLayout.CENTER)
          tablePanel.add(table.getTableHeader, BorderLayout.PAGE_START)

          val textArea = new JTextArea()
          textArea.append("First sets: \n")
          textArea.append(firstSets.toList.map {case (n, set) =>
              "" + n + ": " + set.mkString(", ")
          }.mkString("\n"))
          tablePanel.add(textArea, BorderLayout.SOUTH)
          revalidate()

        }
      }
    })

    parseMode.actionPerformed(null)
    setVisible(true)

    try {
      UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
    } catch {
      case _: Exception => println("Cannot set look and feel, using the default one.")
    }
  }

  val frame = new ParserFrame

  def main(args: Array[String]) {
    frame.repaint()
  }
}
