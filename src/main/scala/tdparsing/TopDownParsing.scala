package tdparsing

import grammar._

/**
  * Created by Dandoh on 10/9/16.
  */
object TopDownParsing {

  type State = (Char, Int, List[String], List[String])

  val NormalState = 'q'
  val BacktrackState = 'b'
  val TerminateState = 't'

  // Parse an array of input. The input array start at position 0.
  def parse(G: CFGrammar)(input: Array[String]): (List[State], Boolean) = {

    val NULL = "#"

    def parsing(acc: List[State], appliedProducts: List[(String, Int)]): (List[State], Boolean) = acc match {
      case (cstate, position, alpha, beta) :: xs =>
        cstate match {
          case NormalState =>
            // normal state, check the first item of beta

            val currentSymbol = beta.head
            if (position == input.length + 1 && currentSymbol == NULL) {
              parsing((TerminateState,
                position,
                alpha,
                beta.tail) :: acc,
                appliedProducts)
            } else {
              if (G.N(currentSymbol)) {
                //                println("Is non-terminal")
                // non-terminal symbol
                val productWithPositions = G.productsWithPositionOf(currentSymbol)
                // apply the first product
                val (prd, pos) = productWithPositions.head
                val (lhs, rhs) = prd

                parsing((NormalState,
                  position,
                  makeProductString((prd, pos)) :: alpha,
                  rhs ++ beta.tail) :: acc,
                  (lhs, pos) :: appliedProducts)
              } else {
                //                println("Is terminal")
                // if current symbol is a terminal symbol or #(NULL symbol)
                if (position <= input.length && currentSymbol == input(position - 1)) {
                  //                  println("Matched")
                  parsing((NormalState,
                    position + 1,
                    currentSymbol :: alpha,
                    beta.tail) :: acc,
                    appliedProducts)
                } else {
                  //                  println("Turning to backtrack mode")
                  // back track
                  parsing((BacktrackState,
                    position,
                    alpha,
                    beta) :: acc,
                    appliedProducts)
                }
              }
            }
          case BacktrackState =>
            if (alpha.nonEmpty) {
              val headAlpha = alpha.head
              if (G.T(headAlpha)) {
                //              println("Is terminal")
                // backtracking when the last appended symbol to alpha is a terminal symbol
                parsing((BacktrackState,
                  position - 1,
                  alpha.tail,
                  headAlpha :: beta) :: acc,
                  appliedProducts)
              } else {
                //              println("Is non-terminal")
                // the last appended is a non-terminal symbol
                val (nonTerminal, prdPos) = appliedProducts.head
                val productWithPositions = G.productsWithPositionOf(nonTerminal)
                // check if a product of non-terminal symbol is left to apply
                if (prdPos == productWithPositions.length) {

                  // last applied product
                  val (_, rhs) = productWithPositions(prdPos - 1)._1
                  // out of product
                  parsing((BacktrackState,
                    position,
                    alpha.tail,
                    nonTerminal :: beta.drop(rhs.length)) :: acc,
                    appliedProducts.tail)
                } else {
                  // next product to be applied
                  val (_, rhs) = productWithPositions(prdPos)._1
                  val (_, oldRhs) = productWithPositions(prdPos - 1)._1
                  parsing((NormalState,
                    position,
                    makeProductString(((nonTerminal, rhs), prdPos + 1)) :: alpha.tail,
                    rhs ++ beta.drop(oldRhs.length)) :: acc,
                    (nonTerminal, prdPos + 1) :: appliedProducts.tail)
                }
              }
            } else {
              (acc, false)
            }

          case TerminateState =>
            (acc, true)
        }
      case Nil => (acc, false)
    }

    parsing(List((NormalState,
      1,
      Nil,
      List(G.S, NULL))),
      Nil)
  }

  def makeProductString(p: (Product, Int)): String = p match {
    case ((n, _), position) => n + "_" + position
  }

  def main(args: Array[String]) {
    val G = CFGrammar(Set("S"),
      Set("a", "b", "c"),
      "S",
      List("S" -> List("a", "S"), "S" -> List("a", "S", "b", "S"), "S" -> List("c"))
    )

    val parser: Array[String] => (List[State], Boolean) = parse(G)
    val res = parser("aacbc".toCharArray.map(_.toString))

    for {
      (cstate, position, alpha, beta) <- res._1.reverse
    } println((cstate, position, alpha.reverse.mkString, beta.mkString))
    println(res._2)
  }
}
