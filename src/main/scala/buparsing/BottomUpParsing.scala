package buparsing

import grammar._

/**
  * Created by Dandoh on 10/10/16.
  */
object BottomUpParsing {
  type State = (Char, Int, List[String], List[String])

  // state
  val NormalState = 'q'
  val BacktrackState = 'b'
  val TerminateState = 't'

  // shift symbol
  val Shift = "s"

  def parse(G: CFGrammar)(input: Array[String]): (List[State], Boolean) = {
    def parse(acc: List[State], optionApplied: List[Int]): (List[State], Boolean) = acc match {
      case (cstate, position, alpha, beta) :: xs =>
        println((cstate, position, "#" + alpha.reverse.mkString, beta.mkString))
        cstate match {
          case NormalState =>
            if (position == input.length + 1 && alpha == List(G.S)) {
              parse((TerminateState,
                position,
                alpha,
                beta) :: acc,
                optionApplied)
            } else {
              // check contractible
              val cp: List[(Product, Int)] = contractibleProducts(G, alpha)
              //              println(cp.length + " product available to contract ")
              if (cp.nonEmpty) {
                // contract using first product
                val ((lhs, rhs), pos) = cp.head
                parse((NormalState,
                  position,
                  lhs :: alpha.drop(rhs.length),
                  pos.toString :: beta) :: acc,
                  0 :: optionApplied)
              } else {
                // only able to shift
                if (position != input.length + 1) {
                  val apos = input(position - 1)
                  parse((NormalState,
                    position + 1,
                    apos :: alpha,
                    Shift :: beta) :: acc,
                    optionApplied)
                } else {
                  parse((BacktrackState,
                    position,
                    alpha,
                    beta) :: acc,
                    optionApplied)
                }
              }
            }
          case BacktrackState =>
            //            println("backtracking")
            if (beta.nonEmpty) {
              val betaHead = beta.head
              if (betaHead == Shift) {
                parse((BacktrackState,
                  position - 1,
                  alpha.tail,
                  beta.tail) :: acc,
                  optionApplied) // TODO
              } else {
                val (lastAppliedProduct, _) = G.productsWithPosition.filter { case (_, pos) => pos == betaHead.toInt }.head
                val (lhs, rhs) = lastAppliedProduct
                val oldAlpha = rhs.reverse ++ alpha.drop(lhs.length)
                val oldBeta = beta.tail

                val cp: List[(Product, Int)] = contractibleProducts(G, oldAlpha)
                //                println(cp.length + " product available to contract ")
                if (optionApplied.head < cp.length - 1) {
                  //                  println("alter by another product")
                  // alter by next product
                  val (nextProduct, pos) = cp(optionApplied.head + 1)
                  val (lhs, rhs) = nextProduct
                  parse((NormalState,
                    position,
                    lhs :: oldAlpha.drop(rhs.length),
                    pos.toString :: oldBeta) :: acc,
                    optionApplied.head + 1 :: optionApplied.tail)
                } else {
                  if (position == input.length + 1) {
                    // not able to shift
                    parse((BacktrackState,
                      position,
                      oldAlpha,
                      oldBeta) :: acc,
                      optionApplied.tail)
                  } else {
                    // cancel and shift
                    val apos = input(position - 1)
                    parse((NormalState,
                      position + 1,
                      apos :: oldAlpha,
                      Shift :: oldBeta) :: acc,
                      optionApplied.tail)
                  }
                }

              }
            } else {
              (acc, false)
            }

          case TerminateState => (acc, true)
        }
      case Nil => (acc, false)
    }

    parse(
      List((NormalState,
        1,
        List(),
        List())), List())
  }

  def contractibleProducts(G: CFGrammar, xs: List[String]): List[(Product, Int)] = {
    (1 to xs.length).flatMap(i => {
      val rhs = xs.take(i).reverse
      G.productsWithRhs(rhs)
    }).toList
  }


  def main(args: Array[String]) {
    //    val G = CFGrammar(Set("E", "T", "F"),
    //      Set("a", "*", "+", "(", ")"),
    //      "E",
    //      List("E" -> List("E", "+", "T"), "E" -> List("T"), "T" -> List("T", "*", "F"), "T" -> List("F"),
    //        "F" -> List("(", "E", ")"), "F" -> List("a"))
    //    )
    //
    //    val parser: Array[String] => (List[State], Boolean) = parsing(G)
    //    val res = parser("+a".toCharArray.map(_.toString))
    val G = CFGrammar(Set("S"),
      Set("a", "b", "c"),
      "S",
      List("S" -> List("a", "S", "b", "S"), "S" -> List("a", "S"), "S" -> List("c"))
    )

    val parser: Array[String] => (List[State], Boolean) = parse(G)
    val res = parser("aacbc".toCharArray.map(_.toString))
    println(res._2)

  }

}
