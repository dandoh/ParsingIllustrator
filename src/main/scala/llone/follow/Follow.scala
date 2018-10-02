package llone.follow

import grammar._
import llone.first.First._
import llone.first._

import scala.collection.mutable

/**
  * Created by Dandoh on 10/26/16.
  *
  */
object Follow {

  type FollowSet = Set[String]

  type Step = (Product, String, Map[String, FollowSet]) // indicate a product add up to
  // follow set of a non-terminal (string) and follow sets after changed

  val END = "#"

  def compute(G: CFGrammar): (List[Step], Int) = {
    val firstSet: Map[String, FirstSet] = First.compute(G).head

    def firstSetOfString(xs: List[String]): Set[String] = {
      xs.map(symbol => {
        if (G.N(symbol)) {
          firstSet(symbol)
        } else {
          Set(symbol)
        }
      }).reduce(plus)
    }

    def computeRuleTwo(xs: List[Product],
                       rhsPos: Int, // next position of rhs, start from 1
                       acc: List[Step]): List[Step] = xs match {
      case (lhs, rhs) :: ys =>
        if (rhsPos == rhs.length) {
          computeRuleTwo(ys, 1, acc)
        } else {
          val symbol = rhs(rhsPos - 1)
          if (G.T(symbol)) {
            computeRuleTwo(xs, rhsPos + 1, acc)
          } else {
            val oldFollowSets: Map[String, FollowSet] = acc.head._3
            val addUp = firstSetOfString(rhs.drop(rhsPos)) -- Set(epsilon)
            val combine = addUp union oldFollowSets(symbol)
            if (combine == oldFollowSets(symbol)) {
              // no new symbol
              computeRuleTwo(xs, rhsPos + 1, acc)
            } else {
              computeRuleTwo(xs, rhsPos + 1, (lhs -> rhs, symbol,
                oldFollowSets.updated(symbol, combine)) :: acc)
            }
          }
        }
      case Nil => acc
    }

    val initial: Map[String, FollowSet] = G.N.map(nonTerm => nonTerm -> Set[String]()).toMap.updated(G.S, Set(END))
    val resultRuleTwo = computeRuleTwo(G.products, 1, List(("Initialize" -> List("Initialize"), G.S, initial)))

    def computeRuleThree(xs: List[Product],
                         rhsPos: Int, // next position of rhs, start from 1
                         acc: List[Step],
                         lastPass: List[Step]): List[Step] = xs match {
      case (lhs, rhs) :: ys =>
        if (rhsPos <= rhs.length && rhs != List(epsilon)) {
          val symbol = rhs(rhsPos - 1)
          if (G.T(symbol)) {
            // move on
            computeRuleThree(xs, rhsPos + 1, acc, lastPass)
          } else {
            val oldFollowSets: Map[String, FollowSet] = acc.head._3
            val addUp: FollowSet = if (rhsPos == rhs.length) {
              oldFollowSets(lhs)
            } else {
              if (firstSetOfString(rhs.drop(rhsPos)) contains epsilon) {
                oldFollowSets(lhs)
              } else {
                Set()
              }
            }

            val combine = addUp union oldFollowSets(symbol)
            if (combine == oldFollowSets(symbol)) {
              // nothing new in the follow set of this non-terminal symbol
              computeRuleThree(xs, rhsPos + 1, acc, lastPass)
            } else {
//              println(lhs -> rhs + " " + symbol + " Add up : " + addUp)
              computeRuleThree(xs, rhsPos + 1, (lhs -> rhs, symbol,
                oldFollowSets updated(symbol, combine)) :: acc, lastPass)
            }
          }
        } else {
          // complete rhs, next
          computeRuleThree(ys, 1, acc, lastPass)
        }


      case Nil =>
        if (acc.head._3 == lastPass.head._3) lastPass
        else computeRuleThree(G.products, 1, acc, acc)
    }


    (computeRuleThree(G.products, 1, resultRuleTwo, resultRuleTwo), resultRuleTwo.size - 1)
  }


  def main(args: Array[String]) {
        val G = CFGrammar(Set("E", "T", "F"),
          Set("+", "a", "(", ")", "*"),
          "E",
          List("E" -> List("T"), "E" -> List("E", "+", "T"),
            "T" -> List("F"), "T" -> List("T", "*", "F"),
            "F" -> List("a"), "F" -> List("(", "E", ")"))
        )

//    val G = CFGrammar(Set("S", "A", "B", "C", "D"),
//      Set("+", "a", "(", ")", "*"),
//      "S",
//      List("S" -> List("B", "A"),
//        "A" -> List("+", "B", "A"), "A" -> List(epsilon),
//        "B" -> List("D", "C"),
//        "C" -> List("*", "D", "C"), "C" -> List(epsilon),
//        "D" -> List("(", "S", ")"), "D" -> List("a"))
//    )

//    for (step <- compute(G).reverse.tail) {
//      println(step._1._1 + " -> " + step._1._2.mkString
//        + ": " + step._2 + " " + step._3(step._2) )
//    }
  }

}
