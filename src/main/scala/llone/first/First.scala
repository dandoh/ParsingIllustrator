package llone.first

import grammar.CFGrammar

/**
  * Created by Dandoh on 10/26/16.
  */
object First {

  type FirstSet = Set[String]

  val epsilon = "epsilon"

  def compute(G: CFGrammar): List[Map[String, FirstSet]] = {
    def compute(G: CFGrammar, acc: List[Map[String, FirstSet]]): List[Map[String, FirstSet]] = {
      val previous = acc.head
      val next = {
        for {
          nonTerm <- G.N
        } yield {
          val product = G.productsOf(nonTerm)
          // calculate F_i of this non-terminal symbol
          val firstSet: FirstSet = {
            for {
              (lhs, rhs) <- product
            } yield {
              // every product of nonTerm add up to the first set
              if (rhs == List(epsilon))
                Set(epsilon)
              else
                rhs.map(symbol => {
                  if (G.N(symbol)) previous(symbol) else Set(symbol)
                }).reduce(plus)

            }
          }.foldLeft(Set[String]())(_ ++ _) // union all sets

          nonTerm -> firstSet
        }
      }.toMap

      // move on if there is change, otherwise terminate
      if (previous == next) next :: acc
      else compute(G, next :: acc)
    }

    // calculate initial first set
    val initial = for {
      nonTerm <- G.N
    } yield {
      val products = G.productsOf(nonTerm)
      val firstSetList: List[Set[String]] =
        for {
          (lhs, rhs) <- products
        } yield {
          if (rhs == List(epsilon)) {
            Set(epsilon)
          } else {
            if (G.T(rhs.head))
              Set(rhs.head)
            else
              Set[String]()
          }
        }

      val initialFirstSet = firstSetList.foldLeft(Set[String]())(_ ++ _)
      nonTerm -> initialFirstSet
    }

    compute(G, List(initial.toMap))
  }

  def plus(a: FirstSet, b: FirstSet): FirstSet = {
    val res = if (a.isEmpty) Set[String]()
    else {
      if (a.contains(epsilon)) (a - epsilon) ++ b
      else a
    }

    res
  }

  def main(args: Array[String]) {
    //    val G = CFGrammar(Set("E", "T", "F"),
    //      Set("+", "a", "(", ")", "*"),
    //      "E",
    //      List("E" -> List("T"), "E" -> List("E", "+", "T"),
    //        "T" -> List("F"), "T" -> List("T", "*", "F"),
    //        "F" -> List("a"), "F" -> List("(", "E", ")"))
    //    )

    val G = CFGrammar(Set("S", "A", "B", "C", "D"),
      Set("+", "a", "(", ")", "*"),
      "S",
      List("S" -> List("B", "A"),
        "A" -> List("+", "B", "A"), "A" -> List(epsilon),
        "B" -> List("D", "C"),
        "C" -> List("*", "D", "C"), "C" -> List(epsilon),
        "D" -> List("(", "S", ")"), "D" -> List("a"))
    )


    for (
      (nonTerminal, setFirst) <- compute(G).last
    ) {
      println(nonTerminal + " : " + setFirst)
    }
  }
}
