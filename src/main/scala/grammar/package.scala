/**
  * Created by Dandoh on 10/9/16.
  */
package object grammar {

  type SymbolSet = Set[String]
  // product of grammar, e.g S -> List(a, S)
  type Product = (String, List[String])

  // Context-free grammar
  case class CFGrammar(N: SymbolSet, // set of non-terminal symbols
                       T: SymbolSet, // set of terminal symbols
                       S: String, // start symbol
                       products: List[Product]) {

    // products of a given non-terminal symbol with position, start from 1
    def productsWithPositionOf(nonTerm: String): List[(Product, Int)] = products.filter { case (n, _) => n == nonTerm }
      .zip(Stream from 1)

    def productsWithPosition: List[(Product, Int)] = products.zip(Stream from 1)

    def productsWithRhs(rhs: List[String]): List[(Product, Int)] =
      productsWithPosition.filter { case ((_, prhs), pos) => prhs == rhs }

    def productsOf(nonTerm: String): List[Product] = products.filter{ case (n, _) => n == nonTerm}

    // check whether the grammar is left recursive
    // def isLeftRecursive: Boolean = products.exists { case (n, x :: xs) => n == x }
  }

  def buildGrammar(nonTerminals: String,
                   terminals: String,
                   startSymBol: String,
                   products: String): CFGrammar = {
    val N = nonTerminals.split("\\s+").toSet
    val T = terminals.split("\\s+").toSet
    val S = startSymBol

    val ps = for {
      line <- products.split("\\r?\\n")
      xs = line.split("\\s+")
    } yield xs.head -> xs.tail.tail.toList

    CFGrammar(N, T, S, ps.toList)
  }

}
