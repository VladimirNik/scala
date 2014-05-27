package scala.reflect.internal.tools.reflect
package quasiquotes

//TODO-REFLECT - implementation of this class in compiler's package
//scala.tools.reflect.quasiquotes. Parsers class in compilers should extends this Trait
/** Builds upon the vanilla Scala parser and teams up together with Placeholders.scala to emulate holes.
 *  A principled solution to splicing into Scala syntax would be a parser that natively supports holes.
 *  Unfortunately, that's outside of our reach in Scala 2.11, so we have to emulate.
 */
trait Parsers { self: Quasiquotes =>
  import global._

  abstract class Parser {
    def parse(code: String): Tree
  }

  val TermParser: Parser = ???
  val TypeParser: Parser = ???
  val CaseParser: Parser = ???
  val PatternParser: Parser = ???
  val ForEnumeratorParser: Parser = ???
  val Q: Parser {
    def unapply(tree: Tree): Option[Tree]
  } = ???
  val FreshName: FreshNameExtractor = ???
}
