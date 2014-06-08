package scala.reflect.internal
package tools.nsc
package transform.patmat

trait ScalacPatternExpanders {
  val global: SymbolTable
  import global._

  //TODO-REFLECT wrapper to simplify reflect
  def patternsUnexpandedFormals(sel: Tree, args: List[Tree]): List[Type] = ???

}