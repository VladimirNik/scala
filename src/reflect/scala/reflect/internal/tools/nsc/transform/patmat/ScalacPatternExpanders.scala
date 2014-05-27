package scala.reflect.internal
package tools.nsc
package transform.patmat

trait ScalacPatternExpanders {
  val global: SymbolTable
  import global._
  
  trait ScalacPatternExpander
  
  //Add to AlignedOps in compiler's ScalacPatternExpanders
  trait AlignedBase {
    def unexpandedFormals: List[Type]
  }
  val alignPatterns: ScalacPatternExpander {
    def apply(sel: Tree, args: List[Tree]): AlignedBase
  }
}