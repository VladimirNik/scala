package scala.tools.nsc.typechecker

//TODO-REFLECT this trait should be reimplemented in compiler with default behaviour from compiler
//change self and Context in compiler's Analyzer
//refactor?
//TODO-REFLECT - move to Analyzer or macros.runtime?
trait MacrosImpl {
  self: Analyzer =>
  import self.global._
  
  override def macroContextImpl(typer: Typer, prefixTree: Tree, expandeeTree: Tree) = {
    new {
      val universe: self.global.type = self.global
      val callsiteTyper: universe.analyzer.Typer = typer.asInstanceOf[global.analyzer.Typer]
      val expandee = universe.analyzer.macroExpanderAttachment(expandeeTree).original orElse duplicateAndKeepPositions(expandeeTree)
    } with scala.reflect.macros.contexts.Context {
      val prefix = Expr[Nothing](prefixTree)(TypeTag.Nothing)
      override def toString = "MacroContext(%s@%s +%d)".format(expandee.symbol.name, expandee.pos, enclosingMacros.length - 1 /* exclude myself */)
    }
  }
}