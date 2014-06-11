package scala.reflect.internal.tools.nsc
package transform.patmat

trait PatternMatching {
  val global: TypecheckerRequirements
  import global._

//  val vpmName = global.analyzer.vpmName

  class PureMatchTranslator(val typer: analyzer.Typer, val matchStrategy: Tree) {
    def translateMatch(match_ : Match): Tree = ???
  }
}