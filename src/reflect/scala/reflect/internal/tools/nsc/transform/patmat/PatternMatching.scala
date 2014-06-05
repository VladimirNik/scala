package scala.reflect.internal.tools.nsc
package transform.patmat

trait PatternMatching {
  val global: ReflectGlobal
  import global._
  
  object vpmName {
    val one       = newTermName("one")
    val flatMap   = newTermName("flatMap")
    val get       = newTermName("get")
    val guard     = newTermName("guard")
    val isEmpty   = newTermName("isEmpty")
    val orElse    = newTermName("orElse")
    val outer     = newTermName("<outer>")
    val runOrElse = newTermName("runOrElse")
    val zero      = newTermName("zero")
    val _match    = newTermName("__match") // don't call the val __match, since that will trigger virtual pattern matching...

    def counted(str: String, i: Int) = newTermName(str + i)
  }
  
  class PureMatchTranslator(val typer: analyzer.Typer, val matchStrategy: Tree) {
    def translateMatch(match_ : Match): Tree = ???
  }
}