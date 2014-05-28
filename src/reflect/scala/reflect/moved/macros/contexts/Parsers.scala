package scala.reflect.moved.macros.contexts

//import _root_.scala.reflect.internal.tools.nsc.reporters.StoreReporter

trait Parsers {
  self: Context =>
  import global._

  def parse(code: String): Tree = ??? /* {
    val sreporter = new StoreReporter()
    val unit = new CompilationUnit(newSourceFile(code, "<macro>")) { override def reporter = sreporter }
    val parser: Tree = ??? //newUnitParser(unit) - UnitParser from scala.tools.nsc.ast.parser is Required
    val tree = gen.mkTreeOrBlock(parser.parseStatsOrPackages())
    sreporter.infos.foreach {
      case sreporter.Info(pos, msg, sreporter.ERROR) => throw ParseException(pos, msg)
    }
    tree
  }
  *
  */
}