package scala.reflect.moved.reify
package utils

import scala.reflect.internal.tools.nsc.TypecheckerRequirements

trait Utils extends NodePrinters
               with Extractors
               with SymbolTables
               with StdAttachments {

  val global: TypecheckerRequirements
  val typer: global.analyzer.Typer

  lazy val reifier: Reifier { val global: Utils.this.global.type } = getReifier
  def getReifier: Reifier { val global: Utils.this.global.type } = ???
  def hasReifier = false

  val reifyDebug = global.settings.Yreifydebug.value
  val reifyCopypaste = global.settings.Yreifycopypaste.value
  val reifyTrace = scala.tools.nsc.util.trace when reifyDebug
}