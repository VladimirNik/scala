package scala.reflect.reify
package utils

import scala.tools.nsc.Global

trait Utils extends scala.reflect.moved.reify.utils.Utils
               with NodePrinters
               with Extractors
               with SymbolTables
               with StdAttachments {

  val global: Global
  val typer: global.analyzer.Typer
}