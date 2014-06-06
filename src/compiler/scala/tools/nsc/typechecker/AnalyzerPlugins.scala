/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package typechecker

/**
 *  @author Lukas Rytz
 *  @version 1.0
 */
trait AnalyzerPlugins extends scala.reflect.internal.tools.nsc.typechecker.AnalyzerPlugins {
  self: Analyzer =>
}
