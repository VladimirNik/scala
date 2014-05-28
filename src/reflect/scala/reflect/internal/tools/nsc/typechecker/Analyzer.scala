/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect.internal.tools.nsc
package typechecker

import scala.reflect.internal.util.Statistics

/** The main attribution phase.
 */
trait Analyzer extends AnyRef
            with Contexts
            with Namers
            with Typers
            with Infer
            with Implicits
            with EtaExpansion
            with SyntheticMethods
            with Unapplies
            with Macros
            with NamesDefaults
            with TypeDiagnostics
            with ContextErrors
            with StdAttachments
            with AnalyzerPlugins {
  val global : ReflectGlobal
}
