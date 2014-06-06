package scala.tools
package reflect

import scala.tools.nsc.typechecker.{ Analyzer, Macros }

/** Optimizes system macro expansions by hardwiring them directly to their implementations
 *  bypassing standard reflective load and invoke to avoid the overhead of Java/Scala reflection.
 */
class FastTrack[MacrosAndAnalyzer <: Macros with Analyzer](override val macros: MacrosAndAnalyzer)
  extends scala.reflect.internal.tools.reflect.FastTrack[MacrosAndAnalyzer](macros)
