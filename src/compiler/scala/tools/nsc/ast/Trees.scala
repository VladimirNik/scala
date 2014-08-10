/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package ast

import scala.reflect.ClassTag
import scala.reflect.internal.Flags.BYNAMEPARAM
import scala.reflect.internal.Flags.DEFAULTPARAM
import scala.reflect.internal.Flags.IMPLICIT
import scala.reflect.internal.Flags.PARAM
import scala.reflect.internal.Flags.PARAMACCESSOR
import scala.reflect.internal.Flags.PRESUPER
import scala.reflect.internal.Flags.TRAIT
import scala.compat.Platform.EOL

trait Trees extends scala.reflect.internal.tools.nsc.ast.Trees { self: Global =>

  //TODO-REFLECT - wrapper to save old name
  /** Construct class definition with given class symbol, value parameters,
   *  supercall arguments and template body.
   *
   *  @param sym        the class symbol
   *  @param constrMods the modifiers for the class constructor, i.e. as in `class C private (...)`
   *  @param vparamss   the value parameters -- if they have symbols they
   *                    should be owned by `sym`
   *  @param body       the template statements without primary constructor
   *                    and value parameter fields.
   */
  def ClassDef(sym: Symbol, constrMods: Modifiers, vparamss: List[List[ValDef]], body: List[Tree], superPos: Position): ClassDef = 
    mkClassDef(sym, constrMods, vparamss, body, superPos)

 // --- subcomponents --------------------------------------------------

  object treeInfo extends {
    val global: Trees.this.type = self
  } with TreeInfo
  
  //TODO-REFLECT problem with CompilationUnit in reflect (moved from refact)
  class Transformer extends super.Transformer {
    def transformUnit(unit: CompilationUnit) {
      try unit.body = transform(unit.body)
      catch {
        case ex: Exception =>
          log(supplementErrorMessage("unhandled exception while transforming "+unit))
          throw ex
      }
    }
  }

  // used when a phase is disabled
  object noopTransformer extends Transformer {
    override def transformUnit(unit: CompilationUnit): Unit = {}
  }
  
  //TODO-REFLECT - probably this override value can be removed (see implicit)
  implicit override val TreeCopierTag: ClassTag[TreeCopier] = TreeCopierTagImpl
 }
