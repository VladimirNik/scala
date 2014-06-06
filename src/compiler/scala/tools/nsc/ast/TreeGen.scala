/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package ast

import scala.collection.mutable.ListBuffer
import symtab.Flags._
import scala.language.postfixOps

/** XXX to resolve: TreeGen only assumes global is a SymbolTable, but
 *  TreeDSL at the moment expects a Global.  Can we get by with SymbolTable?
 */
abstract class TreeGen extends scala.reflect.internal.tools.nsc.ast.TreeGen {
  val global: Global

  import global._
  import definitions._

  /**
   * Create a method based on a Function
   *
   * Used both to under `-Ydelambdafy:method` create a lifted function and
   * under `-Ydelamdafy:inline` to create the apply method on the anonymous
   * class.
   *
   * It creates a method definition with value params cloned from the
   * original lambda. Then it calls a supplied function to create
   * the body and types the result. Finally
   * everything is wrapped up in a DefDef
   *
   * @param owner The owner for the new method
   * @param name name for the new method
   * @param additionalFlags flags to be put on the method in addition to FINAL
   */
  def mkMethodFromFunction(localTyper: analyzer.Typer)
                          (fun: Function, owner: Symbol, name: TermName, additionalFlags: FlagSet = NoFlags) = {
    val funParams = fun.vparams map (_.symbol)
    val formals :+ restpe = fun.tpe.typeArgs

    val methSym = owner.newMethod(name, fun.pos, FINAL | additionalFlags)

    val paramSyms = map2(formals, fun.vparams) {
      (tp, vparam) => methSym.newSyntheticValueParam(tp, vparam.name)
    }

    methSym setInfo MethodType(paramSyms, restpe.deconst)

    fun.body.substituteSymbols(funParams, paramSyms)
    fun.body changeOwner (fun.symbol -> methSym)

    val methDef = DefDef(methSym, fun.body)

    // Have to repack the type to avoid mismatches when existentials
    // appear in the result - see SI-4869.
    methDef.tpt setType localTyper.packedType(fun.body, methSym).deconst
    methDef
  }
}
