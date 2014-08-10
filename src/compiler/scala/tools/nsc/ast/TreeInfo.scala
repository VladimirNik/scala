/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package ast

/** This class ...
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
abstract class TreeInfo extends scala.reflect.internal.tools.nsc.ast.TreeInfo {
  val global: Global
  import global._
  import definitions._

  // Extractors for value classes.
  object ValueClass {
    def isValueClass(tpe: Type)                  = enteringErasure(tpe.typeSymbol.isDerivedValueClass)
    def valueUnbox(tpe: Type)                    = enteringErasure(tpe.typeSymbol.derivedValueClassUnbox)

    // B.unbox. Returns B.
    object Unbox {
      def unapply(t: Tree): Option[Tree] = t match {
        case Apply(sel @ Select(ref, _), Nil) if valueUnbox(ref.tpe) == sel.symbol => Some(ref)
        case _                                                                     => None
      }
    }
    // new B(v). Returns B and v.
    object Box {
      def unapply(t: Tree): Option[(Tree, Type)] = t match {
        case Apply(sel @ Select(New(tpt), nme.CONSTRUCTOR), v :: Nil) => Some((v, tpt.tpe.finalResultType))
        case _                                                        => None
      }
    }
    // (new B(v)).unbox. returns v.
    object BoxAndUnbox {
      def unapply(t: Tree): Option[Tree] = t match {
        case Unbox(Box(v, tpe)) if isValueClass(tpe) => Some(v)
        case _                                       => None
      }
    }
    // new B(v1) op new B(v2) where op is == or !=. Returns v1, op, v2.
    object BoxAndCompare {
      def unapply(t: Tree): Option[(Tree, Symbol, Tree)] = t match {
        case BinaryOp(Box(v1, tpe1), op @ (Object_== | Object_!=), Box(v2, tpe2)) if isValueClass(tpe1) && tpe1 =:= tpe2 => Some((v1, op, v2))
        case _                                                                                                           => None
      }
    }
  }
}
