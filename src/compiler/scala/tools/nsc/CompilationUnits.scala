/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc

import scala.reflect.internal.util.{ SourceFile, NoSourceFile, FreshNameCreator }
import scala.collection.mutable
import scala.collection.mutable.{ LinkedHashSet, ListBuffer }
import scala.tools.nsc.reporters.Reporter

trait CompilationUnits extends scala.reflect.internal.tools.nsc.CompilationUnits { global: Global =>

//  override object NoCompilationUnit extends CompilationUnit(NoSourceFile) {
//    //TODO-REFLECT remove code duplication
//    override lazy val isJava = false
//    override def exists = false
//    override def toString() = "NoCompilationUnit"
//  }

  override lazy val NoCompilationUnit = new CompilationUnit(NoSourceFile) {
    //TODO-REFLECT remove code duplication
    override lazy val isJava = false
    override def exists = false
    override def toString() = "NoCompilationUnit"
  }

  /** One unit of compilation that has been submitted to the compiler.
    * It typically corresponds to a single file of source code.  It includes
    * error-reporting hooks.  */
  class CompilationUnit(override val source: SourceFile) extends super.CompilationUnit(source) { self =>

    type Icode = icodes.IClass
    /** The icode representation of classes in this compilation unit.
     *  It is empty up to phase 'icode'.
     */
    override val icode: LinkedHashSet[Icode] = new LinkedHashSet
  }
}
