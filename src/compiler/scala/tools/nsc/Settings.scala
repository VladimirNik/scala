/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools
package nsc

import scala.reflect.internal.tools.nsc.{Settings => rSettings}

//TODO-REFLECT - refactor it - this code duplicates Settings in scala.reflect.internal.tools.nsc
/** A compatibility stub.
 */
class Settings(errorFn: String => Unit) extends rSettings(errorFn) {
  def this() = this(Console.println)
  
  override def withErrorFn(errorFn: String => Unit): Settings = {
    val settings = new Settings(errorFn)
    copyInto(settings)
    settings
  }
}
