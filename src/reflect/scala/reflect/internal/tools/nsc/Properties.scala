/* NSC -- new Scala compiler
 * Copyright 2006-2013 LAMP/EPFL
 * @author  Stephane Micheloud
 */

package scala.reflect.internal.tools.nsc

/** Loads `compiler.properties` from the jar archive file.
 */
object Properties extends scala.util.PropertiesTrait {
  protected def propCategory   = "compiler"
  //TODO-Reflect Here Global is Required
  protected def pickJarBasedOn = classOf[ReflectGlobal]

  // settings based on jar properties
  def residentPromptString = scalaPropOrElse("resident.prompt", "\nnsc> ")
  def shellPromptString    = scalaPropOrElse("shell.prompt", "\nscala> ")

  // derived values
  def isEmacsShell         = propOrEmpty("env.emacs") != ""
}