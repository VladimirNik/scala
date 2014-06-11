package scala.reflect.internal
package tools

//TODO-REFLECT refactor compiler's version
package object nsc {
  def EXPRmode = Mode.EXPRmode

  @deprecated("Use scala.reflect.internal.util.ListOfNil", "2.11.0")
  lazy val ListOfNil = scala.reflect.internal.util.ListOfNil
}
