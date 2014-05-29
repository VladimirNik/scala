package scala.reflect.internal
package tools

//TODO-REFLECT refactor compiler's version
package object nsc {
  def EXPRmode = Mode.EXPRmode

  @deprecated("Use scala.reflect.internal.util.ListOfNil", "2.11.0")
  //TODO_REFLECT remove _root_
  lazy val ListOfNil = _root_.scala.reflect.internal.util.ListOfNil
}