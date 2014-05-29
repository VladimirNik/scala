package scala.reflect.internal.tools.nsc

import java.io.{ OutputStream, PrintStream, ByteArrayOutputStream, PrintWriter, StringWriter, Reader }

//TODO-REFLECT - REFACTOR package object in compiler
package object util {
  type HashSet[T >: Null <: AnyRef] = scala.reflect.internal.util.HashSet[T]
  val HashSet = scala.reflect.internal.util.HashSet

  /** Apply a function and return the passed value */
  def returning[T](x: T)(f: T => Unit): T = { f(x) ; x }

  /** Generate a string using a routine that wants to write on a stream. */
  def stringFromWriter(writer: PrintWriter => Unit): String = {
    val stringWriter = new StringWriter()
    val stream = new NewLinePrintWriter(stringWriter)
    writer(stream)
    stream.close()
    stringWriter.toString
  }

  def stackTraceString(ex: Throwable): String = stringFromWriter(ex printStackTrace _)

  lazy val trace = new SimpleTracer(System.out)

  @deprecated("Moved to scala.reflect.internal.util.ScalaClassLoader", "2.11.0")
  val ScalaClassLoader = scala.reflect.internal.util.ScalaClassLoader

  @deprecated("Moved to scala.reflect.internal.util.ScalaClassLoader", "2.11.0")
  type ScalaClassLoader = scala.reflect.internal.util.ScalaClassLoader
}