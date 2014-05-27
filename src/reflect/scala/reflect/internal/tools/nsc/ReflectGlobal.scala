package scala.reflect.internal
package tools.nsc

import java.io.{ File, FileOutputStream, PrintWriter, IOException, FileNotFoundException }
import java.nio.charset.{ Charset, CharsetDecoder, IllegalCharsetNameException, UnsupportedCharsetException }
import java.util.UUID._
import _root_.scala.compat.Platform.currentTime
import _root_.scala.collection.{ mutable, immutable }
import _root_.scala.reflect.io._
import _root_.scala.reflect.internal.tools.nsc.io._
import _root_.scala.reflect.ClassTag
import _root_.scala.reflect.internal.util.{ OffsetPosition, SourceFile, NoSourceFile, BatchSourceFile, ScriptSourceFile }
import _root_.scala.reflect.internal.pickling.{ PickleBuffer, PickleFormat }
import _root_.scala.reflect.io.VirtualFile
import _root_.scala.language.postfixOps
import scala.reflect.internal.tools.nsc.reporters.Reporter
import _root_.scala.reflect.internal.tools.nsc.ast.{TreeGen => AstTreeGen}
import scala.reflect.internal.tools.nsc.util.ClassPath
import scala.reflect.internal.tools.nsc.typechecker.ConstantFolder
import scala.reflect.internal.transform.Erasure
import scala.reflect.internal.tools.nsc.transform._
import scala.reflect.internal.tools.nsc.transform.patmat._

trait ReflectGlobal extends ReflectMix {
  val analyzer: typechecker.Analyzer {
    val global: ReflectGlobal.this.type
  }
  
  var reporter: Reporter = ???
  var globalPhase: Phase = ???
  def settings: Settings = ???

  def currentRun: ReflectRun = ???
  def currentUnit: CompilationUnit = if (currentRun eq null) NoCompilationUnit else currentRun.currentUnit

  var printTypings = settings.Ytyperdebug.value
  def RootClass: ClassSymbol = ???
  
  def registerContext(c: analyzer.Context): Unit = ???
  
  //TODO-REFLECT maybe we don't require such method and its usage
  def enteringTyper[T](op: => T): T = ???
  
  /** This is for WARNINGS which should reach the ears of scala developers
   *  whenever they occur, but are not useful for normal users. They should
   *  be precise, explanatory, and infrequent. Please don't use this as a
   *  logging mechanism. !!! is prefixed to all messages issued via this route
   *  to make them visually distinct.
   */
  @inline final override def devWarning(msg: => String): Unit = devWarning(NoPosition, msg)
  @inline final def devWarning(pos: Position, msg: => String) {
    def pos_s = if (pos eq NoPosition) "" else s" [@ $pos]"
    if (isDeveloper)
      warning(pos, "!!! " + msg)
    else
      log(s"!!!$pos_s $msg") // such warnings always at least logged
  }

  def globalError(pos: Position, msg: String) = reporter.error(pos, msg)
  def warning(pos: Position, msg: String)     = if (settings.fatalWarnings) globalError(pos, msg) else reporter.warning(pos, msg)
  def inform(pos: Position, msg: String)      = reporter.echo(pos, msg)

  //implementation from original Global
  def logError(msg: String, t: Throwable): Unit = ()
  def signalDone(context: analyzer.Context, old: Tree, result: Tree) {}
  /** Register top level class (called on entering the class)
  */
  def registerTopLevelSym(sym: Symbol) {}
  
  type PlatformClassPath = ClassPath[AbstractFile]
  def classPath: PlatformClassPath = ???

    /** Tree generation, usually based on existing symbols. */
//  val gen: _root_.scala.reflect.internal.tools.nsc.ast.TreeGen {
//    def mkAttributedCast(tree: Tree, pt: Type): Tree
//  }
  
    /** Tree generation, usually based on existing symbols. */
  override object gen extends {
    val global: ReflectGlobal.this.type = ReflectGlobal.this
  } with AstTreeGen {
    def mkAttributedCast(tree: Tree, pt: Type): Tree =
      typer.typed(mkCast(tree, pt))
  }
  
    /** Print tree in detailed form */
  object nodePrinters extends {
    val global: ReflectGlobal.this.type = ReflectGlobal.this
  } with ast.NodePrinters {
    var lastPrintedPhase: Phase = NoPhase
    var lastPrintedSource: String = ""
    infolevel = InfoLevel.Verbose

    def showUnit(unit: CompilationUnit) {
      print(" // " + unit.source)
      if (unit.body == null) println(": tree is null")
      else {
        val source = util.stringFromWriter(w => newTreePrinter(w) print unit.body)

        // treePrinter show unit.body
        if (lastPrintedSource == source)
          println(": tree is unchanged since " + lastPrintedPhase)
        else {
          lastPrintedPhase = phase.prev // since we're running inside "exitingPhase"
          lastPrintedSource = source
          println("")
          println(source)
          println("")
        }
      }
    }
  }
  
  object constfold extends {
    val global: ReflectGlobal.this.type = ReflectGlobal.this
  } with ConstantFolder

  def withInfoLevel[T](infolevel: nodePrinters.InfoLevel.Value)(op: => T) = {
    val saved = nodePrinters.infolevel
    try {
      nodePrinters.infolevel = infolevel
      op
    } finally {
      nodePrinters.infolevel = saved
    }
  }

//  val typer: analyzer.Typer
  object typer extends analyzer.Typer(
    analyzer.NoContext.make(EmptyTree, RootClass, newScope)
  )

  // phaseName = "patmat"
  val patmat: PatternMatching with ScalacPatternExpanders with TreeAndTypeAnalysis {
    val global: ReflectGlobal.this.type
  } = ???

  //TODO we can make object here as in compiler Global but in compiler it uses Global
  // phaseName = "erasure"
  object erasureArray extends {
    val global: ReflectGlobal.this.type = ReflectGlobal.this
  } with ErasureGenericArray
  
  val loaders: GlobalSymbolLoaders = ???
  
  //TODO: ConditionalWarning from Global
  /** Collects for certain classes of warnings during this run. */
  class ConditionalWarning(what: String, option: Settings#BooleanSetting) {
    val warnings = mutable.LinkedHashMap[Position, String]()
    def warn(pos: Position, msg: String) =
      if (option) reporter.warning(pos, msg)
      else if (!(warnings contains pos)) warnings += ((pos, msg))
    def summarize() =
      if (warnings.nonEmpty && (option.isDefault || settings.fatalWarnings))
        warning("there were %d %s warning(s); re-run with %s for details".format(warnings.size, what, option.name))
  }
  
  def beforeErasure = phaseId(currentPeriod) < currentRun.erasurePhase.id
  def beforeErasure(global: ReflectGlobal) = global.phase.id < global.currentRun.erasurePhase.id

  //TODO: Run from Global
  trait ReflectRun extends RunContextApi {
     /** Have been running into too many init order issues with Run
     *  during erroneous conditions.  Moved all these vals up to the
     *  top of the file so at least they're not trivially null.
     */
    var isDefined: Boolean = ???
    /** The currently compiled unit; set from GlobalPhase */
    var currentUnit: CompilationUnit = ???

    /** A map from compiled top-level symbols to their source files */
    val symSource: mutable.HashMap[Symbol, AbstractFile] = ???
    
    def canRedefine(sym: Symbol): Boolean = ???
    def compiles(sym: Symbol): Boolean = ???

    var reportedFeature: Set[Symbol] = ???
    val compiledFiles: mutable.HashSet[String] = ???
    val runDefinitions: definitions.RunDefinitions = ???
    
    val typerPhase: Phase = ???
    val erasurePhase: Phase = ???

    // This change broke sbt; I gave it the thrilling name of uncheckedWarnings0 so
    // as to recover uncheckedWarnings for its ever-fragile compiler interface.
    val deprecationWarnings0 = new ConditionalWarning("deprecation", settings.deprecation)
    val uncheckedWarnings0 = new ConditionalWarning("unchecked", settings.unchecked)
    val featureWarnings = new ConditionalWarning("feature", settings.feature)
    val inlinerWarnings = new ConditionalWarning("inliner", settings.YinlinerWarnings)
    val allConditionalWarnings = List(deprecationWarnings0, uncheckedWarnings0, featureWarnings, inlinerWarnings)

    def uncheckedWarnings: List[(Position, String)] = uncheckedWarnings0.warnings.toList // used in sbt
    def deprecationWarnings: List[(Position, String)] = deprecationWarnings0.warnings.toList // used in sbt
    
    var seenMacroExpansionsFallingBack: Boolean = ???
  }
}

trait ReflectMix extends SymbolTable
    with ast.Trees
    with Printers
    with ast.DocComments
    with Positions 
    with CompilationUnits {
  self: ReflectGlobal =>
}