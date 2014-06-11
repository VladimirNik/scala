package scala.reflect.internal
package tools.nsc

import java.io.{ File, FileOutputStream, PrintWriter, IOException, FileNotFoundException }
import java.nio.charset.{ Charset, CharsetDecoder, IllegalCharsetNameException, UnsupportedCharsetException }
import java.util.UUID._
import scala.compat.Platform.currentTime
import scala.collection.{ mutable, immutable }
import scala.reflect.io._
import scala.reflect.internal.tools.nsc.io._
import scala.reflect.ClassTag
import scala.reflect.internal.util.{ OffsetPosition, SourceFile, NoSourceFile, BatchSourceFile, ScriptSourceFile }
import scala.reflect.internal.pickling.{ PickleBuffer, PickleFormat }
import scala.reflect.io.VirtualFile
import scala.language.postfixOps
import scala.reflect.internal.tools.nsc.reporters.Reporter
import scala.reflect.internal.tools.nsc.ast.{TreeGen => AstTreeGen}
import scala.reflect.internal.tools.nsc.util.ClassPath
import scala.reflect.internal.tools.nsc.typechecker.ConstantFolder
import scala.reflect.internal.transform.Erasure
import scala.reflect.internal.tools.nsc.transform._
import scala.reflect.internal.tools.nsc.transform.patmat._
import scala.reflect.internal.tools.reflect.quasiquotes.Quasiquotes

trait Typechecker extends SymbolTable
    with Printers
    with Positions 
    with CompilationUnits
    with QuasiquotesImpl {
  self: ReflectGlobal =>

  val analyzer: typechecker.Analyzer {
    val global: Typechecker.this.type
  }
  // phaseName = "patmat"
  val patmat: PatternMatching with ScalacPatternExpanders with TreeAndTypeAnalysis {
    val global: Typechecker.this.type
  }

  var reporter: Reporter
  def settings: Settings

  var globalPhase: Phase
  def currentRun: Run

  def RootClass: ClassSymbol
  //TODO-REFLECT maybe we don't require such method and its usage
  def enteringTyper[T](op: => T): T
  def registerContext(c: analyzer.Context): Unit
  
  def classPath: PlatformClassPath
  val loaders: ReflectSymbolLoaders

  trait Run extends RunContextApi with RunBase {
     /** Have been running into too many init order issues with Run
     *  during erroneous conditions.  Moved all these vals up to the
     *  top of the file so at least they're not trivially null.
     */
    var isDefined: Boolean
    
    //TODO-REFLECT in original implementation: var currentUnit: CompilationUnit
    //but can't be defined here as var because of CompilationUnit problem
    /** The currently compiled unit; set from GlobalPhase */
    def currentUnit: CompilationUnit
    
    /** A map from compiled top-level symbols to their source files */
    val symSource: mutable.HashMap[Symbol, AbstractFile]
    val compiledFiles: mutable.HashSet[String]
    val runDefinitions: definitions.RunDefinitions
    
    def canRedefine(sym: Symbol): Boolean
    def compiles(sym: Symbol): Boolean
    var reportedFeature: Set[Symbol]

    val typerPhase: Phase
    val erasurePhase: Phase
    
    var seenMacroExpansionsFallingBack: Boolean
  }
}

trait ReflectGlobal extends Typechecker {
  lazy val analyzer: typechecker.Analyzer {
    val global: ReflectGlobal.this.type
  } = new {
    val global: ReflectGlobal.this.type = ReflectGlobal.this
  } with typechecker.Analyzer

//  override val gen: scala.reflect.internal.tools.nsc.ast.TreeGen {
//    val global: ReflectGlobal.this.type
//    def mkAttributedCast(tree: Tree, pt: Type): Tree
//  }

  /** Tree generation, usually based on existing symbols. */
  override lazy val gen: AstTreeGen {
    val global: ReflectGlobal.this.type
    def mkAttributedCast(tree: Tree, pt: Type): Tree
  } = new {
    val global: ReflectGlobal.this.type = ReflectGlobal.this
  } with AstTreeGen {
    def mkAttributedCast(tree: Tree, pt: Type): Tree =
      typer.typed(mkCast(tree, pt))
  }

  //TODO-REFLECT probably here it's better to use object (it's not necessary to override it in compiler)
  val typer: analyzer.Typer
  
////TODO-REFLECT probably here it's better to use object (it's not necessary to override it in compiler)
//  val typer = new analyzer.Typer(
//  analyzer.NoContext.make(EmptyTree, RootClass, newScope)
//){}

  // phaseName = "erasure"  
//  override val erasure: scala.reflect.internal.transform.Erasure {
//    val global: ReflectGlobal.this.type
//  } = null //???

  // phaseName = "erasure"  
  override lazy val erasure = new {
    val global: ReflectGlobal.this.type = ReflectGlobal.this
  } with scala.reflect.internal.transform.Erasure

  val constfold: ConstantFolder {
    val global: ReflectGlobal.this.type
  }

//  val constfold = new {
//    val global: ReflectGlobal.this.type = ReflectGlobal.this
//  } with ConstantFolder

  /** Print tree in detailed form */
  lazy val nodePrinters = new {
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

  def withInfoLevel[T](infolevel: nodePrinters.InfoLevel.Value)(op: => T) = {
    val saved = nodePrinters.infolevel
    try {
      nodePrinters.infolevel = infolevel
      op
    } finally {
      nodePrinters.infolevel = saved
    }
  }

  def isBeforeErasure = phaseId(currentPeriod) < currentRun.erasurePhase.id
  def isBeforeErasure(global: ReflectGlobal) = global.phase.id < global.currentRun.erasurePhase.id
  def isAfterUncurryPhase = false
  def notAfterTyperPhase = true

  def currentUnit: CompilationUnit = if (currentRun eq null) NoCompilationUnit else currentRun.currentUnit

  type PlatformClassPath = ClassPath[AbstractFile]

  def newSourceFile(code: String, filename: String = "<console>") =
    new BatchSourceFile(filename, code)

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
  var printTypings = settings.Ytyperdebug.value

  def logError(msg: String, t: Throwable): Unit = ()
  def signalDone(context: analyzer.Context, old: Tree, result: Tree) {}
  /** Register top level class (called on entering the class)
  */
  def registerTopLevelSym(sym: Symbol) {}

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

  trait RunBase {
    // This change broke sbt; I gave it the thrilling name of uncheckedWarnings0 so
    // as to recover uncheckedWarnings for its ever-fragile compiler interface.
    val deprecationWarnings0 = new ConditionalWarning("deprecation", settings.deprecation)
    val uncheckedWarnings0 = new ConditionalWarning("unchecked", settings.unchecked)
    val featureWarnings = new ConditionalWarning("feature", settings.feature)
    val inlinerWarnings = new ConditionalWarning("inliner", settings.YinlinerWarnings)
    val allConditionalWarnings = List(deprecationWarnings0, uncheckedWarnings0, featureWarnings, inlinerWarnings)

    def uncheckedWarnings: List[(Position, String)] = uncheckedWarnings0.warnings.toList // used in sbt
    def deprecationWarnings: List[(Position, String)] = deprecationWarnings0.warnings.toList // used in sbt
  }
}

//TODO-REFLECT this trait should be reimplemented in compiler with default behaviour from compiler
//TODO-REFLECT - move to Analyzer or macro.runtime?
trait QuasiquotesImpl {
  self: Typechecker =>
  //REFLECT-GLOBAL override in compiler new { val c: c0.type = c0 } with QuasiquoteImpls
  def context2quasiquoteImpl(c0: analyzer.MacroContext): Quasiquotes { val c: c0.type } = ???
}

//TODO-REFLECT this trait should be reimplemented in compiler with default behaviour from compiler
//change self and Context in compiler's Analyzer
//refactor?
//TODO-REFLECT - move to Analyzer or macros.runtime?
trait MacrosImpl {
  self: typechecker.Analyzer =>
  import self.global._
  
  def macroContextImpl(typer: Typer, prefixTree: Tree, expandeeTree: Tree) = {
    new {
      val universe: self.global.type = self.global
      val callsiteTyper: universe.analyzer.Typer = typer.asInstanceOf[global.analyzer.Typer]
      val expandee = universe.analyzer.macroExpanderAttachment(expandeeTree).original orElse duplicateAndKeepPositions(expandeeTree)
    } with scala.reflect.moved.macros.contexts.Context {
      val prefix = Expr[Nothing](prefixTree)(TypeTag.Nothing)
      override def toString = "MacroContext(%s@%s +%d)".format(expandee.symbol.name, expandee.pos, enclosingMacros.length - 1 /* exclude myself */)
    }
  }
}

//TODO-REFLECT add Parsers, Internals, Evals to compiler's Contexts
trait ContextAPIImpl {
  self: scala.reflect.moved.macros.contexts.Context =>
    def eval[T](expr: Expr[T]): T = ???
    lazy val internal: ContextInternalApi = ???
    def parse(code: String): Tree = ??? 
}

trait PatternMatchingImpl extends PatternMatching {
  import global._  
  
  class PureMatchTranslator(override val typer: analyzer.Typer, override val matchStrategy: Tree) extends super.PureMatchTranslator(typer, matchStrategy) {
    override def translateMatch(match_ : Match): Tree = ???
  }
}

trait ScalacPatternExpandersImpl extends ScalacPatternExpanders {
  import global._
  
  override def patternsUnexpandedFormals(sel: Tree, args: List[Tree]) = ???
}

trait TypecheckerImpl extends ReflectGlobal { 
  override object typer extends analyzer.Typer(
    analyzer.NoContext.make(EmptyTree, RootClass, newScope)
  ){}

  override object constfold extends {
    val global: TypecheckerImpl.this.type = TypecheckerImpl.this
  } with ConstantFolder

  override object patmat extends {
	val global: TypecheckerImpl.this.type = TypecheckerImpl.this
  } with PatternMatchingImpl with ScalacPatternExpandersImpl with TreeAndTypeAnalysis{}

  var reporter: Reporter = ???
  def settings: Settings = ???

  var globalPhase: Phase = ???
  def currentRun: Run = ???

  def RootClass: ClassSymbol = ???
  //TODO-REFLECT maybe we don't require such method and its usage
  def enteringTyper[T](op: => T): T = ???
  def registerContext(c: analyzer.Context): Unit = ???
  
  def classPath: PlatformClassPath = ???
  val loaders: ReflectSymbolLoaders = ???

  trait Run extends super[ReflectGlobal].Run {
     /** Have been running into too many init order issues with Run
     *  during erroneous conditions.  Moved all these vals up to the
     *  top of the file so at least they're not trivially null.
     */
    var isDefined: Boolean = ???
    
    /** The currently compiled unit; set from GlobalPhase */
    var currentUnit: CompilationUnit = ???
    
    /** A map from compiled top-level symbols to their source files */
    val symSource: mutable.HashMap[Symbol, AbstractFile] = ???
    val compiledFiles: mutable.HashSet[String] = ???
    val runDefinitions: definitions.RunDefinitions = ???
    
    def canRedefine(sym: Symbol): Boolean = ???
    def compiles(sym: Symbol): Boolean = ???
    var reportedFeature: Set[Symbol] = ???

    val typerPhase: Phase = ???
    val erasurePhase: Phase = ???
    
    var seenMacroExpansionsFallingBack: Boolean = ???
  }
}
//TODO-REFLECT - field required for Typechecker instantiation
//[locker.reflect] /** As seen from class TypecheckerImpl, the missing signatures are as follows.
//[locker.reflect]  *  For convenience, these are usable as stub implementations.
//[locker.reflect]  */
//[locker.reflect]   // Members declared in scala.reflect.internal.FreshNames
//[locker.reflect]   def currentFreshNameCreator: scala.reflect.internal.util.FreshNameCreator = ???
//[locker.reflect] 
//[locker.reflect]   // Members declared in scala.reflect.api.ImplicitTags
//[locker.reflect]   implicit val MirrorTag: (=> scala.reflect.ClassTag[_1899.Mirror]) forSome { val _1899: scala.reflect.internal.tools.nsc.TypecheckerImpl } = ???
//[locker.reflect]   implicit val RuntimeClassTag: (=> scala.reflect.ClassTag[_1900.RuntimeClass]) forSome { val _1900: scala.reflect.internal.tools.nsc.TypecheckerImpl } = ???
//[locker.reflect]   implicit val TreeCopierTag: (=> scala.reflect.ClassTag[_1901.TreeCopier]) forSome { val _1901: scala.reflect.internal.tools.nsc.TypecheckerImpl } = ???
//[locker.reflect] 
//[locker.reflect]   // Members declared in scala.reflect.api.Mirrors
//[locker.reflect]   val rootMirror: scala.reflect.internal.tools.nsc.TypecheckerImpl#Mirror = ???
//[locker.reflect] 
//[locker.reflect]   // Members declared in scala.reflect.internal.Required
//[locker.reflect]   def erasurePhase: scala.reflect.internal.Phase = ???
//[locker.reflect]   def picklerPhase: scala.reflect.internal.Phase = ???
//[locker.reflect] 
//[locker.reflect]   // Members declared in scala.reflect.internal.SymbolTable
//[locker.reflect]   def currentRunId: Int = ???
//[locker.reflect]   def log(msg: => AnyRef): Unit = ???
//[locker.reflect]   def mirrorThatLoaded: ((sym: _1904.Symbol)_1904.Mirror) forSome { val _1904: scala.reflect.internal.tools.nsc.TypecheckerImpl } = ???
//[locker.reflect]   val phaseWithId: Array[scala.reflect.internal.Phase] = ???
//[locker.reflect]   val treeInfo: scala.reflect.internal.tools.nsc.ast.TreeInfo{val global: scala.reflect.internal.tools.nsc.TypecheckerImpl} = ???