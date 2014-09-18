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
import scala.tools.nsc.reporters.Reporter
import scala.reflect.internal.tools.nsc.ast.{TreeGen => AstTreeGen}
import scala.tools.nsc.util.ClassPath
import scala.reflect.internal.tools.nsc.typechecker.ConstantFolder
import scala.reflect.internal.transform.Erasure
import scala.reflect.internal.tools.nsc.transform._
import scala.reflect.internal.tools.nsc.transform.patmat._
import scala.reflect.internal.tools.reflect.quasiquotes.Quasiquotes
import scala.reflect.runtime. {ReflectSetup, JavaUniverse}
import scala.reflect.api.Universe
import scala.tools.util.PathResolver
import scala.tools.nsc.Settings
import scala.collection.mutable.WeakHashMap
import scala.reflect.runtime.JavaMirrors

trait Typechecker extends SymbolTable
    with Printers
    with Positions 
    with CompilationUnits
    with QuasiquotesImpl {
  self: TypecheckerRequirements =>

  val analyzer: typechecker.Analyzer {
    val global: Typechecker.this.type
  }

  val patmat: PatternMatching with ScalacPatternExpanders with TreeAndTypeAnalysis {
    val global: Typechecker.this.type
  }

  var reporter: Reporter
  def settings: Settings

  def currentRun: Run

  def registerContext(c: analyzer.Context): Unit
  def classPath: PlatformClassPath

  val loaders: ReflectSymbolLoaders
  def RootClass: ClassSymbol
  //TODO-REFLECT maybe we don't require such method here
  def enteringTyper[T](op: => T): T

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

    //TODO-REFLECT required only for compatibility in Reflect, try to remove
    val erasurePhase: Phase
    var seenMacroExpansionsFallingBack: Boolean
  }

  def isBeforeErasure: Boolean
  def isBeforeErasure(global: TypecheckerRequirements): Boolean
  def isAtPhaseAfterUncurryPhase: Boolean
  def notAfterTyperPhase: Boolean
  def notAfterTyper: Boolean
}

trait TypecheckerRequirements extends Typechecker {
  lazy val analyzer: typechecker.Analyzer {
    val global: TypecheckerRequirements.this.type
  } = new {
    val global: TypecheckerRequirements.this.type = TypecheckerRequirements.this
  } with typechecker.Analyzer

  //TODO-REFLECT try to remove mkAttributedCast - it's required only for compiler
  /** Tree generation, usually based on existing symbols. */
  override lazy val gen: AstTreeGen {
    val global: TypecheckerRequirements.this.type
    def mkAttributedCast(tree: Tree, pt: Type): Tree
  } = new {
    val global: TypecheckerRequirements.this.type = TypecheckerRequirements.this
  } with AstTreeGen {
    def mkAttributedCast(tree: Tree, pt: Type): Tree =
      typer.typed(mkCast(tree, pt))
  }

  //TODO-REFLECT probably it's better to use object here (it's not necessary to override it in the compiler)
  val typer: analyzer.Typer
  
////TODO-REFLECT probably here it's better to use object (it's not necessary to override it in compiler)
//  val typer = new analyzer.Typer(
//  analyzer.NoContext.make(EmptyTree, RootClass, newScope)
//){}

  // phaseName = "erasure"  
//  override val erasure: scala.reflect.internal.transform.Erasure {
//    val global: TypecheckerRequirements.this.type
//  } = null //???

  // phaseName = "erasure"  
  override lazy val erasure = new {
    val global: TypecheckerRequirements.this.type = TypecheckerRequirements.this
  } with scala.reflect.internal.transform.Erasure

  val constfold: ConstantFolder {
    val global: TypecheckerRequirements.this.type
  }

//  val constfold = new {
//    val global: TypecheckerRequirements.this.type = TypecheckerRequirements.this
//  } with ConstantFolder

  //TODO-REFLECT same code is in Global
  /** Print tree in detailed form */
  lazy val nodePrinters = new {
    val global: TypecheckerRequirements.this.type = TypecheckerRequirements.this
  } with ast.NodePrinters {
    var lastPrintedPhase: Phase = NoPhase
    var lastPrintedSource: String = ""
    infolevel = InfoLevel.Verbose

    def showUnit(unit: CompilationUnit) {
      print(" // " + unit.source)
      if (unit.body == null) println(": tree is null")
      else {
        val source = scala.tools.nsc.util.stringFromWriter(w => newTreePrinter(w) print unit.body)

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

  protected var lastSeenContext: analyzer.Context = null

  /** Register new context; called for every created context
   */
  def registerContext(c: analyzer.Context) {
    lastSeenContext = c
  }

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

trait TypecheckerApi {
  self: Universe =>
  def typecheck(tree: Tree, mirror: Mirror): Tree
  //TODO-REFLECT: remove method, added for simplicity
  def typecheck(tree: Tree, classLoader: ClassLoader): Tree = ???
}

//class RuntimeTypechecker extends JavaUniverse with TypecheckerRequirements with TypecheckerApi {
trait RuntimeTypechecker extends TypecheckerRequirements {
  self: JavaMirrors =>
  import analyzer._

  override object typer extends analyzer.Typer(
    analyzer.NoContext.make(EmptyTree, RootClass, newScope)
  ){}

  override object constfold extends {
    val global: RuntimeTypechecker.this.type = RuntimeTypechecker.this
  } with ConstantFolder

  override object patmat extends {
	val global: RuntimeTypechecker.this.type = RuntimeTypechecker.this
  } with PatternMatchingImpl with ScalacPatternExpanders with TreeAndTypeAnalysis{}

  //TODO-REFLECT: reporter is implemented in JavaUniverse, try to remove it
  var reporter: Reporter = new Reporter {
    protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean): Unit = () //???
  }

  //TODO-REFLECT settings implementation is moved to JavaUniverse
  //override lazy val settings: Settings = new Settings

  def currentRun: Run = new Run{}

  def classPath: PlatformClassPath = new PathResolver(settings).result

  //TODO-REFLECT fix it to Multiverse support
  def RootClass: ClassSymbol = rootMirror.RootClass

  //TODO-REFLECT maybe we don't require such method here
  def enteringTyper[T](op: => T): T = ???
  lazy val loaders: ReflectSymbolLoaders = ???

  def isBeforeErasure = true
  def isBeforeErasure(global: TypecheckerRequirements) = true
  def isAtPhaseAfterUncurryPhase = false
  def notAfterTyperPhase = true
  def notAfterTyper = true	  

  trait Run extends super[TypecheckerRequirements].Run {
     /** Have been running into too many init order issues with Run
     *  during erroneous conditions.  Moved all these vals up to the
     *  top of the file so at least they're not trivially null.
     */
    var isDefined: Boolean = false //???

    /** The currently compiled unit; set from GlobalPhase */
    var currentUnit: CompilationUnit = NoCompilationUnit //???

    /** A map from compiled top-level symbols to their source files */
    lazy val symSource: mutable.HashMap[Symbol, AbstractFile] = new mutable.HashMap[Symbol, AbstractFile] //??? - TODO-REFLECT - if default value is correct in this case?
    lazy val compiledFiles: mutable.HashSet[String] = ??? //new mutable.HashSet[String]
    lazy val runDefinitions: definitions.RunDefinitions = new definitions.RunDefinitions

    //TODO-REFLECT move to TypecheckerRequirements
    def compiles(sym: Symbol): Boolean = 
      if (sym == NoSymbol) false
      else if (symSource.isDefinedAt(sym)) true
      else if (sym.isTopLevel && sym.isEarlyInitialized) true
      else if (!sym.isTopLevel) compiles(sym.enclosingTopLevelClass)
      else if (sym.isModuleClass) compiles(sym.sourceModule)
      else false

    //TODO-REFLECT move to TypecheckerRequirements
    def canRedefine(sym: Symbol): Boolean = !compiles(sym)
    var reportedFeature: Set[Symbol] = Set[Symbol]() //???

    lazy val erasurePhase: Phase = ???

    var seenMacroExpansionsFallingBack: Boolean = false //???

    def units = ???
  }

  // *** Definitions ***
  //TODO-REFLECT: cache to persist definitions for mirrors used during typecheck
  lazy val definitionsCache = {
    WeakHashMap[Symbol, DefinitionsClass](rootMirror.RootClass -> definitions)
  }
 
//  override lazy val definitions: DefinitionsClass = {
//    System.out.println("<<< RuntimeTypechecker from reflect (definitions) >>>")
////    val value = new DefinitionsClass(rootMirror) {}
//    definitionsCache(rootMirror.RootClass) = super.definitions //value
//    super.definitions
//  }

  def createDefinitions(mirror: Mirror): DefinitionsClass = {
      definitionsCache.getOrElseUpdate(mirror.RootClass, {val defs = new DefinitionsClass(mirror){}; defs.init; defs})
  }
  //TODO-REFLECT-DEFS add checking for passed symbol
  override def definitionsBySym(symbol: Symbol): DefinitionsClass = {
    //TODO-REFLECT-DEFS check result for None symbol (result symbol should be correct) - if enclosing is not found None is returned
    val rootSymbol = if (symbol.isRoot) symbol else symbol.enclosingRootClass
    definitionsCache.getOrElse(rootSymbol, definitions) //TODO-REFLECT: change null to definitions
  }
  override def definitionsByMirror(mirror: Mirror): DefinitionsClass = definitionsCache.getOrElse(mirror.RootClass, definitions) //TODO-REFLECT: change null to definitions

  // *** Mirrors ****
  //TODO-REFLECT: cache to persist definitions for mirrors used during typecheck with rootMirror init
  lazy val mirrorsCache = {
    WeakHashMap[Symbol, Mirror](rootMirror.RootClass -> rootMirror)
  }

//  override lazy val rootMirror = {
//    val value = createMirror(NoSymbol, rootClassLoader)
//    //val value = super.rootMirror //TODO-REFLECT unfortunately super may be not be used on lazy value rootMirror
//    mirrorsCache(value.RootClass) = value
//    value
//  }
  def addMirror(mirror: Mirror) = mirrorsCache.getOrElseUpdate(mirror.RootClass, mirror)
  override def reflectMirror(symbol: Symbol) = {
    val rootSymbol = if (symbol.isRoot) symbol else symbol.enclosingRootClass
    mirrorsCache.getOrElse(rootSymbol, rootMirror) //TODO-REFLECT: change null to rootMirror (as default value)
  }

  //TODO-REFLECT: required for infos init (in Symbols)
  override def isCompilerUniverse = false

  override def isReflectTypechecker = true

  //typecheck method
  override def typecheck(tree: Tree, mirror: Mirror) = {
    //create definitions
    //println("*** Multiverse testing: ***")
    //println(s"isCompilerUniverse: $isCompilerUniverse")
    //println(s"isReflectTypechecer: $isReflectTypechecker")
//    val myDefs = createDefinitions(mirror)
//    addMirror(mirror)
    
    val mirrorsCount = mirrorsCache.size
    val definitionsCount = definitionsCache.size

    //println(s"myDefs == mirror.definitions: ${myDefs == definitions}")
    //println(s"mirrorsCount: $mirrorsCount")
    //println(s"definitionsCount: $definitionsCount")
    //println("*** *** ***")
    val newTree = tree.duplicate
    val compUnit = new CompilationUnit(NoSourceFile)
    compUnit.body = newTree
    val context = typecheckerContext(mirror)
    val namer = newNamer(context)
    val newCont = namer.enterSym(newTree)
    val typer = newTyper(newCont)
    typer.typed(newTree)
  }

  //TODO-REFLECT remove method, added for simplicity
  override def typecheck(tree: Tree, classLoader: ClassLoader): Tree = typecheck(tree, runtimeMirror(classLoader))
}