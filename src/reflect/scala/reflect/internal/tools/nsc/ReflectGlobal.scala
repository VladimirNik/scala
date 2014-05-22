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

trait ReflectGlobal extends ReflectMix {
  var reporter: Reporter
  var currentSettings: Settings
  var globalPhase: Phase
  override def settings = currentSettings
  def currentRun: Run
  
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
  
  //TODO: Run from Global
  class Run extends RunContextApi {
    /** Have been running into too many init order issues with Run
     *  during erroneous conditions.  Moved all these vals up to the
     *  top of the file so at least they're not trivially null.
     */
    var isDefined = false
    /** The currently compiled unit; set from GlobalPhase */
    var currentUnit: CompilationUnit = NoCompilationUnit

    // This change broke sbt; I gave it the thrilling name of uncheckedWarnings0 so
    // as to recover uncheckedWarnings for its ever-fragile compiler interface.
    val deprecationWarnings0 = new ConditionalWarning("deprecation", settings.deprecation)
    val uncheckedWarnings0 = new ConditionalWarning("unchecked", settings.unchecked)
    val featureWarnings = new ConditionalWarning("feature", settings.feature)
    val inlinerWarnings = new ConditionalWarning("inliner", settings.YinlinerWarnings)
    val allConditionalWarnings = List(deprecationWarnings0, uncheckedWarnings0, featureWarnings, inlinerWarnings)

    def uncheckedWarnings: List[(Position, String)] = uncheckedWarnings0.warnings.toList // used in sbt
    def deprecationWarnings: List[(Position, String)] = deprecationWarnings0.warnings.toList // used in sbt

    var reportedFeature = Set[Symbol]()

    /** Has any macro expansion used a fallback during this run? */
    var seenMacroExpansionsFallingBack = false

    /** Have we already supplemented the error message of a compiler crash? */
    private[nsc] final var supplementedError = false

    private class SyncedCompilationBuffer { self =>
      private val underlying = new mutable.ArrayBuffer[CompilationUnit]
      def size = synchronized { underlying.size }
      def +=(cu: CompilationUnit): this.type = { synchronized { underlying += cu }; this }
      def head: CompilationUnit = synchronized{ underlying.head }
      def apply(i: Int): CompilationUnit = synchronized { underlying(i) }
      def iterator: Iterator[CompilationUnit] = new collection.AbstractIterator[CompilationUnit] {
        private var used = 0
        def hasNext = self.synchronized{ used < underlying.size }
        def next = self.synchronized {
          if (!hasNext) throw new NoSuchElementException("next on empty Iterator")
          used += 1
          underlying(used-1)
        }
      }
      def toList: List[CompilationUnit] = synchronized{ underlying.toList }
    }

    private val unitbuf = new SyncedCompilationBuffer

    val compiledFiles   = new mutable.HashSet[String]

    /** A map from compiled top-level symbols to their source files */
    val symSource = new mutable.HashMap[Symbol, AbstractFile]

    /** A map from compiled top-level symbols to their picklers */
    val symData = new mutable.HashMap[Symbol, PickleBuffer]

    private var phasec: Int  = 0   // phases completed
    private var unitc: Int   = 0   // units completed this phase

    def size = unitbuf.size
    override def toString = "scalac Run for:\n  " + compiledFiles.toList.sorted.mkString("\n  ")

    // Calculate where to stop based on settings -Ystop-before or -Ystop-after.
    // The result is the phase to stop at BEFORE running it.
    private lazy val stopPhaseSetting = {
      def isBefore(pd: SubComponent) = settings.stopBefore contains pd.phaseName
      phaseDescriptors sliding 2 collectFirst {
        case xs if xs exists isBefore
                => (xs find isBefore).get
        case xs if settings.stopAfter contains xs.head.phaseName
                => xs.last
      }
    }
    /** Should we stop right before entering the given phase? */
    protected def stopPhase(name: String) = stopPhaseSetting exists (_.phaseName == name)
    /** Should we skip the given phase? */
    protected def skipPhase(name: String) = settings.skip contains name

    private val firstPhase = {
      // Initialization.  definitions.init requires phase != NoPhase
      import scala.reflect.internal.SomePhase
      curRunId += 1
      curRun = this
      phase = SomePhase
      phaseWithId(phase.id) = phase
      definitions.init()

      // the components to use, omitting those named by -Yskip and stopping at the -Ystop phase
      val components = {
        // stop on a dime, but this test fails if pd is after the stop phase
        def unstoppable(pd: SubComponent) = {
          val stoppable = stopPhase(pd.phaseName)
          if (stoppable && pd.initial) {
            globalError(s"Cannot stop before initial phase '${pd.phaseName}'.")
            true
          } else
            !stoppable
        }
        // skip a component for -Yskip or if not enabled
        def skippable(pd: SubComponent) = {
          val skippable = skipPhase(pd.phaseName)
          if (skippable && (pd.initial || pd.terminal)) {
            globalError(s"Cannot skip an initial or terminal phase '${pd.phaseName}'.")
            false
          } else
            skippable || !pd.enabled
        }
        val phs = phaseDescriptors takeWhile unstoppable filterNot skippable
        // Ensure there is a terminal phase at the end, since -Ystop may have limited the phases.
        if (phs.isEmpty || !phs.last.terminal) {
          val t = if (phaseDescriptors.last.terminal) phaseDescriptors.last else terminal
          phs :+ t
        } else phs
      }
      // Create phases and link them together. We supply the previous, and the ctor sets prev.next.
      val last  = components.foldLeft(NoPhase: Phase)((prev, c) => c newPhase prev)
      // rewind (Iterator.iterate(last)(_.prev) dropWhile (_.prev ne NoPhase)).next
      val first = { var p = last ; while (p.prev ne NoPhase) p = p.prev ; p }
      val ss    = settings

      // As a final courtesy, see if the settings make any sense at all.
      // If a setting selects no phase, it's a mistake. If a name prefix
      // doesn't select a unique phase, that might be surprising too.
      def checkPhaseSettings(including: Boolean, specs: Seq[String]*) = {
        def isRange(s: String) = s.forall(c => c.isDigit || c == '-')
        def isSpecial(s: String) = (s == "all" || isRange(s))
        val setting = new ss.PhasesSetting("fake","fake")
        for (p <- specs.flatten.to[Set]) {
          setting.value = List(p)
          val count = (
            if (including) first.iterator count (setting containsPhase _)
            else phaseDescriptors count (setting contains _.phaseName)
          )
          if (count == 0) warning(s"'$p' specifies no phase")
          if (count > 1 && !isSpecial(p)) warning(s"'$p' selects $count phases")
          if (!including && isSpecial(p)) globalError(s"-Yskip and -Ystop values must name phases: '$p'")
          setting.clear()
        }
      }
      // phases that are excluded; for historical reasons, these settings only select by phase name
      val exclusions = List(ss.stopBefore, ss.stopAfter, ss.skip)
      val inclusions = ss.visibleSettings collect {
        case s: ss.PhasesSetting if !(exclusions contains s) => s.value
      }
      checkPhaseSettings(including = true, inclusions.toSeq: _*)
      checkPhaseSettings(including = false, exclusions map (_.value): _*)

      phase = first   //parserPhase
      first
    }

    /** Reset all classes contained in current project, as determined by
     *  the clearOnNextRun hook
     */
    @deprecated("use invalidateClassPathEntries instead", "2.10.0")
    def resetProjectClasses(root: Symbol): Unit = try {
      def unlink(sym: Symbol) =
        if (sym != NoSymbol) root.info.decls.unlink(sym)
      if (settings.verbose) inform("[reset] recursing in "+root)
      val toReload = mutable.Set[String]()
      for (sym <- root.info.decls) {
        if (sym.isInitialized && clearOnNextRun(sym))
          if (sym.hasPackageFlag) {
            resetProjectClasses(sym.moduleClass)
            openPackageModule(sym.moduleClass)
          } else {
            unlink(sym)
            unlink(root.info.decls.lookup(
              if (sym.isTerm) sym.name.toTypeName else sym.name.toTermName))
            toReload += sym.fullName
              // note: toReload could be set twice with the same name
              // but reinit must happen only once per name. That's why
              // the following classPath.findClass { ... } code cannot be moved here.
          }
      }
      for (fullname <- toReload)
        classPath.findClass(fullname) match {
          case Some(classRep) =>
            if (settings.verbose) inform("[reset] reinit "+fullname)
            loaders.initializeFromClassPath(root, classRep)
          case _ =>
        }
    } catch {
      case ex: Throwable =>
        // this handler should not be nessasary, but it seems that `fsc`
        // eats exceptions if they appear here. Need to find out the cause for
        // this and fix it.
        inform("[reset] exception happened: "+ex)
        ex.printStackTrace()
        throw ex
    }

    // --------------- Miscellania -------------------------------

    /** Progress tracking.  Measured in "progress units" which are 1 per
     *  compilation unit per phase completed.
     *
     *  @param    current   number of "progress units" completed
     *  @param    total     total number of "progress units" in run
     */
    def progress(current: Int, total: Int) {}

    /**
     * For subclasses to override. Called when `phase` is about to be run on `unit`.
     * Variables are passed explicitly to indicate that `globalPhase` and `currentUnit` have been set.
     */
    def informUnitStarting(phase: Phase, unit: CompilationUnit) { }

    /** take note that phase is completed
     *  (for progress reporting)
     */
    def advancePhase() {
      unitc = 0
      phasec += 1
      refreshProgress()
    }
    /** take note that a phase on a unit is completed
     *  (for progress reporting)
     */
    def advanceUnit() {
      unitc += 1
      refreshProgress()
    }

    def cancel() { reporter.cancelled = true }

    private def currentProgress   = (phasec * size) + unitc
    private def totalProgress     = (phaseDescriptors.size - 1) * size // -1: drops terminal phase
    private def refreshProgress() = if (size > 0) progress(currentProgress, totalProgress)

    // ----- finding phases --------------------------------------------

    def phaseNamed(name: String): Phase =
      findOrElse(firstPhase.iterator)(_.name == name)(NoPhase)

    /** All phases as of 3/2012 here for handiness; the ones in
     *  active use uncommented.
     */
    val parserPhase                  = phaseNamed("parser")
    val namerPhase                   = phaseNamed("namer")
    // val packageobjectsPhase          = phaseNamed("packageobjects")
    val typerPhase                   = phaseNamed("typer")
    // val inlineclassesPhase           = phaseNamed("inlineclasses")
    // val superaccessorsPhase          = phaseNamed("superaccessors")
    val picklerPhase                 = phaseNamed("pickler")
    val refchecksPhase               = phaseNamed("refchecks")
    // val selectiveanfPhase            = phaseNamed("selectiveanf")
    // val selectivecpsPhase            = phaseNamed("selectivecps")
    val uncurryPhase                 = phaseNamed("uncurry")
    // val tailcallsPhase               = phaseNamed("tailcalls")
    val specializePhase              = phaseNamed("specialize")
    val explicitouterPhase           = phaseNamed("explicitouter")
    val erasurePhase                 = phaseNamed("erasure")
    val posterasurePhase             = phaseNamed("posterasure")
    // val lazyvalsPhase                = phaseNamed("lazyvals")
    // val lambdaliftPhase              = phaseNamed("lambdalift")
    // val constructorsPhase            = phaseNamed("constructors")
    val flattenPhase                 = phaseNamed("flatten")
    val mixinPhase                   = phaseNamed("mixin")
    val delambdafyPhase              = phaseNamed("delambdafy")
    val cleanupPhase                 = phaseNamed("cleanup")
    val icodePhase                   = phaseNamed("icode")
    val inlinerPhase                 = phaseNamed("inliner")
    val inlineExceptionHandlersPhase = phaseNamed("inlinehandlers")
    val closelimPhase                = phaseNamed("closelim")
    val dcePhase                     = phaseNamed("dce")
    // val jvmPhase                     = phaseNamed("jvm")

    def runIsAt(ph: Phase)   = globalPhase.id == ph.id
    def runIsAtOptimiz       = {
      runIsAt(inlinerPhase)                 || // listing phases in full for robustness when -Ystop-after has been given.
      runIsAt(inlineExceptionHandlersPhase) ||
      runIsAt(closelimPhase)                ||
      runIsAt(dcePhase)
    }

    isDefined = true

    // ----------- Units and top-level classes and objects --------


    /** add unit to be compiled in this run */
    private def addUnit(unit: CompilationUnit) {
      unitbuf += unit
      compiledFiles += unit.source.file.path
    }
    private def checkDeprecatedSettings(unit: CompilationUnit) {
      // issue warnings for any usage of deprecated settings
      settings.userSetSettings filter (_.isDeprecated) foreach { s =>
        unit.deprecationWarning(NoPosition, s.name + " is deprecated: " + s.deprecationMessage.get)
      }
      if (settings.target.value.contains("jvm-1.5"))
        unit.deprecationWarning(NoPosition, settings.target.name + ":" + settings.target.value + " is deprecated: use target for Java 1.6 or above.")
    }

    /* An iterator returning all the units being compiled in this run */
    /* !!! Note: changing this to unitbuf.toList.iterator breaks a bunch
       of tests in tests/res.  This is bad, it means the resident compiler
       relies on an iterator of a mutable data structure reflecting changes
       made to the underlying structure.
     */
    def units: Iterator[CompilationUnit] = unitbuf.iterator

    def registerPickle(sym: Symbol): Unit = ()

    /** does this run compile given class, module, or case factory? */
    // NOTE: Early initialized members temporarily typechecked before the enclosing class, see typedPrimaryConstrBody!
    //       Here we work around that wrinkle by claiming that a top-level, early-initialized member is compiled in
    //       *every* run. This approximation works because this method is exclusively called with `this` == `currentRun`.
    def compiles(sym: Symbol): Boolean =
      if (sym == NoSymbol) false
      else if (symSource.isDefinedAt(sym)) true
      else if (sym.isTopLevel && sym.isEarlyInitialized) true
      else if (!sym.isTopLevel) compiles(sym.enclosingTopLevelClass)
      else if (sym.isModuleClass) compiles(sym.sourceModule)
      else false

    /** Is this run allowed to redefine the given symbol? Usually this is true
     *  if the run does not already compile `sym`, but for interactive mode
     *  we have a more liberal interpretation.
     */
    def canRedefine(sym: Symbol) = !compiles(sym)

    // --------------- Compilation methods ----------------------------

    protected def runCheckers() {
      val toCheck  = globalPhase.prev
      val canCheck = toCheck.checkable
      val fmt      = if (canCheck) "[Now checking: %s]" else "[Not checkable: %s]"

      inform(fmt format toCheck.name)

      if (canCheck) {
        phase = globalPhase
        if (globalPhase.id >= icodePhase.id) icodeChecker.checkICodes()
        else treeChecker.checkTrees()
      }
    }

    private def showMembers() = {
      // Allows for syntax like scalac -Xshow-class Random@erasure,typer
      def splitClassAndPhase(str: String, term: Boolean): Name = {
        def mkName(s: String) = if (term) newTermName(s) else newTypeName(s)
        (str indexOf '@') match {
          case -1   => mkName(str)
          case idx  =>
            val phasePart = str drop (idx + 1)
            settings.Yshow.tryToSetColon(phasePart split ',' toList)
            mkName(str take idx)
        }
      }
      if (settings.Xshowcls.isSetByUser)
        showDef(splitClassAndPhase(settings.Xshowcls.value, term = false), declsOnly = false, globalPhase)

      if (settings.Xshowobj.isSetByUser)
        showDef(splitClassAndPhase(settings.Xshowobj.value, term = true), declsOnly = false, globalPhase)
    }

    // Similarly, this will only be created under -Yshow-syms.
    object trackerFactory extends SymbolTrackers {
      val global: Global.this.type = Global.this
      lazy val trackers = currentRun.units.toList map (x => SymbolTracker(x))
      def snapshot() = {
        inform("\n[[symbol layout at end of " + phase + "]]")
        exitingPhase(phase) {
          trackers foreach { t =>
            t.snapshot()
            inform(t.show("Heading from " + phase.prev.name + " to " + phase.name))
          }
        }
      }
    }

    def reportCompileErrors() {
      if (!reporter.hasErrors && reporter.hasWarnings && settings.fatalWarnings)
        globalError("No warnings can be incurred under -Xfatal-warnings.")

      if (reporter.hasErrors) {
        for ((sym, file) <- symSource.iterator) {
          sym.reset(new loaders.SourcefileLoader(file))
          if (sym.isTerm)
            sym.moduleClass reset loaders.moduleClassLoader
        }
      }
      else {
        allConditionalWarnings foreach (_.summarize())

        if (seenMacroExpansionsFallingBack)
          warning("some macros could not be expanded and code fell back to overridden methods;"+
                  "\nrecompiling with generated classfiles on the classpath might help.")
        // todo: migrationWarnings
      }
    }

    /** Caching member symbols that are def-s in Defintions because they might change from Run to Run. */
    val runDefinitions: definitions.RunDefinitions = new definitions.RunDefinitions

    /** Compile list of source files,
     *  unless there is a problem already,
     *  such as a plugin was passed a bad option.
     */
    def compileSources(sources: List[SourceFile]) = if (!reporter.hasErrors) {

      def checkDeprecations() = {
        checkDeprecatedSettings(newCompilationUnit(""))
        reportCompileErrors()
      }

      val units = sources map scripted map (new CompilationUnit(_))

      units match {
        case Nil => checkDeprecations()   // nothing to compile, report deprecated options
        case _   => compileUnits(units, firstPhase)
      }
    }

    def compileUnits(units: List[CompilationUnit], fromPhase: Phase): Unit =
      compileUnitsInternal(units, fromPhase)

    private def compileUnitsInternal(units: List[CompilationUnit], fromPhase: Phase) {
      doInvalidation()

      units foreach addUnit
      val startTime = currentTime

      reporter.reset()
      checkDeprecatedSettings(unitbuf.head)
      globalPhase = fromPhase

     while (globalPhase.hasNext && !reporter.hasErrors) {
        val startTime = currentTime
        phase = globalPhase
        globalPhase.run()

        // progress update
        informTime(globalPhase.description, startTime)
        val shouldWriteIcode = (
             (settings.writeICode.isSetByUser && (settings.writeICode containsPhase globalPhase))
          || (!settings.Xprint.doAllPhases && (settings.Xprint containsPhase globalPhase) && runIsAtOptimiz)
        )
        if (shouldWriteIcode) {
          // Write *.icode files when -Xprint-icode or -Xprint:<some-optimiz-phase> was given.
          writeICode()
        } else if ((settings.Xprint containsPhase globalPhase) || settings.printLate && runIsAt(cleanupPhase)) {
          // print trees
          if (settings.Xshowtrees || settings.XshowtreesCompact || settings.XshowtreesStringified) nodePrinters.printAll()
          else printAllUnits()
        }

        // print the symbols presently attached to AST nodes
        if (settings.Yshowsyms)
          trackerFactory.snapshot()

        // print members
        if (settings.Yshow containsPhase globalPhase)
          showMembers()

        // browse trees with swing tree viewer
        if (settings.browse containsPhase globalPhase)
          treeBrowser browse (phase.name, units)

        // move the pointer
        globalPhase = globalPhase.next

        // run tree/icode checkers
        if (settings.check containsPhase globalPhase.prev)
          runCheckers()

        // output collected statistics
        if (settings.Ystatistics)
          statistics.print(phase)

        advancePhase()
      }

      if (traceSymbolActivity)
        units map (_.body) foreach (traceSymbols recordSymbolsInTree _)

      // In case no phase was specified for -Xshow-class/object, show it now for sure.
      if (settings.Yshow.isDefault)
        showMembers()

      reportCompileErrors()
      symSource.keys foreach (x => resetPackageClass(x.owner))
      informTime("total", startTime)

      // Clear any sets or maps created via perRunCaches.
      perRunCaches.clearAll()

      // Reset project
      if (!stopPhase("namer")) {
        enteringPhase(namerPhase) {
          resetProjectClasses(RootClass)
        }
      }
    }

    /** Compile list of abstract files. */
    def compileFiles(files: List[AbstractFile]) {
      try compileSources(files map getSourceFile)
      catch { case ex: IOException => globalError(ex.getMessage()) }
    }

    /** Compile list of files given by their names */
    def compile(filenames: List[String]) {
      try {
        val sources: List[SourceFile] =
          if (settings.script.isSetByUser && filenames.size > 1) returning(Nil)(_ => globalError("can only compile one script at a time"))
          else filenames map getSourceFile

        compileSources(sources)
      }
      catch { case ex: IOException => globalError(ex.getMessage()) }
    }

    /** If this compilation is scripted, convert the source to a script source. */
    private def scripted(s: SourceFile) = s match {
      case b: BatchSourceFile if settings.script.isSetByUser => ScriptSourceFile(b)
      case _ => s
    }

    /** Compile abstract file until `globalPhase`, but at least
     *  to phase "namer".
     */
    def compileLate(file: AbstractFile) {
      if (!compiledFiles(file.path))
        compileLate(new CompilationUnit(scripted(getSourceFile(file))))
    }

    /** Compile abstract file until `globalPhase`, but at least to phase "namer".
     */
    def compileLate(unit: CompilationUnit) {
      addUnit(unit)

      if (firstPhase ne null) { // we might get here during initialization, is a source is newer than the binary
        val maxId = math.max(globalPhase.id, typerPhase.id)
        firstPhase.iterator takeWhile (_.id < maxId) foreach (ph =>
          enteringPhase(ph)(ph.asInstanceOf[GlobalPhase] applyPhase unit))
        refreshProgress()
      }
    }

    /** Reset package class to state at typer (not sure what this
     *  is needed for?)
     */
    private def resetPackageClass(pclazz: Symbol) {
      enteringPhase(firstPhase) {
        pclazz.setInfo(enteringPhase(typerPhase)(pclazz.info))
      }
      if (!pclazz.isRoot) resetPackageClass(pclazz.owner)
    }
  } // class Run
}

trait ReflectMix extends SymbolTable
    with ast.Trees
    with Printers
    with ast.DocComments
    with Positions 
    with CompilationUnits {
  self: ReflectGlobal =>
}
    
//TODO: differences with Global
//    with CompilationUnits
//    with Plugins
//    with PhaseAssembly