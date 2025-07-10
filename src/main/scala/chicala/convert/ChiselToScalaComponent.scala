package chicala.convert

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.PluginComponent

import java.io._

import chicala.util.Format
import chicala.convert.frontend.Scala2Reader
import chicala.util.Printer
import chicala.convert.backend.stainless.StainlessEmitter
import chicala.convert.pass._
import chicala.ChicalaConfig

object ChiselToScalaComponent {
  val phaseName = "chiselToScala"
}

class ChiselToScalaComponent(val global: Global) extends PluginComponent {
  import global._

  val runsAfter: List[String] = List("typer")

  override val runsBefore: List[String] = List(
    // to keep recursive structure
    "tailcalls",
    // make sure run before chisel plugin
    "chiselbundlephase",
    "chiselcomponent"
  )

  val phaseName: String = ChiselToScalaComponent.phaseName

  def newPhase(_prev: Phase) = new ChiselToScalaPhase(_prev)

  class ChiselToScalaPhase(prev: Phase)
      extends StdPhase(prev)
      with Scala2Reader
      with ChicalaPassCollecttion
      with StainlessEmitter
      with Format {
    lazy val global: ChiselToScalaComponent.this.global.type = ChiselToScalaComponent.this.global

    val testRunDir = new File("test_run_dir/" + phaseName)
    testRunDir.mkdirs()

    val chicalaLog = new BufferedWriter(new PrintWriter(testRunDir.getPath() + "/chicala_log.txt"))
    global.computePhaseAssembly().foreach(s => chicalaLog.write(s.toString + "\n"))
    chicalaLog.close()

    var readerInfo: ReaderInfo = ReaderInfo.empty

    override def run(): Unit = {
      super.run()
      processTodos()
      readerInfo.todos.foreach { p => reporter.error(p.tree.pos, "This class not processed") }
    }

    def apply(unit: CompilationUnit): Unit = {
      val packageDef  = unit.body.asInstanceOf[PackageDef]
      val packageName = packageDef.pid.toString()

      for (
        tree @ ClassDef(mods, name, tparams, Template(parents, self, body)) <- packageDef.stats
        if (ChicalaConfig.whitelist.isEmpty || ChicalaConfig.whitelist.contains(s"$packageName.$name"))
      ) {
        inform(s"chicala processing $packageName.$name")
        applyOnTree(tree, packageName)
      }
    }

    def applyOnTree(tr: Tree, packageName: String): Unit = {
      val packageDir = s"${testRunDir.getPath()}/test/${packageName.replace(".", "/")}"
      val outputDir  = s"${testRunDir.getPath()}/out/${packageName.replace(".", "/")}"
      (new File(packageDir)).mkdirs()
      (new File(outputDir)).mkdirs()

      tr match {
        case tree @ ClassDef(mods, name, tparams, Template(parents, self, body)) =>
          Format.saveToFile(
            packageDir + s"/${name}.scala",
            show(tree) + "\n"
          )
          Format.saveToFile(
            packageDir + s"/${name}.AST.scala",
            showFormattedRaw(tree) + "\n"
          )

          val eitherDef = CClassDefLoader(tree, packageName)(readerInfo)

          eitherDef match {
            case Left(Failed) =>
              reporter.error(tree.pos, "Unknown error in ChiselToScalaPhase #1")
            case Left(DependentClassNotDef(depenName)) =>
              inform(s"Dependent Class not defined: $depenName, process later")
              readerInfo = readerInfo.addedTodo(
                ProcessLater(tree, packageName, s"$packageName.$name", depenName)
              )

            case Right(cClassDef) => {
              Format.saveToFile(
                packageDir + s"/${name}.chicala.0.scala",
                cClassDef.toString + "\n"
              )

              val sortedCClassDef = cClassDef match {
                case m @ ModuleDef(name, info, body, pkg) =>
                  // save the original module definition
                  readerInfo = readerInfo.addedModuleDef(m)

                  val sorted = RunChicalaPass(
                    m,
                    List(
                      LiteralPropagation,
                      RegEnableApply,
                      ChicalaPeek(packageDir, "1.beforeExpandSubModuleDef"),
                      ExpandSubModuleDef,
                      ChicalaPeek(packageDir, "2.beforeSv2Simp"),
                      Sv2ChiselSimplify,
                      ChicalaPeek(packageDir, "3.beforeUseVecOnly"),
                      UseVecOnly,
                      ChicalaPeek(packageDir, "4.beforeReduceAsTypeOf"),
                      ReduceAsTypeOf,
                      ChicalaPeek(packageDir, "5.beforeSort"),
                      DependencySort,
                      ChicalaPeek(packageDir, "6.sorted"),
                      AfterSort,
                      UseRecursiveFunc,
                      UseNestedCat,
                      BeforeEmitScala,
                      ChicalaPeek(packageDir, "7.beforeEmit")
                    )
                  )
                  sorted
                case b: BundleDef =>
                  readerInfo = readerInfo.addedBundleDef(b)
                  val passed = RunChicalaPass(
                    b,
                    List(UseVecOnly)
                  )
                  passed
              }

              sortedCClassDef match {
                case m: ModuleDef =>
                  if (ChicalaConfig.simulation == false)
                    Format.saveToFile(
                      outputDir + s"/${name}.stainless.scala",
                      EmitStainless(m)
                    )
                  else
                    Format.saveToFile(
                      outputDir + s"/${name}.simscala.scala",
                      EmitStainless(m)
                    )
                case _ =>
              }
            }
          }
      }
    }

    def processTodos(): Unit = {

      /** simple version of topological sort */
      def sortTodos(todos: List[ProcessLater]): List[ProcessLater] = {
        var needProcess: Set[String] = todos.map(_.fullName).toSet
        var depenBy                  = todos.groupBy(_.depenName)
        var newTodos                 = List.empty[ProcessLater]
        while (depenBy.nonEmpty) {
          var processed = depenBy.keySet -- needProcess
          if (processed.isEmpty) {
            // has circular dependency, just return all todos
            processed = depenBy.keySet
          }
          processed.foreach { name =>
            newTodos ++= depenBy(name)
            needProcess --= depenBy(name).map(_.fullName)
            depenBy -= name
          }
        }
        newTodos
      }

      var lastNum = readerInfo.todos.size + 1
      while (readerInfo.todos.size > 0 && lastNum > readerInfo.todos.size) {
        val todos = sortTodos(readerInfo.todos)
        readerInfo = readerInfo.copy(todos = List.empty)
        lastNum = todos.size
        inform(s"processTodos: ${lastNum} remaining")
        todos.foreach { p =>
          inform(s"processTodos: ${p.tree.asInstanceOf[ClassDef].name} in ${p.packageName}")
          applyOnTree(p.tree, p.packageName)
        }
      }
    }

  }

}
