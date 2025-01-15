package chicala

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

import chicala.convert.ChiselToScalaComponent

object ChicalaPlugin {
  val name: String        = "chicala"
  val description: String = "Convert Chisel to semantically equivalent Scala program"
}

object ChicalaConfig {
  var simulation = false
  var whitelist  = List.empty[String]
}

class ChicalaPlugin(val global: Global) extends Plugin {
  import global._

  val name: String        = ChicalaPlugin.name
  val description: String = ChicalaPlugin.description

  val components: List[PluginComponent] = List(
    new ChiselToScalaComponent(global)
  )

  override def init(options: List[String], error: String => Unit): Boolean = {
    for (option <- options) {
      if (option.startsWith("simulation:")) {
        val emitFormat = option.substring("simulation:".length)
        emitFormat match {
          case "false" => ChicalaConfig.simulation = false
          case "true"  => ChicalaConfig.simulation = true
          case _: String =>
            error("simulation not understood: " + emitFormat)
        }
      } else if (option.startsWith("whitelist:")) {
        ChicalaConfig.whitelist = option
          .substring("whitelist:".length)
          .split(";")
          .map(_.strip())
          .toList
        inform("chicala whitelist:")
        ChicalaConfig.whitelist.foreach(s => inform(" " + s))
      } else {
        error("Option not understood: " + option)
      }
    }
    true
  }

  override val optionsHelp: Option[String] = Some(
    """|  -P:chicala:simulation:<true/false>
       |                               Set emit mode, for simulation or not. [false]
       |  -P:chicala:whitelist:<package.class>;<package.class>;...
       |                               Only modules in the whitelist will be processed, not set to process all modules. [not set]
       |""".stripMargin
  )
}
