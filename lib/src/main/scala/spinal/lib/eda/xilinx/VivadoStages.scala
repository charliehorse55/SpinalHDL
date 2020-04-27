package spinal.lib.eda.xilinx

import java.io.File
import java.lang.NumberFormatException
import java.nio.file.Paths

import spinal.core._

import scala.collection.mutable.ArrayBuffer
import sys.process._


case class Vivado(vivadoPath: String, family: String, device: String, processorCount: Int = 8) {

  def writeFile(name: String, dir: String, contents: String): Unit = {
    val f = new java.io.FileWriter(Paths.get(dir,name).toFile)
    f.write(contents)
    f.flush();
    f.close();
  }

  def synthModuleOOC(rtl: File, module: Component, extraOptions: String = "", targetFrequency: HertzNumber = 600 MHz) {

    val runName = f"${module.definitionName}@${targetFrequency.toDouble/1e6}%.0fMHz"

    val output = Paths.get("ooc", runName)
    output.toFile.mkdirs()
    val outputDir = output.toString

    val period = (targetFrequency.toTime*1e9) toBigDecimal
    val clkName = module.clockDomain.clock.getName()
    val xdcName = "constraints.xdc"
    writeFile(xdcName, outputDir, f"create_clock -period $period%.3f [get_ports $clkName]\n")

    val src = new VivadoScript()

    src.addCmd(s"read_xdc -mode out_of_context $xdcName")
    src.addSrc(rtl)
    src.addCmd(s"synth_design -top ${module.definitionName} -part ${device} -mode out_of_context ${extraOptions}")
    src.addCmd(s"opt_design")
    src.addCmd(s"report_utilization -file Utilization.rpt")
    src.addCmd(s"report_timing_summary -file Timing.rpt")
    src.writeCheckpoint(module.definitionName)
    Run(new File(outputDir), src.build())
  }

  def synthProject(topName: String, runName: String, includes: Seq[File], extraOptions: String = "") {
    val output = Paths.get("synth", runName)
    output.toFile.mkdirs()
    val outputDir = output.toString

    val src = new VivadoScript()

    src.addCmd(s"create_project -in_memory -part ${device}")

    for(file <- includes) {
      src.addSrc(file)
    }

    src.addCmd(s"synth_design -top ${topName} ${extraOptions}")
    src.addCmd(s"report_utilization -file Utilization.rpt")
    src.addCmd(s"report_timing_summary -file Timing.rpt")
    src.writeCheckpoint("Synth")
    Run(new File(outputDir), src.build())

  }

  def placement(runName: String, checkpoint: File, directive: String, extraOptions: String = "") {
    val output = Paths.get("place", runName)
    output.toFile.mkdirs()
    val outputDir = output.toString

    val src = new VivadoScript()

    src.addCmd(s"open_checkpoint ${checkpoint.getAbsolutePath}")

    src.addCmd(s"opt_design")
    src.addCmd(s"place_design -directive ${directive} ${extraOptions}")
    src.addCmd(s"phys_opt_design -directive AggressiveExplore")
    src.addCmd(s"report_timing_summary -file Timing.rpt")
    src.writeCheckpoint("Placed")
    Run(new File(outputDir), src.build())
  }

  def route(runName: String, checkpoint: File, directive: String, extraOptions: String) {
    val output = Paths.get("route", runName)
    output.toFile.mkdirs()
    val outputDir = output.toString

    val src = new VivadoScript()

    src.addCmd(s"open_checkpoint ${checkpoint.getAbsolutePath}")

    src.addCmd(s"route_design -directive ${directive} ${extraOptions}")
    src.addCmd(s"phys_opt_design -directive AggressiveExplore")
    src.addCmd(s"report_timing_summary -file Timing.rpt")
    src.writeCheckpoint("Routed")
    Run(new File(outputDir), src.build())

  }

  def bitstream(outputName: String, checkpoint: File): Int = {
    val outputDir = "bitstreams"


    val src = new VivadoScript()

    src.addCmd(s"read_checkpoint ${checkpoint.getAbsolutePath}")
    src.addCmd(s"write_bitstream ${outputName}.bit")
    Run(new File(outputDir), src.build())
  }


  def findCheckpoints(folder: String): Seq[(Double, String, File)] = {
    val directories = new File(folder).listFiles.filter(_.isDirectory)
    var results: ArrayBuffer[(Double, String, File)] = new ArrayBuffer
    for(dir <- directories) {
      val checkpoints = dir.listFiles.filter(f => f.getName.endsWith(".dcp"))
      for(checkpoint <- checkpoints) {
        var name = dir.getName
        if(checkpoints.length > 1) {
          name += checkpoint.getName
        }
        val toTry = Array("Synth", "Placed", "Routed")
        var slackStr = checkpoint.getName.stripSuffix(".dcp")
        for(prefix <- toTry) {
          slackStr = slackStr.stripPrefix(prefix)
        }
        var slack = 0.0
        try {
          slack = slackStr.toDouble
        } catch {
          case _: NumberFormatException => //do nothing
        }
        results += ((slack, name, checkpoint))
      }
    }
    results.sortBy(-_._1)
  }

  class VivadoScript() {
    val builder = new StringBuilder

    addCmd(s"set_param general.maxThreads ${processorCount}")

    def addCmd(cmd: String) {
      builder ++= cmd
      builder += '\n'
    }

    def addSrc(file: File) {
      val cmd: String = file.getName.split("\\.").last match {
        case "v" | "sv" => "verilog"
        case "dcp" => "checkpoint"
        case "vhd" | "vhdl" => "vhdl"
        case "xdc" => "xdc"
      }
      addCmd(s"read_${cmd} ${file.getAbsolutePath}")
    }

    def writeCheckpoint(name: String): Unit = {
      addCmd(s"set WNS [expr {[get_property SLACK [get_timing_paths]]}]")
      addCmd(s"write_checkpoint $name$$WNS.dcp")
    }

    def build(): String = {
      builder.toString()
    }
  }

  def Run(directory: File, script: String): Int = {
    val scriptName = "run.tcl"
    writeFile(scriptName, directory.getAbsolutePath, script)

    val cmd = Array(s"$vivadoPath/vivado", "-nojournal", "-mode", "batch", "-source", scriptName)

    Process(cmd, directory) ! ProcessLogger(line => ())
  }
}

object VivadoTest {
  def main(args: Array[String]) {
    val vivado = Vivado(
      vivadoPath="/home/evan/apps/xilinx-linux/Vivado/2019.1/bin",
      family="Virtex Ultrascale+",
      device="xcvu9p-fsgd2104-2LV-e"
    )
    val results = vivado.findCheckpoints("/home/evan/projects/tellor_fpga/controller/hardware/output_FKSLR2/place")
    for((slack, name, path) <- results) {
      printf("%.3f %s - %s\n", slack, name, path.toString)
    }

    //    val f = new vivado.Instance("")
//    val out = f.doCmd("read_verilog asdfasdf.v")
//    f.exit()
//    printf("output:\n%s\n", out)
  }

}
