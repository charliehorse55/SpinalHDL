package spinal.lib.eda.xilinx

import java.io.File
import java.nio.file.Paths
import java.util
import java.util.Scanner

import com.pty4j.PtyProcess
import spinal.core._
import spinal.lib.eda.bench.Report


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

    val inst = new Instance(outputDir)

    inst.doCmd(s"read_xdc -mode out_of_context $xdcName")
    inst.doCmd(s"read_verilog ${rtl.getAbsolutePath}")
    inst.doCmd(s"synth_design -top ${module.definitionName} -part ${device} -mode out_of_context ${extraOptions}")
    inst.doCmd(s"opt_design")
    inst.doCmd(s"report_utilization -file Utilization.rpt")
    inst.doCmd(s"report_timing_summary -file Timing.rpt")
    inst.doCmd(s"set WNS [expr {[get_property SLACK [get_timing_paths]]}]")
    inst.doCmd(s"write_checkpoint ${module.definitionName}$$WNS.dcp")
    inst.exit()
  }

  def synthProject(topName: String, runName: String, includes: Seq[File], extraOptions: String = "") {
    val output = Paths.get("synth", runName)
    output.toFile.mkdirs()
    val outputDir = output.toString

    val inst = new Instance(outputDir)

    inst.doCmd(s"create_project -in_memory -part ${device}")

    for(file <- includes) {
      val cmd: String = file.getName.split("\\.").last match {
        case "v" | "sv" => "verilog"
        case "dcp" => "checkpoint"
        case "vhd" | "vhdl" => "vhdl"
        case "xdc" => "xdc"
      }
      inst.doCmd(s"read_${cmd} ${file.getAbsolutePath}")
    }

    inst.doCmd(s"synth_design -top ${topName} ${extraOptions}")
    inst.doCmd(s"report_utilization -file Utilization.rpt")
    inst.doCmd(s"report_timing_summary -file Timing.rpt")
    inst.doCmd(s"set WNS [expr {[get_property SLACK [get_timing_paths]]}]")
    inst.doCmd(s"write_checkpoint Synth$$WNS.dcp")
    inst.exit()
  }

  def placement(runName: String, checkpoint: File, directive: String, extraOptions: String = "") {
    val output = Paths.get("place", runName)
    output.toFile.mkdirs()
    val outputDir = output.toString

    val inst = new Instance(outputDir)

    inst.doCmd(s"open_checkpoint ${checkpoint.getAbsolutePath}")

    inst.doCmd(s"opt_design")
    inst.doCmd(s"place_design -directive ${directive} ${extraOptions}")
    inst.doCmd(s"phys_opt_design -directive AggressiveExplore")
    inst.doCmd(s"report_timing_summary -file Timing.rpt")
    inst.doCmd(s"set WNS [expr {[get_property SLACK [get_timing_paths]]}]")
    inst.doCmd(s"write_checkpoint Placed$$WNS.dcp")
    inst.exit()
  }

  def route(runName: String, checkpoint: File, directive: String, extraOptions: String) {
    val output = Paths.get("route", runName)
    output.toFile.mkdirs()
    val outputDir = output.toString

    val inst = new Instance(outputDir)

    inst.doCmd(s"open_checkpoint ${checkpoint.getAbsolutePath}")

    inst.doCmd(s"route_design -directive ${directive} ${extraOptions}")
    inst.doCmd(s"phys_opt_design -directive AggressiveExplore")
    inst.doCmd(s"report_timing_summary -file Timing.rpt")
    inst.doCmd(s"set WNS [expr {[get_property SLACK [get_timing_paths]]}]")
    inst.doCmd(s"write_checkpoint Routed$$WNS.dcp")
    inst.exit()
  }

  def bitstream(outputName: String, checkpoint: File) {
    val outputDir = "bitstreams"

    val inst = new Instance(outputDir)

    inst.doCmd(s"read_checkpoint ${checkpoint.getAbsolutePath}")
    inst.doCmd(s"write_bitstream ${outputName}.bit")
    inst.exit()
  }


  def findCheckpoints(folder: String): Map[String, File] = {
    val directories = new File(folder).listFiles.filter(_.isDirectory)
    var results = Map.empty[String, File]
    for(dir <- directories) {
      val checkpoints = dir.listFiles.filter(f => f.getName.endsWith(".dcp"))
      for(checkpoint <- checkpoints) {
        var name = dir.getName
        if(checkpoints.length > 1) {
          name += checkpoint.getName
        }
        results += (name -> checkpoint)
      }
    }
    results
  }

  class Instance(directory: String) {
    private val cmd = Array(s"$vivadoPath/vivado", "-nojournal", "-mode", "tcl")

    val p = PtyProcess.exec(cmd, null, directory)
    private val inputStream = p.getOutputStream
    private val outputStream = p.getInputStream

    val s = new Scanner(outputStream)
    s.useDelimiter("\nVivado%")

    //read in the stuff printed at launch
    s.next()

    doCmd(s"set_param general.maxThreads ${processorCount}")


    def doCmd(cmd: String): String = {
      inputStream.write(s"$cmd\n".getBytes())
      inputStream.flush()
      s.next().split("\n", 2)(1)
    }

    def getSlowestPath(p: TimeNumber): TimeNumber = {
      val result = doCmd("expr {[get_property SLACK [get_timing_paths]]}")
      val lines = result.split("\n")
      p - TimeNumber(lines(lines.length-1).toDouble)
    }


    def exit() {
      inputStream.write("exit\n".getBytes())
      inputStream.flush()
    }
  }

}

object VivadoTest {
  def main(args: Array[String]) {
    val vivado = Vivado(
      vivadoPath="/home/evan/apps/xilinx-linux/Vivado/2019.1/bin",
      family="Virtex Ultrascale+",
      device="xcvu9p-fsgd2104-2LV-e"
    )
    val results = vivado.findCheckpoints("/home/evan/projects/tellor_fpga/controller/hardware/output_FKSLR2/ooc")
    for((name, path) <- results) {
      printf("%s - %s\n", name, path.toString)
    }
//    val f = new vivado.Instance("")
//    val out = f.doCmd("read_verilog asdfasdf.v")
//    f.exit()
//    printf("output:\n%s\n", out)
  }

}
