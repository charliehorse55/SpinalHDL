package spinal.lib.misc

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.bus.wishbone.WishboneConfig

object Clint{
  def getWisboneConfig() = WishboneConfig(
    addressWidth = 14,
    dataWidth = 32
  )
}

case class Clint(hartCount : Int) extends Area{
  val time = Reg(UInt(64 bits)) init(0)
  time := time + 1

  val harts = for(hartId <- 0 until hartCount) yield new Area{
    val cmp = Reg(UInt(64 bits))
    val timerInterrupt = RegNext(!(time - cmp).msb)
    val softwareInterrupt = RegInit(False)
  }

  def driveFrom(bus : BusSlaveFactory) = new Area{
    val IPI_ADDR = 0x0000
    val CMP_ADDR = 0x4000
    val TIME_ADDR = 0xBFF8

    bus.readMultiWord(time, TIME_ADDR)
    val hartsMapping = for(hartId <- 0 until hartCount) yield new Area{
      bus.writeMultiWord(harts(hartId).cmp, CMP_ADDR + 8*hartId)
      bus.readAndWrite(harts(hartId).softwareInterrupt, IPI_ADDR + 4*hartId, bitOffset = 0)
    }
  }
}
