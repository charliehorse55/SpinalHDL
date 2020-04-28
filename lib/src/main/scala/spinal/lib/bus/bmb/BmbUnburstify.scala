package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._

object BmbUnburstify{
  def outputParameter(inputParameter : BmbParameter) = inputParameter.copy(
    lengthWidth = log2Up(inputParameter.byteCount),
    contextWidth = inputParameter.contextWidth + 2 + inputParameter.sourceWidth
  )
}

//TODO check inputParameter requirements
case class BmbUnburstify(inputParameter : BmbParameter) extends Component{
  val outputParameter = BmbUnburstify.outputParameter(inputParameter)

  val io = new Bundle {
    val input = slave(Bmb(inputParameter))
    val output = master(Bmb(outputParameter))
  }

  val doResult = Bool
  val addrIncrRange = (Math.min(Bmb.boundaryWidth-1, inputParameter.addressWidth - 1) downto 0)

  val buffer = new Area{
    val valid       = RegInit(False)
    val opcode      = Reg(Bits(1 bits))
    val source      = Reg(UInt(inputParameter.sourceWidth bits))
    val address     = Reg(UInt(inputParameter.addressWidth bits))
    val context     = Reg(Bits(inputParameter.contextWidth bits))
    val beat        = Reg(UInt(inputParameter.beatCounterWidth bits))
    val last        = beat === 1
    val addressIncr = Bmb.incr(address = address, p = inputParameter)
    val isWrite = Bmb.Cmd.Opcode.isWrite(opcode)

    when(io.output.cmd.fire) {
      beat := beat - 1
      address(addrIncrRange) := addressIncr(addrIncrRange)
      when(last){
        valid := False
      }
    }
  }

  val cmdTransferBeatCount = io.input.cmd.transferBeatCountMinusOne
  val requireBuffer = cmdTransferBeatCount =/= 0

  if(outputParameter.canWrite) {
    io.output.cmd.data := io.input.cmd.data
    io.output.cmd.mask := io.input.cmd.mask
  }
  io.output.cmd.last := True

  case class Context() extends Bundle {
    val context = Bits(inputParameter.contextWidth bits)
    val drop, last = Bool()
    val source = UInt(inputParameter.sourceWidth bits)
  }
  val cmdContext = Context()
  io.output.cmd.context := B(cmdContext)
  io.output.cmd.source := 0

  //payload muxes
  when(buffer.valid) {
    io.output.cmd.address := buffer.addressIncr
    io.output.cmd.opcode := buffer.opcode
    io.output.cmd.length := inputParameter.byteCount-1
    cmdContext.context := buffer.context
    cmdContext.source := buffer.source
  } otherwise {
    io.output.cmd.address := io.input.cmd.address
    io.output.cmd.opcode := io.input.cmd.opcode
    when(requireBuffer) {
      io.output.cmd.address(inputParameter.wordRange) := 0
      io.output.cmd.length := inputParameter.byteCount-1 // (inputParameter.byteCount - io.input.cmd.length(inputParameter.wordRange)).resized
    } otherwise {
      io.output.cmd.length := io.input.cmd.length.resized
    }
    cmdContext.context := io.input.cmd.context
    cmdContext.source :=  io.input.cmd.source
  }



  io.input.cmd.ready := False
  when(buffer.valid){
    io.output.cmd.valid := !(buffer.isWrite && !io.input.cmd.valid)
    io.input.cmd.ready  :=   buffer.isWrite && io.output.cmd.ready
    cmdContext.last := buffer.last
    cmdContext.drop := buffer.isWrite
  }otherwise{
    io.input.cmd.ready  := io.output.cmd.ready
    io.output.cmd.valid := io.input.cmd.valid
    buffer.opcode := io.input.cmd.opcode
    buffer.source := io.input.cmd.source
    buffer.address := io.input.cmd.address
    buffer.context := io.input.cmd.context
    buffer.beat    := cmdTransferBeatCount
    cmdContext.drop := io.input.cmd.isWrite
    cmdContext.last := !requireBuffer
    buffer.valid := requireBuffer && io.output.cmd.fire
  }

  val rspContext = io.output.rsp.context.as(Context())
  io.input.rsp.valid := io.output.rsp.valid && (rspContext.last || !rspContext.drop)
  io.input.rsp.last := rspContext.last
  io.input.rsp.source := rspContext.source
  io.input.rsp.opcode := io.output.rsp.opcode
  io.input.rsp.data := io.output.rsp.data
  io.input.rsp.context := rspContext.context
  io.output.rsp.ready := io.input.rsp.ready
}
