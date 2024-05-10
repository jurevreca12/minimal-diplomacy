package lbir_example

import chipsalliance.rocketchip.config.{Config, Parameters}
import chisel3._
import chisel3.util._
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.stage.ChiselStage
import chisel3.util.random.FibonacciLFSR
import freechips.rocketchip.diplomacy.{LazyModule, 
                                       LazyModuleImp, 
                                       LazyModuleImpLike, 
                                       AddressSet, 
                                       RenderedEdge, 
                                       SimpleNodeImp, 
                                       SinkNode, 
                                       SourceNode,
                                       NexusNode,
                                       OutwardNode,
                                       InwardNode,
                                       BaseNode,
                                       ValName}



class SRAMRead(depth: Int, width: Int) extends Bundle {
  val enable = Input(Bool())
  val address = Input(UInt(log2Up(depth).W))
  val data = Output(UInt(width.W))
}
class SRAMWrite(depth: Int, width: Int) extends Bundle {
  val enable = Input(Bool())
  val address = Input(UInt(log2Up(depth).W))
  val data =  Input(UInt(width.W))
}

class SRAM(depth: Int, width: Int = 32) extends Module {  
    val io = IO(new Bundle {
        val read = new SRAMRead(depth, width)
        val write = new SRAMWrite(depth, width)
    })

    val mem = SyncReadMem(depth, UInt(width.W))

    // Create one write port and one read port
    when(io.write.enable) {
        mem.write(io.write.address, io.write.data)
    }
    io.read.data := mem.read(io.read.address, io.read.enable)
}

/* Represents an SRAM memory with one read port and one write port.
*  The write port is diplomaticly engaged, however the read port is available to the "outside"
*/
class LBIRSRAM[T <: Bits](genT: T, numBeats: Int, depth: Int)(implicit p: Parameters) extends LazyModule {
    val node = LBIRSlaveNode[T](Seq(
        LBIRSlavePortParameters[T](
            slaves = Seq(
                LBIRSlaveParameters(
                    Seq(AddressSet(0, depth)),
                    Nil
                )),
            bundleParams = LBIRBundleParameters[T](
                genT,
                numBeats
            ) 
        )
    ))
    lazy val module = new LazyModuleImp(this) {
        val (in, _) = node.in.head
        val (addressCounterValue, addressCounterWrap) = Counter(in.fire, depth)
        val mem = Module(new SRAM(depth, width=genT.getWidth*numBeats))
        mem.io.write.enable := in.fire()
        mem.io.write.address := addressCounterValue
        mem.io.write.data := in.bits.asTypeOf(mem.io.write.data)
        //assert(addressCounterWrap == in.last)
        val out = IO(new SRAMRead(depth, genT.getWidth*numBeats))
        out <> mem.io.read
        dontTouch(out)
    }
}