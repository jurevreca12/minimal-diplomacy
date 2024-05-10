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




// Demo source SoC component
class LBIRDemoCounter[T <: Bits](genT: T, numBeats: Int, depth: Int)(implicit p: Parameters) extends LazyModule {
    val node = LBIRMasterNode[T](Seq(
        LBIRMasterPortParameters[T](Seq(
            LBIRMasterParameters(
                "Demo source",
                Nil,
            )
        ))
    ))
    lazy val module = new LazyModuleImp(this) { 
        val (out, _) = node.out.head
        val (counterValue, counterWrap) = Counter(out.fire, depth)
        out.bits.zipWithIndex.foreach { case (out: Bits, index: Int) => out := counterValue } 
        out.valid := true.B
        out.last := counterWrap
        dontTouch(out)
    }
}


// Top-level demo module
class LBIRDemoSRAMTop(implicit p: Parameters) extends LazyModule {
    val cntr = LazyModule(new LBIRDemoCounter(UInt(4.W), 3, 8))
    val sram = LazyModule(new LBIRSRAM(UInt(4.W), 3, 8))
    
    // Very important connection
    sram.node := cntr.node
    // etc
    lazy val module = new LazyModuleImp(this) { 
    }
}

object main_sram extends App {
    implicit val p: Parameters = Parameters.empty
    val lm = LazyModule(
        new LBIRDemoSRAMTop()
    )
    (new chisel3.stage.ChiselStage).run(Seq(
        chisel3.stage.ChiselGeneratorAnnotation(
            () => lm.module
        )
    ))
}