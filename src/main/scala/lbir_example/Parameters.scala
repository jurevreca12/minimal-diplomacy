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
                                       
case class LBIRBundleParameters[T <: Bits](
    genT: T,
    numBeats: Int
)
class LBIRBundle[T <: Bits](val params: LBIRBundleParameters[T]) extends ReadyValidIO[Vec[T]](Vec(params.numBeats, params.genT)) {
    val last = Output(Bool())
}

case class LBIRMasterParameters(
    name: String,
    nodePath: Seq[BaseNode]
)

case class LBIRMasterPortParameters[T <: Bits](
    masters: Seq[LBIRMasterParameters]
)

case class LBIRSlaveParameters(
    address: Seq[AddressSet],
    nodePath: Seq[BaseNode],
)

case class LBIRSlavePortParameters[T <: Bits](
    slaves: Seq[LBIRSlaveParameters],
    bundleParams: LBIRBundleParameters[T]
)

case class LBIREdgeParameters[T <: Bits](
    master: LBIRMasterPortParameters[T],
    slave: LBIRSlavePortParameters[T],
    p: Parameters,
    sourceInfo: SourceInfo,
) 

