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

class LBIRImp[T <: Bits] extends SimpleNodeImp[LBIRMasterPortParameters[T], LBIRSlavePortParameters[T], LBIREdgeParameters[T], LBIRBundle[T]] {
    // Collect downstream and upstream parameters into an edge.
    def edge(pd: LBIRMasterPortParameters[T], pu: LBIRSlavePortParameters[T], p: Parameters, sourceInfo: SourceInfo) = {
        /*require(pd.bundleParams.genT.getClass == pu.bundleParams.genT.getClass)
        require(pd.bundleParams.genT.getWidth == pu.bundleParams.genT.getWidth)
        require(pd.bundleParams.numBeats == pu.bundleParams.numBeats)*/
        LBIREdgeParameters[T](pd, pu, p, sourceInfo)
    }
    // generate hardware bundle.
    def bundle(e: LBIREdgeParameters[T]) = new LBIRBundle[T](e.slave.bundleParams)

    def render(e: LBIREdgeParameters[T]) = RenderedEdge(colour = "#00ffcc" /* grenish */, "xxxx")

    // Tell this node that it has an additional outgoing connection
    override def mixO(pd: LBIRMasterPortParameters[T], node: OutwardNode[LBIRMasterPortParameters[T], LBIRSlavePortParameters[T], LBIRBundle[T]]) = {
        pd.copy(masters = pd.masters.map  { c => c.copy (nodePath = node +: c.nodePath) })
    }
    // Tell this node that it has an additional incoming connection
    override def mixI(pu: LBIRSlavePortParameters[T], node: InwardNode[LBIRMasterPortParameters[T], LBIRSlavePortParameters[T], LBIRBundle[T]]) = {
        pu.copy(slaves  = pu.slaves.map { m => m.copy (nodePath = node +: m.nodePath) })
    }
}

case class LBIRMasterNode[T <: Bits](portParams: Seq[LBIRMasterPortParameters[T]])(implicit valName: ValName) extends SourceNode(new LBIRImp[T])(portParams)
case class LBIRSlaveNode[T <: Bits](portParams: Seq[LBIRSlavePortParameters[T]])(implicit valName: ValName) extends SinkNode(new LBIRImp[T])(portParams)
case class LBIRNexusNode[T <: Bits](masterFn: Seq[LBIRMasterPortParameters[T]] => LBIRMasterPortParameters[T], 
                    slaveFn: Seq[LBIRSlavePortParameters[T]] => LBIRSlavePortParameters[T])(implicit valName: ValName) extends NexusNode(new LBIRImp[T])(masterFn, slaveFn)

