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
class LBIRBundle[T <: Bits](val params: LBIRBundleParameters[T]) extends ReadyValidIO[T](params.genT) {
    val last = Output(Bool())
}

case class LBIRMasterParameters(
    name: String,
    nodePath: Seq[BaseNode]
)
case class LBIRSlaveParameters(
    address: Seq[AddressSet],
    nodePath: Seq[BaseNode]
)

case class LBIRMasterPortParameters(
    masters: Seq[LBIRMasterParameters]
)
case class LBIRSlavePortParameters(
    slaves: Seq[LBIRSlaveParameters]
)
case class LBIREdgeParameters[T <: Bits](
    master: LBIRMasterPortParameters,
    slave: LBIRSlavePortParameters) {
    val bundle = LBIRBundleParameters[T](
        genT = UInt(3.W).asInstanceOf[T],
        numBeats = 2
    )
}

class LBIRImp[T <: Bits] extends SimpleNodeImp[LBIRMasterPortParameters, LBIRSlavePortParameters, LBIREdgeParameters[T], LBIRBundle[T]] {
    // Collect downstream and upstream parameters into an edge.
    def edge(pd: LBIRMasterPortParameters, pu: LBIRSlavePortParameters, p: Parameters, sourceInfo: SourceInfo) = LBIREdgeParameters(pd, pu)

    // generate hardware bundle.
    def bundle(e: LBIREdgeParameters[T]) = new LBIRBundle[T](e.bundle)


    def render(e: LBIREdgeParameters[T]) = RenderedEdge(colour = "#00ffcc" /* grenish */, "xxxx")

    // Tell this node that it has an additional outgoing connection
    override def mixO(pd: LBIRMasterPortParameters, node: OutwardNode[LBIRMasterPortParameters, LBIRSlavePortParameters, LBIRBundle[T]]) = pd.copy(masters = pd.masters.map {
        c => c.copy(nodePath = node +: c.nodePath)
    })
    // Tell this node that it has an additional incoming connection
    override def mixI(pu: LBIRSlavePortParameters, node: InwardNode[LBIRMasterPortParameters, LBIRSlavePortParameters, LBIRBundle[T]]) = pu.copy(slaves = pu.slaves.map {
        m => m.copy(nodePath = node +: m.nodePath)
    })
}

case class LBIRMasterNode[T <: Bits](portParams: Seq[LBIRMasterPortParameters])(implicit valName: ValName) extends SourceNode(new LBIRImp[T])(portParams)
case class LBIRSlaveNode[T <: Bits](portParams: Seq[LBIRSlavePortParameters])(implicit valName: ValName) extends SinkNode(new LBIRImp[T])(portParams)
case class LBIRNexusNode[T <: Bits](masterFn: Seq[LBIRMasterPortParameters] => LBIRMasterPortParameters, 
                    slaveFn: Seq[LBIRSlavePortParameters] => LBIRSlavePortParameters)(implicit valName: ValName) extends NexusNode(new LBIRImp[T])(masterFn, slaveFn)

// Demo source SoC component
class LBIRDemoSource(implicit p: Parameters) extends LazyModule {
    val node = LBIRMasterNode(Seq(
        LBIRMasterPortParameters(
            Seq(
                LBIRMasterParameters("asd", Nil)
            )
        )
    ))
    lazy val module = new LazyModuleImp(this) { 
        val out = node.out.unzip._1
        out(0) := 0.U.asTypeOf(out(0))
        dontTouch(out(0))
    }
}

// Demo sink SoC component
class LBIRDemoSink(implicit p: Parameters) extends LazyModule {
    val node = LBIRSlaveNode(Seq(
        LBIRSlavePortParameters(
            Seq(
                LBIRSlaveParameters(Seq(AddressSet(0,8)), Nil)
            )
        )
    ))
    lazy val module = new LazyModuleImp(this) { 
        val in = node.in.unzip._1
        dontTouch(in(0))
     }
}

// Top-level demo module
class LBIRDemoTop(implicit p: Parameters) extends LazyModule {
    val m1 = LazyModule(new LBIRDemoSource)
    val m2 = LazyModule(new LBIRDemoSink)
    // Very important connection
    m2.node := m1.node
    // etc
    lazy val module = new LazyModuleImp(this) { 
     }
}

object main extends App {
    implicit val p: Parameters = Parameters.empty
    val lm = LazyModule(
        new LBIRDemoTop()
    )
    (new chisel3.stage.ChiselStage).run(Seq(
        chisel3.stage.ChiselGeneratorAnnotation(
            () => lm.module
        )
    ))
}