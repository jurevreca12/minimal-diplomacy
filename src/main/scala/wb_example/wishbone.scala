package wb_example
/*
    A diplomatic implementation of the WishBone protocol, based on the talk: A Crash Course in the Diplomacy Framework by Edward Wang.
    https://www.youtube.com/watch?v=4VfMCO4q26g
*/
import chipsalliance.rocketchip.config.{Config, Parameters}
import chisel3._
import chisel3.util._
import chisel3.internal.sourceinfo.SourceInfo
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

case class WBBundleParameters(
    addrBits: Int,
    dataBits: Int
)

class WBBundle(val params: WBBundleParameters) extends Bundle {
    val address = Output(UInt(params.addrBits.W))
    val dataOut = Output(UInt(params.dataBits.W))
    val dataIn = Input(UInt(params.dataBits.W))
    val acknowledge = Input(Bool())
}

case class WBMasterParameters(
    name: String,
    nodePath: Seq[BaseNode]
)
case class WBSlaveParameters(
    address: Seq[AddressSet],
    nodePath: Seq[BaseNode]
)
case class WBMasterPortParameters(
    masters: Seq[WBMasterParameters]
)
case class WBSlavePortParameters(
    slaves: Seq[WBSlaveParameters]
)
case class WBEdgeParameters(
    master: WBMasterPortParameters,
    slave: WBSlavePortParameters) {
    val bundle = WBBundleParameters(
        addrBits = 2,
        dataBits = 4
    )
}

object WBImp extends SimpleNodeImp[WBMasterPortParameters, WBSlavePortParameters, WBEdgeParameters, WBBundle] {
    // Collect downstream and upstream parameters into an edge.
    def edge(pd: WBMasterPortParameters, pu: WBSlavePortParameters, p: Parameters, sourceInfo: SourceInfo) = WBEdgeParameters(pd, pu)

    // generate hardware bundle.
    def bundle(e: WBEdgeParameters) = new WBBundle(e.bundle)


    def render(e: WBEdgeParameters) = RenderedEdge(colour = "#00ffcc" /* grenish */, "xxxx")

    // Tell this node that it has an additional outgoing connection
    override def mixO(pd: WBMasterPortParameters, node: OutwardNode[WBMasterPortParameters, WBSlavePortParameters, WBBundle]) = pd.copy(masters = pd.masters.map {
        c => c.copy(nodePath = node +: c.nodePath)
    })
    // Tell this node that it has an additional incoming connection
    override def mixI(pu: WBSlavePortParameters, node: InwardNode[WBMasterPortParameters, WBSlavePortParameters, WBBundle]) = pu.copy(slaves = pu.slaves.map {
        m => m.copy(nodePath = node +: m.nodePath)
    })
}


case class WBMasterNode(portParams: Seq[WBMasterPortParameters])(implicit valName: ValName) extends SourceNode(WBImp)(portParams)
case class WBSlaveNode(portParams: Seq[WBSlavePortParameters])(implicit valName: ValName) extends SinkNode(WBImp)(portParams)
case class WBNexusNode(masterFn: Seq[WBMasterPortParameters] => WBMasterPortParameters, 
                       slaveFn: Seq[WBSlavePortParameters] => WBSlavePortParameters)(implicit valName: ValName) extends NexusNode(WBImp)(masterFn, slaveFn)


// Demo source SoC component
class WBDemoSource(implicit p: Parameters) extends LazyModule {
    val node = WBMasterNode(Seq(
        WBMasterPortParameters(
            Seq(
                WBMasterParameters("asd", Nil)
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
class WBDemoSink(implicit p: Parameters) extends LazyModule {
    val node = WBSlaveNode(Seq(
        WBSlavePortParameters(
            Seq(
                WBSlaveParameters(Seq(AddressSet(0,8)), Nil)
            )
        )
    ))
    lazy val module = new LazyModuleImp(this) { 
        val in = node.in.unzip._1
        dontTouch(in(0))
     }
}

// Top-level demo module
class WBDemoTop(implicit p: Parameters) extends LazyModule {
    val m1 = LazyModule(new WBDemoSource)
    val m2 = LazyModule(new WBDemoSink)
    // Very important connection
    m2.node := m1.node
    // etc
    lazy val module = new LazyModuleImp(this) { 
    }
}

object main extends App {
    implicit val p: Parameters = Parameters.empty
    val lm = LazyModule(
        new WBDemoTop()
    )
    (new chisel3.stage.ChiselStage).run(Seq(
        chisel3.stage.ChiselGeneratorAnnotation(
            () => lm.module
        )
    ))
}