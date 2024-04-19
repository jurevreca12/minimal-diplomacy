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

case class LBIRMasterParameters[T <: Bits](
    name: String,
    nodePath: Seq[BaseNode],
    bundleParams: LBIRBundleParameters[T]
)
case class LBIRSlaveParameters[T <: Bits](
    address: Seq[AddressSet],
    nodePath: Seq[BaseNode],
    bundleParams: LBIRBundleParameters[T]
)
case class LBIREdgeParameters[T <: Bits](
    master: LBIRMasterParameters[T],
    slave: LBIRSlaveParameters[T]
) 

class LBIRImp[T <: Bits] extends SimpleNodeImp[LBIRMasterParameters[T], LBIRSlaveParameters[T], LBIREdgeParameters[T], LBIRBundle[T]] {
    // Collect downstream and upstream parameters into an edge.
    def edge(pd: LBIRMasterParameters[T], pu: LBIRSlaveParameters[T], p: Parameters, sourceInfo: SourceInfo) = {
        require(pd.bundleParams.genT.getClass == pu.bundleParams.genT.getClass)
        require(pd.bundleParams.genT.getWidth == pu.bundleParams.genT.getWidth)
        require(pd.bundleParams.numBeats == pu.bundleParams.numBeats)
        LBIREdgeParameters[T](pd, pu)
    }
    // generate hardware bundle.
    def bundle(e: LBIREdgeParameters[T]) = new LBIRBundle[T](e.master.bundleParams)

    def render(e: LBIREdgeParameters[T]) = RenderedEdge(colour = "#00ffcc" /* grenish */, "xxxx")

    // Tell this node that it has an additional outgoing connection
    override def mixO(pd: LBIRMasterParameters[T], node: OutwardNode[LBIRMasterParameters[T], LBIRSlaveParameters[T], LBIRBundle[T]]) = {
        pd.copy(nodePath = node +: pd.nodePath)
    }
    // Tell this node that it has an additional incoming connection
    override def mixI(pu: LBIRSlaveParameters[T], node: InwardNode[LBIRMasterParameters[T], LBIRSlaveParameters[T], LBIRBundle[T]]) = {
        pu.copy(nodePath = node +: pu.nodePath)
    }
}

case class LBIRMasterNode[T <: Bits](portParams: Seq[LBIRMasterParameters[T]])(implicit valName: ValName) extends SourceNode(new LBIRImp[T])(portParams)
case class LBIRSlaveNode[T <: Bits](portParams: Seq[LBIRSlaveParameters[T]])(implicit valName: ValName) extends SinkNode(new LBIRImp[T])(portParams)
case class LBIRNexusNode[T <: Bits](masterFn: Seq[LBIRMasterParameters[T]] => LBIRMasterParameters[T], 
                    slaveFn: Seq[LBIRSlaveParameters[T]] => LBIRSlaveParameters[T])(implicit valName: ValName) extends NexusNode(new LBIRImp[T])(masterFn, slaveFn)

// Demo source SoC component
class LBIRDemoSource[T <: Bits](genT: T, numBeats: Int)(implicit p: Parameters) extends LazyModule {
    val node = LBIRMasterNode[T](Seq(
        LBIRMasterParameters[T](
            "Demo fuzzer",
            Nil,
            LBIRBundleParameters(
                genT,
                numBeats
            )
        )
    ))
    lazy val module = new LazyModuleImp(this) { 
        val outBundle = node.out.head._1
        outBundle.bits.zipWithIndex.foreach { case (out: Bits, index: Int) => out := index.U.asInstanceOf[T] } 
        dontTouch(outBundle)
    }
}

// Demo sink SoC component
class LBIRDemoSink[T <: Bits](genT: T, numBeats: Int)(implicit p: Parameters) extends LazyModule {
    val node = LBIRSlaveNode[T](Seq(
        LBIRSlaveParameters(
            Seq(AddressSet(0,8)), 
            Nil,
            LBIRBundleParameters(
                genT,
                numBeats
            ))
    ))
    lazy val module = new LazyModuleImp(this) { 
        val io = IO(new Bundle {
            val error = Output(Bool())
        })
        val in = node.in.head._1
        val errorReg = RegInit(false.B)
        when (in.fire) {
            errorReg := in.bits(0).asInstanceOf[UInt] =/= 1.U.asInstanceOf[UInt]
        }
        io.error := errorReg
        dontTouch(in)
        dontTouch(io.error)
        dontTouch(errorReg)
     }
}

class LBIRAdderExample[T <: Bits](genT: T, numBeats: Int)(implicit p: Parameters) extends LazyModule {
    val snode = LBIRSlaveNode[T](Seq(
        LBIRSlaveParameters(
            Seq(AddressSet(0, 8)), 
            Nil,
            LBIRBundleParameters(
                genT,
                numBeats
            )
        )
    ))
    val mnode = LBIRMasterNode[T](Seq(
        LBIRMasterParameters(
            "Adder", 
            Nil,
            LBIRBundleParameters(
                genT,
                numBeats
            )
        )
    ))
    lazy val module = new LazyModuleImp(this) {
        val in = snode.in.head._1
        val out = mnode.out.head._1
        out := in
    }
}

// Top-level demo module
class LBIRDemoTop(implicit p: Parameters) extends LazyModule {
    val source = LazyModule(new LBIRDemoSource(UInt(4.W), 3))
    val adder = LazyModule(new LBIRAdderExample(UInt(4.W), 3))
    val sink = LazyModule(new LBIRDemoSink(UInt(4.W), 3))
    
    // Very important connection
    adder.snode := source.node
    sink.node := adder.mnode

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