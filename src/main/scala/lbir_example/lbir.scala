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

// Demo source SoC component
class LBIRDemoSource[T <: Bits](genT: T, numBeats: Int)(implicit p: Parameters) extends LazyModule {
    val node = LBIRMasterNode[T](Seq(
        LBIRMasterPortParameters[T](Seq(
            LBIRMasterParameters(
                "Demo source",
                Nil,
            )
        ))
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
        LBIRSlavePortParameters(
            slaves = Seq(
                LBIRSlaveParameters(
                    Seq(AddressSet(0,8)),
                    Nil,
                )
            ),
            bundleParams = LBIRBundleParameters(
                genT,
                numBeats
            )

        )
    ))
    lazy val module = new LazyModuleImp(this) { 
        val io = IO(new Bundle {
            val error = Output(Bool())
        })
        val in = node.in.head._1
        val errorReg = RegInit(false.B)
        when (in.fire) {
            errorReg := in.bits(0).asInstanceOf[UInt] =/= 3.U.asInstanceOf[UInt]
        }
        io.error := errorReg
        dontTouch(in)
        dontTouch(io.error)
        dontTouch(errorReg)
     }
}

class LBIRAdderExample[T <: Bits with Num[T]](genT: T, numBeatsIn: Int, numBeatsOut: Int)(implicit p: Parameters) extends LazyModule {
    val snode = LBIRSlaveNode[T](Seq(
        LBIRSlavePortParameters[T](
            slaves = Seq(
                LBIRSlaveParameters(
                    Seq(AddressSet(0,8)),
                    Nil
                )),
            bundleParams = LBIRBundleParameters[T](
                genT,
                numBeatsIn
            )
        )
    ))
    val mnode = LBIRMasterNode[T](Seq(
        LBIRMasterPortParameters(Seq(
            LBIRMasterParameters(
                "Adder",
                Nil
            )  
        ))
    ))
    lazy val module = new LazyModuleImp(this) {
        val in = snode.in.head._1
        val out = mnode.out.head._1
        require(out.bits.length == 1)
        val inBitsReg = Reg(Vec(numBeatsIn, genT))
        when (in.fire) {
            inBitsReg := in.bits
        }
        out.bits.head := inBitsReg.reduce(_ + _)
        dontTouch(in)
        dontTouch(out)
        dontTouch(inBitsReg)
    }
}

// Top-level demo module
class LBIRDemoTop(implicit p: Parameters) extends LazyModule {
    val source = LazyModule(new LBIRDemoSource(UInt(4.W), 3))
    val adder = LazyModule(new LBIRAdderExample(UInt(4.W), 3, 1))
    val sink = LazyModule(new LBIRDemoSink(UInt(4.W), 1))
    
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