package axis_example

import chipsalliance.rocketchip.config.{Config, Parameters}
import chisel3._
import chisel3.util._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.amba.axis._

class AXISDemoSource(implicit p: Parameters) extends LazyModule {
    val node = AXISMasterNode(Seq(
        AXISMasterPortParameters.v1(
            masters = Seq(AXISMasterParameters.v1(
                name = "MySource",
                emitsSizes = TransferSizes(1,1)
            )),
            beatBytes = Some(1)
        )
    ))

    lazy val module = new LazyModuleImp(this) {
        val out = node.out.head._1
        out := 0.U.asTypeOf(out)
        dontTouch(out)
    }
}

class AXISDemoSink(implicit p: Parameters) extends LazyModule {
    val node = AXISSlaveNode(Seq(
        AXISSlavePortParameters.v1(Seq(
            AXISSlaveParameters.v1(
                name = "mySink",
                supportsSizes = TransferSizes(1,1)
            )
        ))
    ))

    lazy val module = new LazyModuleImp(this) {
        val in = node.in.head._1
        dontTouch(in)
    }
}

// Top-level demo module
class AXISDemoTop(implicit p: Parameters) extends LazyModule {
    val m1 = LazyModule(new AXISDemoSource)
    val m2 = LazyModule(new AXISDemoSink)
    // Very important connection
    m2.node := m1.node
    // etc
    lazy val module = new LazyModuleImp(this) { 
     }
}

object main extends App {
    implicit val p: Parameters = Parameters.empty
    val lm = LazyModule(
        new AXISDemoTop()
    )
    (new chisel3.stage.ChiselStage).run(Seq(
        chisel3.stage.ChiselGeneratorAnnotation(
            () => lm.module
        )
    ))
}