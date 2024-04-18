package axis_example
import chipsalliance.rocketchip.config.{Config, Parameters}
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.amba.axis._
import freechips.rocketchip.regmapper._


class MyManyDynamicElementVecFir(length: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(8.W))
    val valid = Input(Bool())
    val out = Output(UInt(8.W))
    val consts = Input(Vec(length, UInt(8.W)))
  })
  
  // Such concision! You'll learn what all this means later.
  val taps = Seq(io.in) ++ Seq.fill(io.consts.length - 1)(RegInit(0.U(8.W)))
  taps.zip(taps.tail).foreach { case (a, b) => when (io.valid) { b := a } }

  io.out := taps.zip(io.consts).map { case (a, b) => a * b }.reduce(_ + _)
}

class FIRFilter(val nFilters: Int, val nTaps: Int)(implicit p: Parameters) extends LazyModule {
    val inode = AXISSlaveNode(Seq(
        AXISSlavePortParameters.v1(Seq(
            AXISSlaveParameters.v1(
                name = "FIRFilter_input",
                supportsSizes = TransferSizes(1,1)
            )
        )
        )
    ))
    val onode = AXISMasterNode(Seq(
        AXISMasterPortParameters.v1(Seq(
            AXISMasterParameters.v1(
                name = "FIRFilter_output",
                emitsSizes = TransferSizes(1,1)
            )
        ))
    ))

    lazy val module = new LazyModuleImp(this) {
        val in = inode.in.head._1
        val out = onode.out.head._1

        // make registers to store taps
        val taps = Reg(Vec(nFilters, Vec(nTaps, UInt(8.W))))

        // memory map the taps, plus the first address is a read-only field that says how many filter lanes there are
        /*val mmap = Seq(
            RegField.r(64, nFilters.U, RegFieldDesc("nFilters", "Number of filter lanes"))
        ) ++ taps.flatMap(_.map(t => RegField(8, t, RegFieldDesc("tap", "Tap"))))*/

        // generate the hardware for the memory interface
        // in this class, regmap is abstract (unimplemented). mixing in something like AXI4HasCSR or TLHasCSR
        // will define regmap for the particular memory interface
        //regmap(mmap.zipWithIndex.map({case (r, i) => i * 8 -> Seq(r)}): _*)

        // make the FIR lanes and connect inputs and taps
        val outs = for (i <- 0 until nFilters) yield {
            val fir = Module(new MyManyDynamicElementVecFir(nTaps))
            
            fir.io.in := in.bits.data((i+1)*8, i*8)
            fir.io.valid := in.valid && out.ready
            fir.io.consts := taps(i)            
            fir.io.out
        }

        val output = if (outs.length == 1) {
            outs.head
        } else {
            outs.reduce((x: UInt, y: UInt) => Cat(y, x))
        }

        out.bits.data := output
        in.ready  := out.ready
        out.valid := in.valid
    }
}

object firMain extends App {
    implicit val p: Parameters = Parameters.empty
    val lm = LazyModule(
        new FIRFilter(4, 3)
    )
    (new chisel3.stage.ChiselStage).run(Seq(
        chisel3.stage.ChiselGeneratorAnnotation(
            () => lm.module
        )
    ))
}