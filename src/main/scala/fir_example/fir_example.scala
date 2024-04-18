package fir_example

import chipsalliance.rocketchip.config.{Config, Parameters}
import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.stage.ChiselStage
import chisel3.util.random.FibonacciLFSR
import freechips.rocketchip.diplomacy.{SimpleNodeImp, RenderedEdge, ValName, SourceNode,
                                       NexusNode, SinkNode, LazyModule, LazyModuleImp}

case class UpwardParam(bitwidth: Int)
case class DownwardParam(bitwidth: Int)
case class EdgeParam(bitwidth: Int)

object FIRNodeImp extends SimpleNodeImp[DownwardParam, UpwardParam, EdgeParam, UInt] {
  def edge(pd: DownwardParam, pu: UpwardParam, p: Parameters, sourceInfo: SourceInfo) = {
    if (pd.bitwidth < pu.bitwidth) EdgeParam(pd.bitwidth) else EdgeParam(pu.bitwidth)
  }
  def bundle(e: EdgeParam) = UInt(e.bitwidth.W)
  def render(e: EdgeParam) = RenderedEdge("blue", s"width = ${e.bitwidth}")
}

class FIRDriverNode(bitwidth: DownwardParam)(implicit valName: ValName)
  extends SourceNode(FIRNodeImp)(Seq(bitwidth))

class FIRMonitorNode(bitwidth: UpwardParam)(implicit valName: ValName)
  extends SinkNode(FIRNodeImp)(Seq(bitwidth))

class FIRNode(dFn: Seq[DownwardParam] => DownwardParam,
                uFn: Seq[UpwardParam] => UpwardParam)(implicit valName: ValName)
  extends NexusNode(FIRNodeImp)(dFn, uFn)

class FIR(implicit p: Parameters) extends LazyModule {
  val node = new FIRNode (
    { case dps: Seq[DownwardParam] =>
      require(dps.forall(dp => dp.bitwidth == dps.head.bitwidth), "inward, downward adder bitwidths must be equivalent")
      dps.head
    },
    { case ups: Seq[UpwardParam] =>
      require(ups.forall(up => up.bitwidth == ups.head.bitwidth), "outward, upward adder bitwidths must be equivalent")
      ups.head
    }
  )
  lazy val module = new LazyModuleImp(this) {
    require(node.in.size == 1, s"Node.in.size == ${node.in.size}")
    val coeffs = Seq(0.U, 1.U, 2.U)
    val zs = Reg(Vec(coeffs.length, UInt(node.edges.out.head.bitwidth.W)))
    dontTouch(node.in.head._1)
    zs(0) := node.in.head._1
    for (i <- 1 until coeffs.length) {
        zs(i) := zs(i - 1)
    }
    val products = VecInit.tabulate(coeffs.length)(i => zs(i) * coeffs(i))
    node.out.head._1 := products.reduce(_ +& _)
    dontTouch(node.out.head._1)
  }

  override lazy val desiredName = "FIR"
}

class FIRDriver(bitwidth: Int)(implicit p: Parameters) extends LazyModule {
  val node = new FIRDriverNode(DownwardParam(bitwidth))

  lazy val module = new LazyModuleImp(this) {
    // check that node parameters converge after negotiation
    val negotiatedWidths = node.edges.out.map(_.bitwidth)
    require(negotiatedWidths.forall(_ == negotiatedWidths.head), "outputs must all have agreed on same width")
    val finalWidth = negotiatedWidths.head

    // generate random addend (notice the use of the negotiated width)
    val randomAddend = FibonacciLFSR.maxPeriod(finalWidth)

    // drive signals
    node.out.head._1 := randomAddend
    dontTouch(node.out.head._1)
  }

  override lazy val desiredName = "FIRDriver"
}

class FIRMonitor(bitwidth: Int)(implicit p: Parameters) extends LazyModule {
  val node = new FIRMonitorNode(UpwardParam(bitwidth))

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val error = Output(Bool())
    })
    printf(p"${node.in.head._1}")

    // basic correctness checking (add sw checker here)
    io.error := false.B
  }

  override lazy val desiredName = "FIRMonitor"
}

class FIRTestHarness()(implicit p: Parameters) extends LazyModule {
  val fir = LazyModule(new FIR)
  // 8 will be the downward-traveling widths from our drivers
  val driver =  LazyModule(new FIRDriver(bitwidth = 8)) 
  // 4 will be the upward-traveling width from our monitor
  val monitor = LazyModule(new FIRMonitor(bitwidth = 4))

  monitor.node := fir.node := driver.node

  lazy val module = new LazyModuleImp(this) {
    when(monitor.module.io.error) {
      printf("something went wrong")
    }
    dontTouch(monitor.module.io.error)
  }

  override lazy val desiredName = "FIRTestHarness"
}

object FIRExample extends App {
    (new chisel3.stage.ChiselStage).run(Seq(
        chisel3.stage.ChiselGeneratorAnnotation(
            () =>  LazyModule(new FIRTestHarness()(Parameters.empty)).module
        )
    ))
}
        