package fir_example

import chipsalliance.rocketchip.config.{Config, Parameters}
import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.stage.ChiselStage
import chisel3.util.random.FibonacciLFSR
import freechips.rocketchip.diplomacy.{SimpleNodeImp, RenderedEdge, ValName, SourceNode,
                                       NexusNode, SinkNode, LazyModule, LazyModuleImp}

case class UpwardParam[T <: Data with Num[T]](bitwidth: Int, genT: T, coeffs: Seq[T])
case class DownwardParam[T <: Data with Num[T]](bitwidth: Int, genT: T, coeffs: Seq[T])
case class EdgeParam[T <: Data with Num[T]](bitwidth: Int, genT: T, coeffs: Seq[T])

class FIRNodeImp[T <: Bits with Num[T]] extends SimpleNodeImp[DownwardParam[T], UpwardParam[T], EdgeParam[T], T] {
  def edge(pd: DownwardParam[T], pu: UpwardParam[T], p: Parameters, sourceInfo: SourceInfo) = {
    if (pd.bitwidth < pu.bitwidth) EdgeParam(pd.bitwidth, pd.genT, pd.coeffs) else EdgeParam(pu.bitwidth, pu.genT, pu.coeffs)
  }
  def bundle(e: EdgeParam[T]) = e.genT
  def render(e: EdgeParam[T]) = RenderedEdge("blue", s"width = ${e.bitwidth}")
}

class FIRDriverNode[T <: Bits with Num[T]](dp: DownwardParam[T])(implicit valName: ValName)
  extends SourceNode(new FIRNodeImp[T])(Seq(dp))

class FIRMonitorNode[T <: Bits with Num[T]](up: UpwardParam[T])(implicit valName: ValName)
  extends SinkNode(new FIRNodeImp[T])(Seq(up))

class FIRNode[T <: Bits with Num[T]](dFn: Seq[DownwardParam[T]] => DownwardParam[T],
                uFn: Seq[UpwardParam[T]] => UpwardParam[T])(implicit valName: ValName)
  extends NexusNode(new FIRNodeImp[T])(dFn, uFn)

class FIR[T <: Bits with Num[T]](implicit p: Parameters) extends LazyModule {
  val node = new FIRNode[T] (
    { case dps: Seq[DownwardParam[T]] =>
      require(dps.forall(dp => dp.bitwidth == dps.head.bitwidth), "inward, downward adder bitwidths must be equivalent")
      dps.head
    },
    { case ups: Seq[UpwardParam[T]] =>
      require(ups.forall(up => up.bitwidth == ups.head.bitwidth), "outward, upward adder bitwidths must be equivalent")
      ups.head
    }
  )
  lazy val module = new LazyModuleImp(this) {
    require(node.in.size == 1, s"Node.in.size == ${node.in.size}")
    val bitwidth = node.edges.in.head.bitwidth
    val zs = Reg(Vec(node.edges.in.head.coeffs.length, node.edges.in.head.genT))
    dontTouch(node.in.head._1)
    zs(0) := node.in.head._1
    for (i <- 1 until node.edges.in.head.coeffs.length) {
        zs(i) := zs(i - 1)
    }
    val products = VecInit.tabulate(node.edges.in.head.coeffs.length)(i => zs(i) * node.edges.in.head.coeffs(i))
    node.out.head._1 := products.reduce(_ + _)
    dontTouch(node.out.head._1)
  }

  override lazy val desiredName = "FIR"
}

class FIRDriver[T <: Bits with Num[T]](bitwidth: Int, genT: T, coeffs: Seq[T])(implicit p: Parameters) extends LazyModule {
  val node = new FIRDriverNode[T](DownwardParam(bitwidth, genT, coeffs))

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

class FIRMonitor[T <: Bits with Num[T]](bitwidth: Int, genT: T, coeffs: Seq[T])(implicit p: Parameters) extends LazyModule {
  val node = new FIRMonitorNode[T](UpwardParam(bitwidth, genT, coeffs))

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
  val fir = LazyModule(new FIR[UInt])
  // 8 will be the downward-traveling widths from our drivers
  val driver =  LazyModule(new FIRDriver[UInt](bitwidth = 8, UInt(8.W), coeffs=Seq(1.U,2.U,3.U))) 
  // 4 will be the upward-traveling width from our monitor
  val monitor = LazyModule(new FIRMonitor[UInt](bitwidth = 4, UInt(4.W), coeffs=Seq(1.U,2.U,3.U)))

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
        