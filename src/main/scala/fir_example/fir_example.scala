package fir_example

import chipsalliance.rocketchip.config.{Config, Parameters}
import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.stage.ChiselStage
import chisel3.util.random.FibonacciLFSR
import freechips.rocketchip.diplomacy.{SimpleNodeImp, RenderedEdge, ValName, SourceNode,
                                       NexusNode, SinkNode, LazyModule, LazyModuleImp}

case class UpwardParam(bitwidth: Int, signed: Boolean)
case class DownwardParam(bitwidth: Int, signed: Boolean)
case class EdgeParam(bitwidth: Int, signed: Boolean)

class FIRNodeImp[T <: Bits with Num[T]] extends SimpleNodeImp[DownwardParam, UpwardParam, EdgeParam, T] {
  def edge(pd: DownwardParam, pu: UpwardParam, p: Parameters, sourceInfo: SourceInfo) = {
    require(pd.signed == pu.signed)
    if (pd.bitwidth < pu.bitwidth) EdgeParam(pd.bitwidth, pd.signed) else EdgeParam(pu.bitwidth, pu.signed)
  }
  def bundle(e: EdgeParam) = if (e.signed) SInt(e.bitwidth.W).asInstanceOf[T] else UInt(e.bitwidth.W).asInstanceOf[T]
  def render(e: EdgeParam) = RenderedEdge("blue", s"width = ${e.bitwidth}")
}

class FIRDriverNode[T <: Bits with Num[T]](bitwidth: DownwardParam)(implicit valName: ValName)
  extends SourceNode(new FIRNodeImp[T])(Seq(bitwidth))

class FIRMonitorNode[T <: Bits with Num[T]](bitwidth: UpwardParam)(implicit valName: ValName)
  extends SinkNode(new FIRNodeImp[T])(Seq(bitwidth))

class FIRNode[T <: Bits with Num[T]](dFn: Seq[DownwardParam] => DownwardParam,
                uFn: Seq[UpwardParam] => UpwardParam)(implicit valName: ValName)
  extends NexusNode(new FIRNodeImp[T])(dFn, uFn)

class FIR[T <: Bits with Num[T]](implicit p: Parameters) extends LazyModule {
  val node = new FIRNode (
    { case dps: Seq[DownwardParam] =>
      require(dps.forall(dp => dp.bitwidth == dps.head.bitwidth), "inward, downward adder bitwidths must be equivalent")
      require(dps.forall(dp => dp.signed == dps.head.signed))
      dps.head
    },
    { case ups: Seq[UpwardParam] =>
      require(ups.forall(up => up.bitwidth == ups.head.bitwidth), "outward, upward adder bitwidths must be equivalent")
      require(ups.forall(up => up.signed == ups.head.signed))
      ups.head
    }
  )
  lazy val module = new LazyModuleImp(this) {
    require(node.in.size == 1, s"Node.in.size == ${node.in.size}")
    val signed = node.edges.in.head.signed
    val bitwidth = node.edges.in.head.bitwidth
    val coeffs = if(signed) Seq(0.S, 1.S, 2.S).map(_.asInstanceOf[T]) else Seq(0.U, 1.U, 2.U).map(_.asInstanceOf[T])
    val zs = Reg(Vec(coeffs.length, if(signed) SInt(bitwidth.W).asInstanceOf[T] else UInt(bitwidth.W).asInstanceOf[T]))
    dontTouch(node.in.head._1)
    zs(0) := node.in.head._1
    for (i <- 1 until coeffs.length) {
        zs(i) := zs(i - 1)
    }
    val products = VecInit.tabulate(coeffs.length)(i => zs(i) * coeffs(i))
    node.out.head._1 := products.reduce(_ + _)
    dontTouch(node.out.head._1)
  }

  override lazy val desiredName = "FIR"
}

class FIRDriver[T <: Bits with Num[T]](bitwidth: Int, signed: Boolean)(implicit p: Parameters) extends LazyModule {
  val node = new FIRDriverNode[T](DownwardParam(bitwidth, signed))

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

class FIRMonitor[T <: Bits with Num[T]](bitwidth: Int, signed: Boolean)(implicit p: Parameters) extends LazyModule {
  val node = new FIRMonitorNode[T](UpwardParam(bitwidth, signed))

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
  val driver =  LazyModule(new FIRDriver[UInt](bitwidth = 8, signed=false)) 
  // 4 will be the upward-traveling width from our monitor
  val monitor = LazyModule(new FIRMonitor[UInt](bitwidth = 4, signed=false))

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
        