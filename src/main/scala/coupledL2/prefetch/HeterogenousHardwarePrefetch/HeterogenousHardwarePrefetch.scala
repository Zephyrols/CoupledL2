package coupledL2.prefetch

import utility.XSPerfAccumulate
import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import coupledL2.HasCoupledL2Parameters
import coupledL2.prefetch._
import coupledL2.prefetch.hhp._

class HHPPatternBufferQueue(implicit p: Parameters) extends HHPModule {
  val io = IO(new Bundle {
    val enq = Flipped(DecoupledIO(new OutputPrefIO))
    val deq = DecoupledIO(new OutputPrefIO)
  })
  /*  Here we implement a queue that
   *  1. is pipelined  2. flows
   *  3. always has the latest reqs, which means 
   *     the queue is always ready for enq and deserting the eldest ones
   */
  val queue = RegInit(VecInit(Seq.fill(patternBufferQueueSize)(0.U.asTypeOf(new OutputPrefIO))))
  val valids = RegInit(VecInit(Seq.fill(patternBufferQueueSize)(false.B)))
  val idxWidth = log2Up(patternBufferQueueSize)
  val head = RegInit(0.U(idxWidth.W))
  val tail = RegInit(0.U(idxWidth.W))
  val empty = head === tail && !valids.last
  val full = head === tail && valids.last

  when(!empty && io.deq.ready) {
    valids(head) := false.B
    head := head + 1.U
  }

  when(io.enq.valid) {
    queue(tail) := io.enq.bits
    valids(tail) := !empty || !io.deq.ready
    tail := tail + (!empty || !io.deq.ready).asUInt
    when(full && !io.deq.ready) {
      head := head + 1.U
    }
  }

  io.enq.ready := true.B
  io.deq.valid := !empty || io.enq.valid
  io.deq.bits := Mux(empty, io.enq.bits, queue(head))

  val overflow = io.enq.fire && full && !io.deq.fire

  XSPerfAccumulate("hhpPBQEnqPtn", io.enq.fire)
  XSPerfAccumulate("hhpPBQOflPtn", overflow)
  XSPerfAccumulate("hhpPBQDeqPtn", io.deq.fire)

  XSPerfAccumulate("hhpPBQEnqPft", Mux(io.enq.fire, PopCount(io.enq.bits.pattern), 0.U))
  XSPerfAccumulate("hhpPBQOflPft", Mux(overflow,    PopCount(queue(tail).pattern), 0.U))
  XSPerfAccumulate("hhpPBQDeqPft", Mux(io.deq.fire, PopCount(io.deq.bits.pattern), 0.U))
}

class HeterogenousHardwarePrefetch(implicit p: Parameters) extends HHPModule {
  val io = IO(new Bundle() {
    val train = Flipped(DecoupledIO(new PrefetchTrain))
    val req = DecoupledIO(new PrefetchReq)
    val resp = Flipped(DecoupledIO(new PrefetchResp))
  })

  val pnsmsnei           = Module(new PnSMSNei)
  val patternBufferQueue = Module(new HHPPatternBufferQueue)

  val sendingWait     = RegInit(Bool(), false.B)
  val pbqDeq          = !sendingWait && patternBufferQueue.io.deq.fire
  
  val sendingReg      = RegInit(0.U.asTypeOf(new OutputPrefIO))
  val sendingBaseAddr = sendingReg.baseAddr
  val sendingPattern  = sendingReg.pattern
  val sendingNeedT    = sendingReg.needT
  val sendingSource   = sendingReg.source
  val sendingPfSource = sendingReg.pfSource

  val sendingBlock    = PriorityEncoder(sendingPattern)
  val sendingBlockOH  = UIntToOH(sendingBlock, patternWidth)
  val sendingAddr     = sendingBaseAddr + (sendingBlock << log2BlockSize.U)
  val (sendingTag, sendingSet, _) = parseFullAddress(sendingAddr)

  when(!sendingWait){
    when(patternBufferQueue.io.deq.fire){
      sendingWait := true.B
      sendingReg  := patternBufferQueue.io.deq.bits
    }
  }.otherwise{
    when(io.req.fire){
      sendingPattern := sendingPattern & ~sendingBlockOH
      when(PopCount(sendingPattern) === 1.U){
        sendingWait := false.B
      }
    }
  }

  // send train to pnsmsnei
  pnsmsnei.io.train.valid         := io.train.fire
  pnsmsnei.io.train.bits.addr     := io.train.bits.addr
  pnsmsnei.io.train.bits.cacheHit := io.train.bits.hit
  pnsmsnei.io.train.bits.needT    := io.train.bits.needT
  pnsmsnei.io.train.bits.source   := io.train.bits.source

  // enqueue when pnsmsnei req valid
  patternBufferQueue.io.enq.valid := pnsmsnei.io.sendPref.valid
  patternBufferQueue.io.enq.bits  := pnsmsnei.io.sendPref.bits
  
  // get the queue head when not waitReqSending
  patternBufferQueue.io.deq.ready := !sendingWait

  val prefetchReqValid = sendingWait && (sendingPattern & sendingBlockOH).orR
  val prefetchReq = WireInit(0.U.asTypeOf(new PrefetchReq))
  prefetchReq.tag := sendingTag
  prefetchReq.set := sendingSet
  prefetchReq.needT := sendingNeedT
  prefetchReq.source := sendingSource
  prefetchReq.pfSource := sendingPfSource

  io.req.valid := prefetchReqValid
  io.req.bits := prefetchReq
  io.train.ready := true.B
  io.resp.ready := true.B

  XSPerfAccumulate("hhp_req_total", io.req.valid)
}