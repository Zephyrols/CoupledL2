package coupledL2.prefetch.hhp

import utility.XSPerfAccumulate
import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import coupledL2.prefetch._

class RecentPageTableEntry(implicit p: Parameters) extends HHPBundle {
  val valid = Bool()
  val lru = UInt(log2Ceil(rpTableWayNum).W)
  val pn = UInt(log2PageNum.W)
  val pattern = UInt(patternWidth.W)
  
  def reset() = {
    valid := false.B
    lru := (rpTableWayNum - 1).U
    pn := DontCare
    pattern := DontCare
  }
}

class RecentPageTable(implicit p: Parameters) extends HHPModule {
  val io = IO(new RecentPageTableIO)

  val table = RegInit(
    VecInit.fill(rpTableWayNum)({
      val initRPEntry = Wire(new RecentPageTableEntry)
      initRPEntry.reset()
      initRPEntry
    })
  )

  def updateLru(idx: UInt) = {
    table.foreach({ e =>
      when(e.valid && e.lru < table(idx).lru) {
        e.lru := e.lru + 1.U
      }
    })
    table(idx).lru := 0.U
  }

  def isCloseToNewPage(e: RecentPageTableEntry): Bool = {
    val distance = Mux(inputPn > e.pn, inputPn - e.pn,e.pn - inputPn)
    val isClose = distance <= pageDistance.U
    return e.valid && e.pn =/= inputPn && isClose
  }

  val inputValid = io.req.valid
  val inputCacheHit = io.req.bits.cacheHit
  val inputPn = io.req.bits.pn
  val inputPattern = io.req.bits.pattern
  val hasMatch = table.exists(e => e.valid && e.pn === inputPn)
  val matchIdx = table.indexWhere(e => e.valid && e.pn === inputPn)
  val maxLruIdx = table.indexWhere(e => e.lru === (rpTableWayNum - 1).U)
  val newPattern = Mux(hasMatch, table(matchIdx).pattern | inputPattern, inputPattern)

  val maxSimalarity = table.map(e =>
    Mux(isCloseToNewPage(e), PopCount(e.pattern & newPattern), 0.U)
  ).reduceLeft(_ max _)

  val maxSimalarIdx = table.indexWhere(e =>
    (isCloseToNewPage(e) && PopCount(e.pattern & newPattern) === maxSimalarity)
  )

  io.resp := 0.U.asTypeOf(new OutputCheckUpdateRPIO)

  when(inputValid) {
    when(hasMatch) {
      table(matchIdx).pattern := newPattern
      updateLru(matchIdx)
      when(maxSimalarity >= (similarThreshold - 1).U) {
        io.resp.hasSmilar := true.B
        io.resp.pattern := table(maxSimalarIdx).pattern | newPattern
      }
    }.otherwise {
      table(maxLruIdx).valid := true.B
      table(maxLruIdx).pn := inputPn
      table(maxLruIdx).pattern := newPattern
      updateLru(maxLruIdx)
    }
  }

  XSPerfAccumulate("rptInsert", inputValid)
  XSPerfAccumulate("rptInsertHit", inputValid && hasMatch)
  XSPerfAccumulate("rptFoundSimalar", inputValid && io.resp.hasSmilar)
}
