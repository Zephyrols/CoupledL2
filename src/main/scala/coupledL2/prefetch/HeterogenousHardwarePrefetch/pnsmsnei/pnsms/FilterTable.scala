package coupledL2.prefetch.hhp

import utility.XSPerfAccumulate
import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import coupledL2.prefetch._

class FilterTableEntry(implicit p: Parameters) extends HHPBundle {
  val valid = Bool()
  val lru = UInt(log2Ceil(ftTableWayNum).W)
  val pn = UInt(log2PageNum.W)
  val pos = UInt(log2Ceil(fteOffsetNum).W)
  val offset = Vec(fteOffsetNum - 1, UInt(log2BlockNum.W))
  val lastAccessTime = UInt(timeStampWidth.W)
  

  def reset() = {
    valid := false.B
    lru := (ftTableWayNum - 1).U
    pn := DontCare
    pos := 0.U
    offset := DontCare
    lastAccessTime := DontCare
  }

  def isMatch(pn: UInt): Bool = {
    valid && this.pn === pn(log2PageNum - 1, 0)
  }

  def isFull(): Bool = {
    valid && pos === (fteOffsetNum - 1).U
  }

  def isExpired(curTimstamp: UInt): Bool = {
    val timeStamp = curTimstamp & timeStampMask.U

    (!valid || (timeStamp - lastAccessTime) > actRetireWin.U)
  }

  def isOffsetRepeat(newOffset: UInt): Bool = {
    offset.zipWithIndex.map({ case (offset, index) =>
      Mux(index.U < pos, newOffset === offset, false.B)
    }).reduce(_ || _)
  }

  def toATPattern(newOffset: UInt): UInt = {
    val pattern = offset.map(offsetToPattern(_)).reduce(_ | _) | offsetToPattern(newOffset)

    pattern.asTypeOf(UInt(patternWidth.W))
  }
}

class FilterTableIO(implicit p: Parameters) extends HHPBundle {
  val insertEntryIO = Flipped(ValidIO(new InsertFTIO))
  val globalTimer = Input(UInt(timeStampWidth.W))
  val toATIO = ValidIO(new FTtoATIO)
}

class FilterTable(implicit p: Parameters) extends HHPModule {
  val io = IO(new FilterTableIO)

  val table = RegInit(
    VecInit.fill(ftTableWayNum)({
      val initFTEntry = Wire(new FilterTableEntry)
      initFTEntry.reset()
      initFTEntry
    })
  )

  // default output start ======================================
  io.toATIO.valid := false.B
  io.toATIO.bits := 0.U.asTypeOf(new FTtoATIO)
  // default output end ========================================


  def updateLru(entryIndex: UInt) = {
    table.foreach(e => {
      when(e.valid && e.lru < table(entryIndex).lru) {
        e.lru := e.lru + 1.U
      }
    })
    table(entryIndex).lru := 0.U
  }

  def findMatchIdx(pn: UInt): (Bool, UInt) = {
    val hasMatch = table.exists(e => e.isMatch(pn))
    val matchIdx = table.indexWhere(e => e.isMatch(pn))
    (hasMatch, matchIdx)
  }

  def findVictimIdx(): (Bool, UInt) = {
    val victimIdx = table.indexWhere(e => (e.lru === (ftTableWayNum - 1).U))
    val victimValid = table(victimIdx).valid
    (victimValid, victimIdx)
  }


  // internal wires start =======================================
  val maxLru = (ftTableWayNum - 1).U
  val enableInsert = io.insertEntryIO.valid
  val newTimestamp = io.globalTimer
  val newPn = io.insertEntryIO.bits.pn
  val newOffset = io.insertEntryIO.bits.offset

  val (hasMatch, matchIdx) = findMatchIdx(newPn)
  val matchEntry = table(matchIdx)
  val isExpired = matchEntry.isExpired(newTimestamp)
  val isOffsetFull = matchEntry.isFull()
  val isOffsetRepeat = matchEntry.isOffsetRepeat(newOffset)
  val toATPattern = matchEntry.toATPattern(newOffset)

  val (victimValid, victimIdx) = findVictimIdx()
  
  // internal wires end =========================================

  // sequential logic start =====================================
  when(enableInsert) {
    when(hasMatch) {
      when(!isExpired){
        table(matchIdx).lastAccessTime := newTimestamp
        when(!isOffsetRepeat) {
          when(!isOffsetFull) {
            table(matchIdx).offset(matchEntry.pos) := newOffset
            table(matchIdx).pos := matchEntry.pos + 1.U
            updateLru(matchIdx)
          } otherwise {
            io.toATIO.valid := true.B
            io.toATIO.bits.newPattern := toATPattern
            table(matchIdx).reset()
          }
        } otherwise {
          updateLru(matchIdx)
        }
      }otherwise{
        table(matchIdx).pos := 1.U
        table(matchIdx).offset(0) := newOffset
        table(matchIdx).lastAccessTime := newTimestamp
        updateLru(matchIdx)
      }
    }otherwise{
      table(victimIdx).valid := true.B
      table(victimIdx).pn := newPn
      table(victimIdx).offset(0) := newOffset
      table(victimIdx).pos := 1.U
      table(victimIdx).lastAccessTime := newTimestamp
      updateLru(victimIdx)
    }
  }
  // sequential logic end =======================================

  // insert = insertHit + insertMiss
  // needAlloc = insertMiss + insertHitExpired
  // insertHitExpired = needAlloc + insertHit - insert
  // replace = alloc -
  XSPerfAccumulate("ftInsert", enableInsert)
  XSPerfAccumulate("ftInsertHit", enableInsert && hasMatch)
  XSPerfAccumulate("ftReplace", enableInsert && (!hasMatch && victimValid))
  XSPerfAccumulate("ftToAT", io.toATIO.valid)
}
