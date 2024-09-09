package coupledL2.prefetch.hhp

import utility.XSPerfAccumulate
import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import coupledL2.prefetch._

class AccumulationTableEntry(implicit p: Parameters) extends HHPBundle {
  val valid = Bool()
  val rrpv = UInt(2.W)
  val pn = UInt(log2PageNum.W)
  val pattern = UInt(patternWidth.W)
  val lastAccessTime = UInt(timeStampWidth.W)
  
  def reset() = {
    valid := false.B
    rrpv := 3.U
    pn := DontCare
    pattern := DontCare
    lastAccessTime := DontCare
  }

  def isMatch(pn: UInt): Bool = {
    valid && this.pn === pn(log2PageNum - 1, 0)
  }

  def isExpired(curTimstamp: UInt): Bool = {
    val timeStamp = curTimstamp & timeStampMask.U
    (!valid || ((timeStamp - lastAccessTime) > actRetireWin.U))
  }

  def getMergedPattern(newPattern: UInt): UInt = {
    pattern | newPattern
  }
}

class AccumulationTable(implicit p: Parameters) extends HHPModule {
  val io = IO(new AccumulationTableIO)

  // Define an array to store table entries
  val table = RegInit(
    VecInit.fill(1<<log2SetNum, acTableWayNum)({
      val initACEntry = Wire(new AccumulationTableEntry)
      initACEntry.reset()
      initACEntry
    })
  )

  // default output start======================================================
  io.checkUpdateIO.resp := 0.U.asTypeOf(new OutputCheckUpdateATIO)
  io.toPTIO.valid := false.B
  io.toPTIO.bits := 0.U.asTypeOf(new ATtoPT)
  // default output end========================================================

  // find logic start========================================================
  def findMatchIdx(set: UInt, pn: UInt): (Bool,UInt) = {
    val hasMatch = table(set).exists(e => e.isMatch(pn))
    val matchIdx = table(set).indexWhere(e => e.isMatch(pn))
    
    (hasMatch, matchIdx)
  }
  // find logic end========================================================


  val maxRrpv = 3.U
  val newTimeStamp = io.globalTimer
  val enableUpdate = io.checkUpdateIO.req.valid
  val checkUpdatePn = io.checkUpdateIO.req.bits.pn
  val checkUpdateSet = io.checkUpdateIO.req.bits.set
  val checkUpdatePattern = io.checkUpdateIO.req.bits.pattern
  val (hasMatch, matchIdx) = findMatchIdx(checkUpdateSet, checkUpdatePn)
  val matchEntry = table(checkUpdateSet)(matchIdx)
  val isExpired = matchEntry.isExpired(newTimeStamp)
  val mergedPattern = matchEntry.getMergedPattern(checkUpdatePattern)
  
  val insertFromFT = io.fromFTIO.valid
  val insertFTPattern = io.fromFTIO.bits.newPattern

  // replace start ==========================================================
  def findVictimIdx(set: UInt): (Bool, UInt, UInt) = {
    val victimRrpv = table(set).map(e => e.rrpv).reduce(_ max _)
    val victimIdx = table(set).indexWhere(e => e.rrpv === victimRrpv)
    val victimValid = table(set)(victimIdx).valid
    
    (victimValid, victimIdx, victimRrpv)
  }

  def updateMetadata(isHit: Bool, set: UInt, way: UInt): Unit = {
    when(isHit) {
      table(set)(way).rrpv := 0.U
    }.otherwise {
      table(set)(way).rrpv := maxRrpv - 1.U
    }
  }

  val (victimValid, victimIdx, victimRrpv) = findVictimIdx(checkUpdateSet)
  // replace end ==========================================================

  when(enableUpdate) {
    when(hasMatch) {
      when(!isExpired) {
        // update entry
        when(table(checkUpdateSet)(matchIdx).pattern =/= mergedPattern) {
          table(checkUpdateSet)(matchIdx).lastAccessTime := newTimeStamp
        }
        table(checkUpdateSet)(matchIdx).pattern := mergedPattern
        updateMetadata(false.B, checkUpdateSet, matchIdx)

        io.checkUpdateIO.resp.hitBeforeInsertFT := true.B
        // io.checkUpdateIO.resp.pattern := mergedPattern
      }.otherwise {
        // send to PT
        io.toPTIO.valid := true.B
        io.toPTIO.bits.set := checkUpdateSet
        io.toPTIO.bits.pn := checkUpdatePn
        io.toPTIO.bits.newPattern := matchEntry.pattern
        // reset expiredEntry
        table(checkUpdateSet)(matchIdx).reset()
      }
    }.otherwise {
      // check FT
      when(insertFromFT) {
        when(victimValid){
          // send victimEntry to PT
          io.toPTIO.valid := true.B
          io.toPTIO.bits.set := checkUpdateSet
          io.toPTIO.bits.pn := table(checkUpdateSet)(victimIdx).pn
          io.toPTIO.bits.newPattern := table(checkUpdateSet)(victimIdx).pattern
        }
        // insert victimIdx
        table(checkUpdateSet)(victimIdx).valid := true.B
        table(checkUpdateSet)(victimIdx).pn := checkUpdatePn
        table(checkUpdateSet)(victimIdx).pattern := insertFTPattern
        table(checkUpdateSet)(victimIdx).lastAccessTime := newTimeStamp
        table(checkUpdateSet).foreach(e =>
          e.rrpv := e.rrpv + maxRrpv - victimRrpv
        )
        updateMetadata(false.B, checkUpdateSet, victimIdx)

        // when(io.fromFTIO.bits === checkUpdatePn) {
        // io.checkUpdateIO.resp.hitAfterInsertFT := true.B
        // io.checkUpdateIO.resp.pattern := insertFTPattern
        // }
      }
    }
  }

  // check = checkHit + checkMiss
  // checkHit = checkHitNotExpire + checkHitExpire
  // toPT = checkHitExpire + insert
  XSPerfAccumulate("atInsert", insertFromFT)
  XSPerfAccumulate("atInsertHit", insertFromFT && hasMatch)
  XSPerfAccumulate("atRepalce", insertFromFT && victimValid)
  XSPerfAccumulate("atCheckUpdate", enableUpdate)
  XSPerfAccumulate("atCheckUpdateHit", enableUpdate && hasMatch && !isExpired)
  XSPerfAccumulate("atToPT", io.toPTIO.valid)
}
