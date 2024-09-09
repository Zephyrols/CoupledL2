package coupledL2.prefetch.hhp

import utility.XSPerfAccumulate
import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import coupledL2.prefetch._


class PatternTableEntry(implicit p: Parameters) extends HHPBundle {
  val valid = Bool()
  val rrpv = UInt(2.W)
  val pn = UInt(log2PageNum.W)
  val pattern = UInt(patternWidth.W)
  val patternCandidate = UInt(patternWidth.W)
  

  def reset() = {
    valid := false.B
    rrpv := 3.U
    pn := DontCare
    pattern := DontCare
    patternCandidate := DontCare
  }

  def isMatch(pn: UInt): Bool = {
    valid && this.pn === pn(log2PageNum - 1, 0)
  }
}



class PatternTable(implicit p: Parameters) extends HHPModule {
  val io = IO(new PatternTableIO)

  val table = RegInit(
    VecInit.fill(1<<log2SetNum, ptTableWayNum)({
      val initPTEntry = Wire(new PatternTableEntry)
      initPTEntry.reset()
      initPTEntry
    })
  )

  // default output start======================================================
  io.checkUpdateIO.resp.hit := false.B
  io.checkUpdateIO.resp.pattern := 0.U
  // default output end========================================================

  // function logic start========================================================
  def findMatchIdx(set: UInt, pn: UInt): (Bool,UInt) = {
    val hasMatch = table(set).exists(e => e.isMatch(pn))
    val matchIdx = table(set).indexWhere(e => e.isMatch(pn))
    
    (hasMatch, matchIdx)
  }

  // def findInvalidIdx(set: UInt): (Bool, UInt) = {
  //   val hasInvalid = table(set).exists(e => !e.valid)
  //   val InvalidIdx = table(set).indexWhere(e => !e.valid)

  //   (hasInvalid, InvalidIdx)
  // }

  def PatternScore(pattern: UInt, demand: UInt): UInt = {
    val needed = pattern & demand
    val countDemand = PopCount(demand)
    val countNeeded = PopCount(needed)
    val countPrefetch = PopCount(pattern)

    val baseScore = Wire(UInt(64.W))
    val additionalScore = Wire(UInt(64.W))
    val accuracyScore = Wire(UInt(64.W))
    val score = Wire(UInt(64.W))

    when(needed =/= demand) {
      additionalScore := Mux(countDemand =/= 0.U, Cat(countDemand, 0.U(6.W)) / countDemand, 0.U)
      baseScore := 0.U
    } otherwise {
      additionalScore := 0.U
      baseScore := 1.U
    }

    accuracyScore := Mux(countPrefetch =/= 0.U, Cat(countPrefetch,0.U(6.W)) / countPrefetch, 0.U)

    score := Cat(baseScore, 0.U(12.W)) + Cat(accuracyScore,0.U(6.W)) + additionalScore

    score
  }
  // function logic end==========================================================

  // internal wires start======================================================
  val maxRrpv = 3.U(2.W)

  val checkUpateEnable = io.checkUpdateIO.req.valid
  val checkUpdatePn = io.checkUpdateIO.req.bits.pn
  val checkUpdateSet = io.checkUpdateIO.req.bits.set
  val (hasCheckUpdateMatch, checkUpdateMatchIdx) = findMatchIdx(checkUpdateSet, checkUpdatePn)
  val checkUpdateMatchEntry = table(checkUpdateSet)(checkUpdateMatchIdx)

  val insertEnable = io.fromATIO.valid
  val insertPn = io.fromATIO.bits.pn
  val insertSet = io.fromATIO.bits.set
  val insertPattern = io.fromATIO.bits.newPattern
  val (hasInsertMatch, insertMatchIdx) = findMatchIdx(insertSet, insertPn)
  // val (hasInvalid, invalidIdx) = findInvalidIdx(insertSet)
  val insertEntry = table(insertSet)(insertMatchIdx)

  val pat1 = insertEntry.pattern | insertPattern
  val pat2 = insertEntry.patternCandidate | insertPattern
  val pat3 = insertEntry.pattern & insertPattern
  val pat4 = insertEntry.patternCandidate & insertPattern
  val scorePat1 = PatternScore(pat1, insertPattern)
  val scorePat2 = PatternScore(pat2, insertPattern)
  val scorePat3 = PatternScore(pat3, insertPattern)
  val scorePat4 = PatternScore(pat4, insertPattern)
  
  val mergedPattern = Mux(scorePat1 > scorePat2, pat1, pat2)
  val mergedPatternCandidate = Mux(scorePat3 > scorePat4, pat3, pat4)
  val newPattern = Mux(hasInsertMatch, mergedPattern, insertPattern)
  val newPatternCandidate = Mux(hasInsertMatch, mergedPatternCandidate, insertPattern)
  
  // val mergedPattern = insertEntry.pattern | insertPattern
  // val newPattern = Mux(hasInsertMatch, mergedPattern, insertPattern)
  // internal wires end========================================================


  // replace start ========================================================
  def getUpdatedRrpv(set: UInt, pn: UInt, rrpv: UInt) = {
    val isUpdated = checkUpateEnable && 
                    hasCheckUpdateMatch &&
                    pn === checkUpdatePn &&
                    set === checkUpdateSet

    Mux(isUpdated, 0.U(2.W), rrpv)
  }

  def findVictimIdx(set: UInt): (Bool, UInt, UInt) = {
    val victimRrpv = table(set)
      .map(e => getUpdatedRrpv(e.pn, set, e.rrpv))
      .reduceLeft(_ max _)

    val victimIdx = table(set).indexWhere(e =>
      getUpdatedRrpv(e.pn, set, e.rrpv) === victimRrpv
    )

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

  val (victimValid, victimIdx, victimRrpv) = findVictimIdx(insertSet)

  // replace end ==========================================================

  // checkUpdate logic start ===============================================
  when(checkUpateEnable){
    when(insertEnable && insertSet === checkUpdateSet && insertPn === checkUpdatePn){
      // insert has higher priority
      io.checkUpdateIO.resp.hit := true.B
      io.checkUpdateIO.resp.pattern := newPattern
    } .elsewhen(hasCheckUpdateMatch) {
      io.checkUpdateIO.resp.hit := true.B
      io.checkUpdateIO.resp.pattern := checkUpdateMatchEntry.pattern
    }
  }
  // checkUpdate logic end ===================================================

  // insert logic start =====================================================

  when(insertEnable) {
    when(hasInsertMatch) {
      table(insertSet)(insertMatchIdx).pattern := newPattern
      table(insertSet)(insertMatchIdx).patternCandidate := newPatternCandidate
      updateMetadata(false.B, insertSet, insertMatchIdx)
    } otherwise {
      table(insertSet)(victimIdx).valid := true.B
      table(insertSet)(victimIdx).pn := insertPn
      table(insertSet)(victimIdx).pattern := newPattern
      table(insertSet)(victimIdx).patternCandidate := newPatternCandidate
      table(insertSet).foreach(e =>
        e.rrpv := getUpdatedRrpv(insertSet, e.pn, e.rrpv) + maxRrpv - victimRrpv
      )
      updateMetadata(false.B, insertSet, victimIdx)
    }
  }
  // insert logic end =======================================================

  // insert = insertHit + insertMiss
  XSPerfAccumulate("ptInsert", insertEnable)
  XSPerfAccumulate("ptInsertHit", insertEnable && hasInsertMatch)
  XSPerfAccumulate("ptReplace", insertEnable && !hasInsertMatch && victimValid)

  // checkUpdate = checkUpdateHit + checkUpdateMiss
  XSPerfAccumulate("ptCheckUpdate", checkUpateEnable)
  XSPerfAccumulate("ptCheckUpdateHit", checkUpateEnable && hasCheckUpdateMatch)
}
