package coupledL2.prefetch.hhp

import utility.{ChiselDB, MemReqSource, XSPerfAccumulate}
import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import coupledL2.prefetch._

class InsertFTIO(implicit p: Parameters) extends HHPBundle {
  val pn = UInt(log2PageNum.W)
  val offset = UInt(log2BlockNum.W)
}

class FTtoATIO(implicit p: Parameters) extends HHPBundle {
  val newPattern = UInt(patternWidth.W)
}

class InputCheckUpdateATIO(implicit p: Parameters) extends HHPBundle {
  val pn = UInt(log2PageNum.W)
  val set = UInt(log2SetNum.W)
  val pattern = UInt(patternWidth.W)
}

class OutputCheckUpdateATIO(implicit p: Parameters) extends HHPBundle {
  val hitBeforeInsertFT = Bool()
  // val hitAfterInsertFT = Bool()
  // val pattern = UInt(patternWidth.W)
}

class CheckUpdateATIO(implicit p: Parameters) extends HHPBundle {
  val req = Flipped(ValidIO(new InputCheckUpdateATIO))
  val resp = Output(new OutputCheckUpdateATIO)
}

class ATtoPT(implicit p: Parameters) extends HHPBundle {
  val set = UInt(log2SetNum.W)
  val pn = UInt(log2PageNum.W)
  val newPattern = UInt(patternWidth.W)
}

class AccumulationTableIO(implicit p: Parameters) extends HHPBundle {
  val globalTimer = Input(UInt(timeStampWidth.W))
  val checkUpdateIO = new CheckUpdateATIO
  val fromFTIO = Flipped(ValidIO(new FTtoATIO))
  val toPTIO = ValidIO(new ATtoPT)
}

class InputCheckUpdatePT(implicit p: Parameters) extends HHPBundle {
  val set = UInt(log2SetNum.W)
  val pn = UInt(log2PageNum.W)
}

class OutputCheckUpdatePTIO(implicit p: Parameters) extends HHPBundle {
  val hit = Bool()
  val pattern = UInt(patternWidth.W)
}

class CheckUpdatePTIO(implicit p: Parameters) extends HHPBundle {
  val req = Flipped(ValidIO(new InputCheckUpdatePT))
  val resp = Output(new OutputCheckUpdatePTIO)
}

class PatternTableIO(implicit p: Parameters) extends HHPBundle {
  val fromATIO = Flipped(ValidIO(new ATtoPT))
  val checkUpdateIO = new CheckUpdatePTIO
}

class InputCheckUpdateRPIO(implicit p: Parameters) extends HHPBundle {
  val cacheHit = Bool()
  val pn = UInt(log2PageNum.W)
  val pattern = UInt(patternWidth.W)
}

class OutputCheckUpdateRPIO(implicit p: Parameters) extends HHPBundle {
  val hasSmilar = Bool()
  val pattern = UInt(patternWidth.W)
  // val mask = UInt(64.W)
}

class RecentPageTableIO(implicit p: Parameters) extends HHPBundle {
  val req = Flipped(ValidIO(new InputCheckUpdateRPIO))
  val resp = Output(new OutputCheckUpdateRPIO)
}

class InputPrefIO(implicit p: Parameters) extends HHPBundle {
  val addr = UInt(log2AddrNum.W)
  val cacheHit = Bool()
  val needT = Bool()
  val source = UInt(sourceIdBits.W)
}

class OutputPrefIO(implicit p: Parameters) extends HHPBundle {
  // val triggerAddr = UInt(log2AddrNum.W)
  val baseAddr = UInt(log2AddrNum.W)
  // val mask = UInt(64.W)
  val pattern = UInt(patternWidth.W)
  // val src = UInt(20.W)
  val source = UInt(sourceIdBits.W)
  val pfSource = UInt(sourceIdBits.W)
  val needT = Bool()
}

class PnSMSNeiIO(implicit p: Parameters) extends HHPBundle {
  val train = Flipped(ValidIO(new InputPrefIO))
  val sendPref = ValidIO(new OutputPrefIO)
}

class PnSMSNei(implicit p: Parameters) extends HHPModule {

  val io = IO(new PnSMSNeiIO)
  val filterTable = Module(new FilterTable)
  val accumulationTable = Module(new AccumulationTable)
  val patternTable = Module(new PatternTable)
  val recentPageTable = Module(new RecentPageTable)
  val accessCount = RegInit(0.U(64.W))

  // default output======================================
  io.sendPref.valid := false.B
  io.sendPref.bits.baseAddr := getBaseAddr(io.train.bits.addr)
  io.sendPref.bits.pattern  := 0.U(patternWidth.W)
  io.sendPref.bits.pfSource := MemReqSource.NoWhere.id.U
  io.sendPref.bits.source   := io.train.bits.source
  io.sendPref.bits.needT    := io.train.bits.needT
  // default output======================================


  // internal wire ==========================================
  accumulationTable.io.fromFTIO := filterTable.io.toATIO
  patternTable.io.fromATIO      := accumulationTable.io.toPTIO

  val atHitBefFt = accumulationTable.io.checkUpdateIO.resp.hitBeforeInsertFT
  // val atHitAftFt = accumulationTable.io.checkUpdateIO.resp.hitAfterInsertFT
  // val atPattern  = accumulationTable.io.checkUpdateIO.resp.pattern

  val ptHit     = patternTable.io.checkUpdateIO.resp.hit
  val ptPattern = patternTable.io.checkUpdateIO.resp.pattern

  val rptHasSimilar = recentPageTable.io.resp.hasSmilar
  val rptPattern    = recentPageTable.io.resp.pattern

  val enable    = io.train.valid
  val inputAddr = io.train.bits.addr
  val cacheHit  = io.train.bits.cacheHit
  val (pn, set, offset)    = parseAddr(inputAddr)
  val inputOffsetToPattern = offsetToPattern(offset)
  val slectSLP           = ptHit && ((inputOffsetToPattern & ptPattern) =/= 0.U)

  val globalTimer = timeStamp(accessCount)
  filterTable.io.globalTimer := globalTimer
  accumulationTable.io.globalTimer := globalTimer
  // internal wire ==========================================

  // default internal wire==========================================
  filterTable.io.insertEntryIO.valid := false.B
  filterTable.io.insertEntryIO.bits.pn := pn
  filterTable.io.insertEntryIO.bits.offset := offset

  accumulationTable.io.checkUpdateIO.req.valid := false.B
  accumulationTable.io.checkUpdateIO.req.bits.pn := pn
  accumulationTable.io.checkUpdateIO.req.bits.set := set
  accumulationTable.io.checkUpdateIO.req.bits.pattern := inputOffsetToPattern

  patternTable.io.checkUpdateIO.req.valid := false.B
  patternTable.io.checkUpdateIO.req.bits.set := set
  patternTable.io.checkUpdateIO.req.bits.pn := pn

  recentPageTable.io.req.valid := false.B
  recentPageTable.io.req.bits.cacheHit := cacheHit
  recentPageTable.io.req.bits.pn := pn
  recentPageTable.io.req.bits.pattern := inputOffsetToPattern

  // default internal wire==========================================


  when(enable) {
    accessCount := accessCount + 1.U

    // checkAndUpdateAT
    accumulationTable.io.checkUpdateIO.req.valid := true.B

    // checkAndUpdatePT
    patternTable.io.checkUpdateIO.req.valid := true.B

    // InsertToFilterTable
    filterTable.io.insertEntryIO.valid := !atHitBefFt

    // updateRPTable
    recentPageTable.io.req.valid := true.B

    when(!cacheHit) {
      when(slectSLP) {
        io.sendPref.valid := true.B
        io.sendPref.bits.pattern := ptPattern
        io.sendPref.bits.pfSource := MemReqSource.Prefetch2L2SLP.id.U
      } otherwise {
        io.sendPref.valid         := rptHasSimilar
        io.sendPref.bits.pattern  := rptPattern
        io.sendPref.bits.pfSource := MemReqSource.Prefetch2L2TLP.id.U
      }
    }
  }

  XSPerfAccumulate("hhp_Ptn_needTotal",    enable && !cacheHit);
  XSPerfAccumulate("hhp_Ptn_selectTotal",  io.sendPref.valid);
  XSPerfAccumulate("hhp_Ptn_selectSLP",    io.sendPref.valid && slectSLP);
  XSPerfAccumulate("hhp_Ptn_selectTLP",    io.sendPref.valid && !slectSLP);

  XSPerfAccumulate("hhp_Pft_selectSLP",    Mux(io.sendPref.valid && slectSLP, PopCount(io.sendPref.bits.pattern), 0.U));
  XSPerfAccumulate("hhp_Pft_selectTLP",    Mux(io.sendPref.valid && !slectSLP, PopCount(io.sendPref.bits.pattern), 0.U));

  class HhpDbEntry extends Bundle{
    val counter = UInt(64.W)
    val pageSet = UInt(log2SetNum.W)
    val pageNum  = UInt(log2PageNum.W)
    val blockNum = UInt(log2BlockNum.W)
    val sentPref = Bool()
    val pattern = UInt(patternWidth.W)
  }
  val L2HhpTrainTable = ChiselDB.createTable("L2HhpTrainTable", new HhpDbEntry, basicDB = true)
  val hhpData = Wire(new HhpDbEntry)
  hhpData.counter := accessCount
  hhpData.pageSet := set
  hhpData.pageNum := pn
  hhpData.blockNum := offset
  hhpData.sentPref := io.sendPref.valid
  hhpData.pattern := Mux(io.sendPref.valid, io.sendPref.bits.pattern, 0.U)
  L2HhpTrainTable.log(data = hhpData, en = enable, site = name, clock, reset)

  val L2SlpTrainTable = ChiselDB.createTable("L2SlpTrainTable", new HhpDbEntry, basicDB = true)
  val slpData = Wire(new HhpDbEntry)
  slpData.counter := accessCount
  slpData.pageSet := set
  slpData.pageNum := pn
  slpData.blockNum := offset
  slpData.sentPref := io.sendPref.valid && slectSLP
  slpData.pattern := Mux(io.sendPref.valid && slectSLP, io.sendPref.bits.pattern, 0.U)
  L2SlpTrainTable.log(data = slpData, en = enable && !cacheHit, site = name, clock, reset)

  val L2TlpTrainTable = ChiselDB.createTable("L2TlpTrainTable", new HhpDbEntry, basicDB = true)
  val tlpData = Wire(new HhpDbEntry)
  tlpData.counter := accessCount
  tlpData.pageSet := set
  tlpData.pageNum := pn
  tlpData.blockNum := offset
  tlpData.sentPref := io.sendPref.valid && !slectSLP
  tlpData.pattern := Mux(io.sendPref.valid && !slectSLP, io.sendPref.bits.pattern, 0.U)
  L2TlpTrainTable.log(data = tlpData, en = enable && !cacheHit, site = name, clock, reset)

}
