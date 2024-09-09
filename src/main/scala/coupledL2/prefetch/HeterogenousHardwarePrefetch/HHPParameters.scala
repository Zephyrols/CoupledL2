package coupledL2.prefetch

import utility.{MemReqSource, XSPerfAccumulate}
import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import coupledL2.HasCoupledL2Parameters

case class HHPParameters(
    ftTableWayNum:   Int = 128,
    acTableWayNum:   Int =  16, // DEFAULT 16
    ptTableWayNum:   Int =  16,
    rpTableWayNum:   Int = 256,
    fteOffsetNum:    Int =   3, // DEFAULT 3
    actRetireWin:    Int =   8, // DEFAULT 8
    pageDistance:    Int =  64,
    timeStampWidth:  Int =  12,
    setBits:         Int =   8, // DEFAULT 8
    similarThreshold:Int =   8,
    patternBufferQueueSize: Int =  128 // DEFAULT 16
) extends PrefetchParameters {
  override val hasPrefetchBit: Boolean = true
  override val hasPrefetchSrc: Boolean = true
  override val inflightEntries: Int = 16
}

trait HasHHPParams extends HasCoupledL2Parameters {
  val hhpParams = prefetchers.find {
      case p: HHPParameters => true
      case _ => false
    }.get.asInstanceOf[HHPParameters]

  // Heterogenous Hardware Prefetcher
  def log2SetNum    = hhpParams.setBits
  def ftTableWayNum = hhpParams.ftTableWayNum
  def acTableWayNum = hhpParams.acTableWayNum
  def ptTableWayNum = hhpParams.ptTableWayNum
  def rpTableWayNum = hhpParams.rpTableWayNum
  def fteOffsetNum = hhpParams.fteOffsetNum
  def actRetireWin = hhpParams.actRetireWin
  def pageDistance = hhpParams.pageDistance
  def timeStampWidth = hhpParams.timeStampWidth
  def timeStampMask = (1 << timeStampWidth) - 1
  def similarThreshold = hhpParams.similarThreshold
  def patternBufferQueueSize = hhpParams.patternBufferQueueSize

  def log2AddrNum = fullAddressBits
  def log2PageSize = pageOffsetBits
  def log2PageNum = fullAddressBits - pageOffsetBits
  def log2BlockSize = offsetBits
  def log2BlockNum = pageOffsetBits - offsetBits
  def pageRegionMask = (1 << pageOffsetBits) - 1
  def patternWidth = 1 << (pageOffsetBits - offsetBits)

  assert(log2SetNum <= log2PageNum, "hhp set number should be less than page number")

  def offsetToPattern(offset: UInt): UInt = {
    UIntToOH(offset, patternWidth)
  }

  def parseAddr(addr: UInt): (UInt, UInt, UInt) = {
    assert(addr.getWidth == log2AddrNum)
    
    val pn = addr(log2AddrNum - 1, log2PageSize)
    val set = addr(log2PageSize + log2SetNum - 1, log2PageSize)
    val offset = addr(log2PageSize - 1, log2BlockSize)

    (pn, set, offset)
  }

  def getBaseAddr(addr: UInt): UInt = {
    assert(addr.getWidth == log2AddrNum)
    parseAddr(addr)._1 << log2PageSize
  }

  def timeStamp(timer: UInt): UInt = {
    (timer >> 5.U) & timeStampMask.U
  }
}

abstract class HHPBundle(implicit val p: Parameters) extends Bundle with HasHHPParams
abstract class HHPModule(implicit val p: Parameters) extends Module with HasHHPParams