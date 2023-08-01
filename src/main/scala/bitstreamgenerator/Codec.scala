package bitstreamgenerator

import scala.math.BigInt
import scala.util.control.Exception.By

object log2Ceil {
  def apply(in: BigInt): Int = {
    require(in > 0)
    (in - 1).bitLength
  }
  def apply(in: Int): Int = apply(BigInt(in))
}

class Codec(params: Parameters, c_dict: Map[String, ModPEControl]):
  private val PEMuxSelDict = Map(
    "V_GRID" -> 0,
    "V" -> 1,
    "H_GRID" -> 2,
    "H" -> 3,
    "D" -> 4,
    "REG" -> 5,
    "ZERO" -> 6,
    "IDENTITY" -> 7
  )
  private val PEMuxSelWidth = log2Ceil(PEMuxSelDict.size)

  private val BigALUSelDict = Map(
    "MUL" -> 0,
    "ADD" -> 1,
    "SUB" -> 2
  )

  private val BigALUSelWidth = log2Ceil(BigALUSelDict.size)

  private val SmallALUSelDict = Map(
    "ADD" -> 0,
    "SUB" -> 1,
    "MAX3" -> 2,
    "MIN3" -> 3,
    "EQ3" -> 4,
    "NEQ3" -> 5
  )

  private val SmallALUSelWidth = log2Ceil(SmallALUSelDict.size)

  private val MMuxSelDict = Map(
    "A" -> 0,
    "BIG_ALU" -> 1
  )

  private val MMuxSelWidth = log2Ceil(MMuxSelDict.size)

  private val OutGridMuxSelDict = Map(
    "FORWARD"-> 0,
    "FU" -> 1
  )

  private val OutGridMuxSelWidth = log2Ceil(OutGridMuxSelDict.size)

  private val OutMuxSelDict= Map(
    "FU"-> 0,
    "REG" -> 1
  )

  private val OutMuxSelWidth =  log2Ceil(OutMuxSelDict.size)

  private val FUControlWidth = BigALUSelWidth + SmallALUSelWidth + 2 + MMuxSelWidth + log2Ceil(params.interPETypeWidth)

  private val ModPEControlWidth = PEMuxSelWidth * 4 + FUControlWidth + OutGridMuxSelWidth * 2 + OutMuxSelWidth + 3

  private val cpgLineWidth = ModPEControlWidth + params.cpgCounterWidth + log2Ceil(params.controlPatternTableSize)

  private val seqLineWidth = params.sequencerCounterWidth + log2Ceil(params.controlPatternTableSize)

  private val cpgWordsPerLine = (cpgLineWidth + params.interPETypeWidth - 1) / params.interPETypeWidth

  private val seqWordsPerLine = (seqLineWidth + params.interPETypeWidth - 1) / params.interPETypeWidth

  private def long2NBools(l: Long, n: Int): Seq[Boolean] =
    def isBitSet(l: Long)(bit: Int): Boolean =
      ((l >> bit) & 1) == 1
    (0 to n - 1).map(isBitSet(l))

  private def bits2byteArray(bits: Seq[Boolean]) = bits
    .grouped(8)
    .map(_.foldRight(0)((b, i) => (i << 1) + (if (b) 1 else 0)).toByte)
    .toArray

  // encode a line to bits. the output is a byte sized. if the bits are not a multiple of 8 the MSBs of the leftmost byte are 0 padded
  // example: start_after_cycle_number[8 bits] = CCCCCCCC
  //          pattern_index_bits[2 bits] = PP
  //                        |byte 0| |byte 1|
  //          encoded line: 000000CC CCCCCCPP
  private def SequencingElementLineEncoder(
      line: SequencingElementLine
  ): Array[Byte] =
    val start_after_cycle_number_bits =
      long2NBools(line.start_after_cycle_number, params.sequencerCounterWidth)
    val pattern_index_bits =
      long2NBools(line.pattern_index, log2Ceil(params.controlPatternTableSize))
    val line_bits = pattern_index_bits ++ start_after_cycle_number_bits
    // val line_bytes = line_bits.grouped(8).map(_.foldRight(0)((b,i) => (i<<1) + (if(b) 1 else 0)).toByte).toArray
    val line_bytes = bits2byteArray(line_bits)
    line_bytes.reverse

  private def SequencingElementLineEncoderWordPadded(
      line: SequencingElementLine
  ): Array[Byte] =
    val start_after_cycle_number_bits =
      long2NBools(line.start_after_cycle_number, params.sequencerCounterWidth)
    val pattern_index_bits =
      long2NBools(line.pattern_index, log2Ceil(params.controlPatternTableSize))
    val line_bits = pattern_index_bits ++ start_after_cycle_number_bits
    // val line_bytes = line_bits.grouped(8).map(_.foldRight(0)((b,i) => (i<<1) + (if(b) 1 else 0)).toByte).toArray
    val padding_bits_number =
      params.verticalGridTypeWidth - (line_bits.size % params.verticalGridTypeWidth)
    val line_bytes = bits2byteArray(
      line_bits ++ Seq.fill(padding_bits_number)(false)
    )
    // val line_bytes = bits2byteArray(line_bits)
    line_bytes.reverse

  // encode the full Sequencer
  // order: the line in each SequencingElement, which form a table are coalesced.
  //        the resulting stream is a sequence of tables in row-major order
  // example: the first table is from SE[0][0], the second is from SE[0][1], then SE[1][0], and then SE[1][1]
  def encondeSequencer(config: Configuration): Array[Byte] =
    config.Mesh.flatten
      .map(_.SequencingElement)
      .flatten
      .map(SequencingElementLineEncoder)
      .flatten
      .toArray

  def encodeSequencerLoadOrder(config: Configuration): Array[Byte] =
    // normal order indexes: meshRow, meshColumn, tableLine, lineWord
    val normal = config.Mesh.map(
      _.map(
        _.SequencingElement
          .map(SequencingElementLineEncoderWordPadded)
          .map(_.grouped(params.verticalGridTypeWidth / 8).toSeq)
      )
    )
    // load order indexes: meshRow, tableLine, lineWord, meshColumn
    val loadOrder = normal
      .map(_.map(_.flatten))
      .map(_.transpose)
      .flatten
      .flatten
      .flatten
      .toArray
    loadOrder

  private def FUControlEncode(fu: FUControl): Seq[Boolean] =
    val shift_bits = long2NBools(fu.shift, log2Ceil(params.interPETypeWidth))
    val sel_m_bits = long2NBools(MMuxSelDict(fu.sel_m), MMuxSelWidth)
    val minus_m_bits = Seq(fu.minus_m)
    val minus_a_bits = Seq(fu.minus_a)
    val small_alu_sel_bits =
      long2NBools(SmallALUSelDict(fu.small_alu_sel), SmallALUSelWidth)
    val big_alu_sel_bits =
      long2NBools(BigALUSelDict(fu.big_alu_sel), BigALUSelWidth)
    val fu_bits =
      shift_bits ++
        sel_m_bits ++
        minus_m_bits ++
        minus_a_bits ++
        small_alu_sel_bits ++
        big_alu_sel_bits
    fu_bits

  private def ControlEncode(pe: ModPEControl): Seq[Boolean] =
    val double_buffer_sel_bits = Seq(pe.double_buffer_sel)
    val reg_q_en = Seq(pe.reg_q_en)
    val reg_p_en = Seq(pe.reg_p_en)
    val sel_out = long2NBools(OutMuxSelDict(pe.sel_out), OutMuxSelWidth)
    val sel_out_h_grid = long2NBools(OutGridMuxSelDict(pe.sel_out_h_grid), OutGridMuxSelWidth)
    val sel_out_v_grid = long2NBools(OutGridMuxSelDict(pe.sel_out_v_grid), OutGridMuxSelWidth)
    val fu_control_bits = FUControlEncode(pe.fu_control)
    val sel_q_bits = long2NBools(PEMuxSelDict(pe.sel_q), PEMuxSelWidth)
    val sel_c_bits = long2NBools(PEMuxSelDict(pe.sel_c), PEMuxSelWidth)
    val sel_b_bits = long2NBools(PEMuxSelDict(pe.sel_b), PEMuxSelWidth)
    val sel_a_bits = long2NBools(PEMuxSelDict(pe.sel_a), PEMuxSelWidth)
    val control_bits =
      double_buffer_sel_bits ++
        reg_q_en ++
        reg_p_en ++
        sel_out ++
        sel_out_h_grid ++
        sel_out_v_grid ++
        fu_control_bits ++
        sel_q_bits ++
        sel_c_bits ++
        sel_b_bits ++
        sel_a_bits
    control_bits

  private def cpgLineEncoder(line: ControlPatternLine): Array[Byte] =
    val next_index_bits =
      long2NBools(line.next_index, log2Ceil(params.controlPatternTableSize))
    val repeat_for_n_cycles_bits =
      long2NBools(line.repeat_for_n_cycles, params.cpgCounterWidth)
    val pe_control_bits = ControlEncode(c_dict(line.pe_control))
    val line_bits =
      next_index_bits ++ repeat_for_n_cycles_bits ++ pe_control_bits
    val padding_bits_number =
      params.verticalGridTypeWidth - (line_bits.size % params.verticalGridTypeWidth)
    val line_bytes = bits2byteArray(
      line_bits ++ Seq.fill(padding_bits_number)(false)
    )
    line_bytes.reverse

  def encodeCpg(config: Configuration): Array[Byte] =
    config.Mesh.flatten
      .map(_.ControlPatternGenerator)
      .flatten
      .map(cpgLineEncoder)
      .flatten
      .toArray
  def encodeCpgLoadOrder(config: Configuration): Array[Byte] =
    // normal order indexes: meshRow, meshColumn, tableLine, lineWord
    val normal = config.Mesh.map(
      _.map(
        _.ControlPatternGenerator
          .map(cpgLineEncoder)
          .map(_.grouped(params.verticalGridTypeWidth / 8).toSeq)
      )
    )
    // load order indexes: tableLine, lineWord, reverse meshRow , meshColumn
    val loadOrder = normal.reverse
      .map(_.map(_.flatten))
      .flatten
      .transpose
      .flatten
      .flatten
      .toArray
    loadOrder
  
  def encodeMesh(config: Configuration): Array[Byte] = encodeSequencerLoadOrder(config) ++ encodeCpgLoadOrder(config) 
  def encode(config: Configuration): Array[Byte] = encodeMesh(config)

  // def decodeSeq(bitstream: Array[Byte]): 


  // def decodeMesh(bitstream: Array[Byte]): Seq[Seq[PE]] =


  // def decode(bitstream: Array[Byte]): Configuration =
  //   val word_bytes = params.interPETypeWidth / 8
  //   val seq_size_bytes = params.meshRows * params.sequenceTableSize * seqWordsPerLine * word_bytes * params.meshColumns
  //   val cpg_size_bytes = params.controlPatternTableSize * cpgWordsPerLine * params.meshRows * params.meshColumns
  //   val mesh_size_bytes =  seq_size_bytes + cpg_size_bytes
  //   val (mesh_bit, remaining_bit) = bitstream.splitAt(mesh_size_bytes)

  //   Configuration(decodeMesh(mesh_bit))

end Codec
