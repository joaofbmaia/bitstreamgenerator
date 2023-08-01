package bitstreamgenerator

import org.virtuslab.yaml.*

// parameters
case class Parameters(
    val meshRows: Int,
    val meshColumns: Int,
    val interPETypeWidth: Int,
    val horizontalGridTypeWidth: Int,
    val verticalGridTypeWidth: Int,
    val sequenceTableSize: Int,
    val controlPatternTableSize: Int,
    val cpgCounterWidth: Int,
    val sequencerCounterWidth: Int
) derives YamlCodec

// control dictionary
case class FUControl(
    val big_alu_sel: String,
    val small_alu_sel: String,
    val minus_a: Boolean,
    val minus_m: Boolean,
    val sel_m: String,
    val shift: Int
) derives YamlCodec

case class ModPEControl(
    val sel_a: String,
    val sel_b: String,
    val sel_c: String,
    val sel_q: String,
    val fu_control: FUControl,
    val sel_out_v_grid: String,
    val sel_out_h_grid: String,
    val sel_out: String,
    val reg_p_en: Boolean,
    val reg_q_en: Boolean,
    val double_buffer_sel: Boolean
) derives YamlCodec

// configuration
case class SequencingElementLine(
    val start_after_cycle_number: Long,
    val pattern_index: Int
) derives YamlCodec

case class ControlPatternLine(
    val pe_control: String,
    val repeat_for_n_cycles: Int,
    val next_index: Int
) derives YamlCodec

case class PE(
    val SequencingElement: Seq[SequencingElementLine],
    val ControlPatternGenerator: Seq[ControlPatternLine]
) derives YamlCodec

case class Configuration(
    val Mesh: Seq[Seq[PE]]
) derives YamlCodec
