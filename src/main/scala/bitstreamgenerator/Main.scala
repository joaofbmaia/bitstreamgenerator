package bitstreamgenerator

import org.virtuslab.yaml.*
import java.io._
import scala.io.Source

@main def main(args: String*): Unit =
  val usage = "Usage: bitstreamgenerator [-t FULL|MESH|SEQ|CPG] [-p parameters_file] [-d control_dictionary_file] [-i input_file] [-o output_file]"

  if (args.isEmpty || args.length % 2 != 0) {
    println(usage)
    sys.exit(1)
  }

  val argMap = Map.newBuilder[String, Any]
  args.sliding(2, 2).toSeq.collect {
    case Seq("-t", out_type: String) => argMap.+=("out_type" -> out_type)
    case Seq("-p", parameters_file: String) => argMap.+=("parameters_file" -> parameters_file)
    case Seq("-d", control_dictionary_file: String) => argMap.+=("control_dictionary_file" -> control_dictionary_file)
    case Seq("-i", input_file: String) => argMap.+=("input_file" -> input_file)
    case Seq("-o", output_file: String) => argMap.+=("output_file" -> output_file)
  }
  val options = argMap.result()

  val out_type = options.get("out_type").getOrElse("FULL").toString()
  val parameters_file = options.get("parameters_file").getOrElse("parameters.yaml").toString()
  val control_dictionary_file = options.get("control_dictionary_file").getOrElse("control_dictionary.yaml").toString()
  val input_file = options.get("input_file").getOrElse("config.yaml").toString()
  val output_file = options.get("output_file").getOrElse(input_file.replaceFirst("[.][^.]+$", "") + "_bit_" + out_type.toLowerCase() + ".bin").toString()

  def yamlValidate[T](yaml_out: Either[YamlError, T]): T =
    yaml_out match
      case Right(x) => x
      case Left(e) => println("Error: " + e.msg)
                      sys.exit(1)

  val c_dict = yamlValidate(Source.fromFile(control_dictionary_file).mkString.as[Map[String, ModPEControl]])
  val params = yamlValidate(Source.fromFile(parameters_file).mkString.as[Parameters])
  val config = yamlValidate(Source.fromFile(input_file).mkString.as[Configuration])
  
  assert(config.Mesh.size == params.meshRows)
  assert(config.Mesh(0).size == params.meshColumns)
  assert(config.Mesh(0)(0).SequencingElement.size == params.sequenceTableSize)
  assert(config.Mesh(0)(0).ControlPatternGenerator.size == params.controlPatternTableSize)

  val codec = Codec(params, c_dict)

  val bitstream = out_type match
    case "FULL" => codec.encode(config)
    case "MESH" => codec.encodeMesh(config)
    case "SEQ" => codec.encodeSequencerLoadOrder(config)
    case "CPG" => codec.encodeCpgLoadOrder(config)
    case _ => println("Error: invalid output type: " + out_type)
              println(usage)
              sys.exit(1)

  val out = new FileOutputStream(output_file)
  out.write(bitstream)
  out.close()

  // val codedSeq = codec.encondeSequencer()
  // val codedCpg = codec.encodeCpg()
  // val codedCpg_load = codec.encodeCpgLoadOrder()
  // val codedSeq_load = codec.encodeSequencerLoadOrder()
  
  // val out = new FileOutputStream("OS_GEMM_bit_cpg.bin")
  // out.write(codedCpg)
  // out.close()

  // val out_load = new FileOutputStream("OS_GEMM_bit_cpg_load.bin")
  // out_load.write(codedCpg_load)
  // out_load.close()

  // val outSeq = new FileOutputStream("OS_GEMM_bit_seq.bin")
  // outSeq.write(codedSeq)
  // outSeq.close()

  // val outSeq_load = new FileOutputStream("OS_GEMM_bit_seq_load.bin")
  // outSeq_load.write(codedSeq_load)
  // outSeq_load.close()

  // val outMesh = new FileOutputStream("OS_GEMM_bit_mesh.bin")
  // outMesh.write(codec.encodeSequencerLoadOrder())
  // outMesh.write(codec.encodeCpgLoadOrder())
  // outMesh.close()


  // val cd_test = ModPEControl("H_BCAST", "V_BCAST", "REG", "V", FUControl("MUL", "ADD", false, false, "BIG_ALU", 0), true, false)
  // val cd_test2 = ModPEControl("H_BCAST", "V_BCAST", "REG", "V", FUControl("MUL", "ADD", false, false, "BIG_ALU", 0), true, true)
  // val cd_test_map = Map("GEMM_OS_PROP" -> cd_test, "GEMM_WS_PROP" -> cd_test2)

  // val cpgLine = ControlPatternLine("GEMM_OS_PROP", 16, 1)
  // val cpgLine2 = ControlPatternLine("GEMM_OS_COMP", 16, 0)
  // val cpgTable = Seq(cpgLine, cpgLine2)

  // val seLine = SequencingElementLine(0, 0)
  // val seLine2 = SequencingElementLine(0, 0)
  // val seTable = Seq(seLine, seLine2)
  // val pe = PE(seTable, cpgTable)
  // val mesh = Seq(Seq(pe, pe), Seq(pe, pe))
  // val cfg = Configuration(mesh)

  // val outputFile = new File("output.yaml")
  // val writer = new FileWriter(outputFile)
  // val parameters = new Parameters(32, 32, 4, 4, 8, 32)
  // val parametersArray = Seq(parameters, parameters, parameters)
  // val parametersMap = Map("PROP" -> parameters, "COMP" -> parameters, "GARBAGE" -> parameters)
  // val parametersYaml = parametersMap.asYaml
  // writer.write(parametersYaml)
  // writer.close()
  
  // val parameters2Yaml = Source.fromFile("output.yaml").mkString
  // val parameters2Either = parameters2Yaml.as[Map[String, Parameters]]
  
  // parameters2Either match {
  //   case Left(e) => throw new Exception("invalid input: " + e.msg)
  //   case Right(r) => 
  // }

  // val parameters2 = parameters2Either.right.get

  // val outputFile2 = new File("output2.yaml")
  // val writer2 = new FileWriter(outputFile2)
  // val parameters3Yaml = parameters2.asYaml
  // writer2.write(parameters3Yaml)
  // writer2.close()

