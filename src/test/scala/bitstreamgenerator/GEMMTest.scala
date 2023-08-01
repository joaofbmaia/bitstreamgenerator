package bitstreamgenerator

import org.virtuslab.yaml.*
import java.io._
import scala.io.Source

class GEMMTest extends munit.FunSuite {
  test("generates tiled OS") {
    def generateOSTest(mesh_dim: Int, m: Int, k: Int, n: Int) = 
      val dimXdim = s"${mesh_dim}x${mesh_dim}"
      val base_dir = "examples/" + dimXdim + "/"
      val mXkXn = s"${m}x${k}x${n}"

      val params = Source.fromFile(base_dir + "parameters.yaml").mkString.as[Parameters].right.get
      val c_dict = Source.fromFile("examples/control_dictionary.yaml").mkString.as[Map[String, ModPEControl]].right.get

      val gemm = new GEMM(params)

      val config = gemm.OS_config(m, k, n)
      val config_yaml = config.asYaml

      val outYaml = new FileWriter(base_dir + "/OS_GEMM_" + mXkXn + "_tiled_" + dimXdim + ".yaml")
      outYaml.write(config_yaml)
      outYaml.close()

      val codec = Codec(params, c_dict)
      val bitstream = codec.encode(config)

      val outBitstream = new FileOutputStream(base_dir + "/OS_GEMM_" + mXkXn + "_tiled_" + dimXdim + "_bit_full.bin")
      outBitstream.write(bitstream)
      outBitstream.close()

      val streams = gemm.OS_test_streams(m, k, n)

      val outA = new FileOutputStream(base_dir + "/OS_GEMM_" + mXkXn + "_tiled_" + dimXdim + "_a_stream.bin")
      outA.write(streams._1)
      outA.close()

      val outB = new FileOutputStream(base_dir + "/OS_GEMM_" + mXkXn + "_tiled_" + dimXdim + "_b_stream.bin")
      outB.write(streams._2)
      outB.close()

      val outD = new FileOutputStream(base_dir + "/OS_GEMM_" + mXkXn + "_tiled_" + dimXdim + "_d_stream.bin")
      outD.write(streams._3)
      outD.close()

      val outC = new FileOutputStream(base_dir + "/OS_GEMM_" + mXkXn + "_tiled_" + dimXdim + "_c_stream.bin")
      outC.write(streams._4)
      outC.close()
    
    generateOSTest(mesh_dim = 2, m = 2, k = 2, n = 2)
    generateOSTest(mesh_dim = 2, m = 4, k = 2, n = 2)
    generateOSTest(mesh_dim = 2, m = 2, k = 4, n = 2)
    generateOSTest(mesh_dim = 2, m = 2, k = 4, n = 4)
    generateOSTest(mesh_dim = 2, m = 4, k = 4, n = 2)
    generateOSTest(mesh_dim = 2, m = 4, k = 2, n = 4)
    generateOSTest(mesh_dim = 2, m = 2, k = 4, n = 4)
    generateOSTest(mesh_dim = 2, m = 4, k = 4, n = 4)
    generateOSTest(mesh_dim = 2, m = 8, k = 8, n = 8)

    generateOSTest(mesh_dim = 3, m = 3, k = 3, n = 3)
    generateOSTest(mesh_dim = 3, m = 6, k = 3, n = 3)
    generateOSTest(mesh_dim = 3, m = 3, k = 6, n = 3)
    generateOSTest(mesh_dim = 3, m = 3, k = 6, n = 6)
    generateOSTest(mesh_dim = 3, m = 6, k = 6, n = 3)
    generateOSTest(mesh_dim = 3, m = 6, k = 3, n = 6)
    generateOSTest(mesh_dim = 3, m = 3, k = 6, n = 6)
    generateOSTest(mesh_dim = 3, m = 6, k = 6, n = 6)
    generateOSTest(mesh_dim = 3, m = 9, k = 9, n = 9)

    generateOSTest(mesh_dim = 4, m = 4, k = 4, n = 4)
    generateOSTest(mesh_dim = 4, m = 8, k = 4, n = 4)
    generateOSTest(mesh_dim = 4, m = 4, k = 8, n = 4)
    generateOSTest(mesh_dim = 4, m = 4, k = 8, n = 8)
    generateOSTest(mesh_dim = 4, m = 8, k = 8, n = 4)
    generateOSTest(mesh_dim = 4, m = 8, k = 4, n = 8)
    generateOSTest(mesh_dim = 4, m = 4, k = 8, n = 8)
    generateOSTest(mesh_dim = 4, m = 8, k = 8, n = 8)
    generateOSTest(mesh_dim = 4, m = 16, k = 16, n = 16)

    generateOSTest(mesh_dim = 8, m = 8, k = 8, n = 8)
    generateOSTest(mesh_dim = 8, m = 24, k = 24, n = 24)

    generateOSTest(mesh_dim = 16, m = 16, k = 16, n = 16)
    generateOSTest(mesh_dim = 16, m = 32, k = 32, n = 32)
    }
}
