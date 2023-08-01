package bitstreamgenerator

import scala.math.BigInt
import breeze.linalg._
import util.Random.nextInt

// GEMM: C = A * B + D
// A is M-by-K matrix
// B is K-by N matrix
// C is M-by-N matrix
// D is M-by-N matrix
// val M: Int
// val N: Int
// val K: Int

class GEMM(params: Parameters/*, c_dict: Map[String, ModPEControl]*/):
  private def toByteArray(n_bytes: Int)(i: Int) = BigInt(i).toByteArray.reverse.padTo(n_bytes, if(i >= 0) then 0x00.toByte else 0xFF.toByte).reverse

  private def matrices2stream(matrices: Seq[Seq[Seq[Int]]], pad_to: Int, initial_offset: Int) = 
    def pad/*(pad_to: Int, initial_offset: Int)*/(columns: Seq[Int], offset: Int) =
      val head_padding = initial_offset + offset
      val tail_padding = pad_to - columns.length - head_padding
      assert(tail_padding >= 0)
      Seq.fill(head_padding)(0) ++ columns ++ Seq.fill(tail_padding)(0)

    val columns = matrices.flatten.transpose
    val padded_columns = columns.zipWithIndex.map(pad/*(stream_size, initial_offset)*/)
    padded_columns.transpose

  private def stream2byteStream(stream: Seq[Seq[Int]], element_size: Int): Array[Byte] = 
    stream.flatten.map(toByteArray(element_size)).flatten.toArray

  // def OS_mesh_sized(): Configuration =
  //   val mesh_vertical_propagation_time = params.meshRows

  //   val cpg_line_0 = ControlPatternLine("IDLE", 0, 0)
  //   val cpg_line_1 = ControlPatternLine("GEMM_OS_PROP", mesh_vertical_propagation_time - 1, 2)
  //   val cpg_line_2 = ControlPatternLine("GEMM_OS_COMP", mesh_vertical_propagation_time - 1, 1)
  //   val cpg_filler_line = ControlPatternLine("IDLE", 0, 0)

  //   assert(params.controlPatternTableSize >= 3)
  //   val cpg = Seq(cpg_line_0, cpg_line_1, cpg_line_2).padTo(params.controlPatternTableSize, cpg_filler_line)

  //   val seq_line_0 = SequencingElementLine(0, 0)
  //   val seq_line_1 = (r: Int, c: Int) => SequencingElementLine(r + c + 1, 1)
  //   val seq_line_2 = (r: Int, c: Int) => SequencingElementLine(r + c + 1 + 3 * mesh_vertical_propagation_time, 0)
  //   val seq_filler_line = SequencingElementLine(0, 0)

  //   assert(params.sequenceTableSize >= 3)
  //   val seq = (r: Int, c: Int) => Seq(seq_line_0, seq_line_1(r, c), seq_line_2(r, c)).padTo(params.sequenceTableSize, seq_filler_line)

  //   val pe = (r: Int, c: Int) => PE(seq(r, c), cpg)

  //   val mesh = Seq.tabulate(params.meshRows, params.meshColumns)(pe)

  //   val configuration = Configuration(mesh)
  //   configuration

  // def OS_mesh_sized_test_streams(): (Array[Byte], Array[Byte], Array[Byte], Array[Byte]) =
  //   val matrix_size = params.meshRows * params.meshColumns

  //   // generate test matrices
  //   val A = Seq.range(1, matrix_size + 1).grouped(params.meshColumns).toSeq
  //   val B = Seq.range(matrix_size + 1, 2 * matrix_size + 1).grouped(params.meshColumns).toSeq
  //   val D = Seq.range(2 * matrix_size + 1, 3 * matrix_size + 1).grouped(params.meshColumns).toSeq
  //   val A_mat = new DenseMatrix(A.length, A.last.length, A.transpose.flatten.toArray)
  //   val B_mat = new DenseMatrix(B.length, B.last.length, B.transpose.flatten.toArray)
  //   val D_mat = new DenseMatrix(D.length, D.last.length, D.transpose.flatten.toArray)
  //   val C_mat = A_mat * B_mat + D_mat
  //   val C = C_mat.data.toSeq.grouped(params.meshRows).toSeq.transpose    

  //   val num_matrices = 1
  //   val stream_size = (3 + num_matrices) * params.meshRows

  //   val A_stream = matrices2stream(Seq(A.transpose), stream_size, params.meshRows)
  //   val B_stream = matrices2stream(Seq(B), stream_size, params.meshRows)
  //   val D_stream = matrices2stream(Seq(D.reverse), stream_size, 0)
  //   val C_stream = matrices2stream(Seq(C.reverse), stream_size, 2 * params.meshRows + 1)

  //   val A_bytes = stream2byteStream(A_stream, params.horizontalGridTypeWidth / 8)
  //   val B_bytes = stream2byteStream(B_stream, params.verticalGridTypeWidth / 8)
  //   val D_bytes = stream2byteStream(D_stream, params.interPETypeWidth / 8)
  //   val C_bytes = stream2byteStream(C_stream, params.interPETypeWidth / 8)

  //   (A_bytes, B_bytes, D_bytes, C_bytes)

    
  def OS_config(m: Int, k: Int, n: Int): Configuration =
    assert(params.meshRows == params.meshColumns)
    assert(m % params.meshRows == 0)
    assert(k % params.meshRows == 0)
    assert(n % params.meshColumns == 0)

    val mesh_dim = params.meshRows

    val tiles_m = m / mesh_dim
    val tiles_k = k / mesh_dim
    val tiles_n = n / mesh_dim

    val tile_operations = tiles_m * tiles_n * tiles_k

    val mesh_vertical_propagation_time = params.meshRows

    val cpg_line_0 = ControlPatternLine("IDLE", 0, 0)
    val cpg_line_1 = ControlPatternLine("GEMM_OS_PROP", tiles_k * mesh_vertical_propagation_time - 1, 2)
    val cpg_line_2 = ControlPatternLine("GEMM_OS_COMP", tiles_k * mesh_vertical_propagation_time - 1, 1)
    val cpg_filler_line = ControlPatternLine("IDLE", 0, 0)

    assert(params.controlPatternTableSize >= 3)
    val cpg = Seq(cpg_line_0, cpg_line_1, cpg_line_2).padTo(params.controlPatternTableSize, cpg_filler_line)

    val seq_line_0 = SequencingElementLine(0, 0) // idle
    val seq_line_1 = (r: Int, c: Int) => SequencingElementLine(r + c + 1, 1) // initial preload
    val seq_line_2 = (r: Int, c: Int) => SequencingElementLine(r + c + 1 + mesh_vertical_propagation_time, 2) // computation
    val seq_line_3 = (r: Int, c: Int) => SequencingElementLine(r + c + 1 + (2 + tile_operations) * mesh_vertical_propagation_time, 0) // idle
    val seq_filler_line = SequencingElementLine(0, 0)

    assert(params.sequenceTableSize >= 4)
    val seq = (r: Int, c: Int) => Seq(seq_line_0, seq_line_1(r, c), seq_line_2(r, c), seq_line_3(r, c)).padTo(params.sequenceTableSize, seq_filler_line)

    val pe = (r: Int, c: Int) => PE(seq(r, c), cpg)

    val mesh = Seq.tabulate(params.meshRows, params.meshColumns)(pe)

    val configuration = Configuration(mesh)
    configuration


  def OS_test_streams(m: Int, k: Int, n: Int): (Array[Byte], Array[Byte], Array[Byte], Array[Byte]) =
    assert(params.meshRows == params.meshColumns)
    assert(m % params.meshRows == 0)
    assert(k % params.meshRows == 0)
    assert(n % params.meshColumns == 0)

    val mesh_dim = params.meshRows

    val tiles_m = m / mesh_dim
    val tiles_k = k / mesh_dim
    val tiles_n = n / mesh_dim

    // generate test matrices
    val A = Seq.range(1, 1 + m * k).map(_.%(128)).grouped(k).toSeq
    val B = Seq.range(1 + m * k, 1 + m * k + k * n).grouped(n).toSeq
    val D = Seq.range(1 + m * k + k * n, 1 + m * k + k * n + m * n).grouped(n).toSeq
    val A_mat = new DenseMatrix(A.length, A.last.length, A.transpose.flatten.toArray)
    val B_mat = new DenseMatrix(B.length, B.last.length, B.transpose.flatten.toArray)
    val D_mat = new DenseMatrix(D.length, D.last.length, D.transpose.flatten.toArray)
    val C_mat = A_mat * B_mat + D_mat
    val C = C_mat.data.toSeq.grouped(m).toSeq.transpose

    val num_matrices = tiles_m * tiles_k * tiles_n
    val stream_size = (4 + num_matrices) * mesh_dim - 2

    val A_mat_tiled = Seq.tabulate(tiles_m, tiles_k)((i, j) => A_mat(i * mesh_dim until (i + 1) * mesh_dim, j * mesh_dim until (j + 1) * mesh_dim))
    val A_tiled = A_mat_tiled.map(_.map(_.toDenseMatrix.data.toSeq.grouped(mesh_dim).toSeq.transpose))
    val B_mat_tiled = Seq.tabulate(tiles_k, tiles_n)((i, j) => B_mat(i * mesh_dim until (i + 1) * mesh_dim, j * mesh_dim until (j + 1) * mesh_dim))
    val B_tiled = B_mat_tiled.map(_.map(_.toDenseMatrix.data.toSeq.grouped(mesh_dim).toSeq.transpose))
    val D_mat_tiled = Seq.tabulate(tiles_m, tiles_n)((i, j) => D_mat(i * mesh_dim until (i + 1) * mesh_dim, j * mesh_dim until (j + 1) * mesh_dim))
    val D_tiled = D_mat_tiled.map(_.map(_.toDenseMatrix.data.toSeq.grouped(mesh_dim).toSeq.transpose))
    val C_mat_tiled = Seq.tabulate(tiles_m, tiles_n)((i, j) => C_mat(i * mesh_dim until (i + 1) * mesh_dim, j * mesh_dim until (j + 1) * mesh_dim))
    val C_tiled = C_mat_tiled.map(_.map(_.toDenseMatrix.data.toSeq.grouped(mesh_dim).toSeq.transpose))

    val zero_matrix = Seq.fill(mesh_dim, mesh_dim)(0)

    val A_matrices = Seq.tabulate(tiles_k * tiles_m * tiles_n)((i) => A_tiled(i / tiles_k / tiles_n)(i % tiles_k))
    val B_matrices = Seq.tabulate(tiles_k * tiles_m * tiles_n)((i) => B_tiled(i % tiles_k)(i / tiles_k % tiles_n))
    val D_matrices = Seq.tabulate(tiles_k * tiles_m * tiles_n)((i) => if i % tiles_k == 0 then D_tiled(i / tiles_k / tiles_n)(i / tiles_k % tiles_n) else zero_matrix)
    val C_matrices = Seq.tabulate(tiles_k * tiles_m * tiles_n)((i) => if i % tiles_k == tiles_k - 1 then C_tiled(i / tiles_k / tiles_n)(i / tiles_k % tiles_n) else zero_matrix)

    val A_stream = matrices2stream(A_matrices.map(_.transpose), stream_size, mesh_dim)
    val B_stream = matrices2stream(B_matrices, stream_size, mesh_dim)
    val D_stream = matrices2stream(D_matrices.map(_.reverse), stream_size, 0)
    val C_stream = matrices2stream(C_matrices.map(_.reverse), stream_size, 3 * mesh_dim - 1)

    val A_bytes = stream2byteStream(A_stream, params.horizontalGridTypeWidth / 8)
    val B_bytes = stream2byteStream(B_stream, params.verticalGridTypeWidth / 8)
    val D_bytes = stream2byteStream(D_stream, params.interPETypeWidth / 8)
    val C_bytes = stream2byteStream(C_stream, params.interPETypeWidth / 8)

    (A_bytes, B_bytes, D_bytes, C_bytes)