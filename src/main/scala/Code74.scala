import breeze.linalg._
import GF2Semirings._

object Code74 {
  val Core = DenseMatrix((One, One, One, Zero), (Zero, One, One, One), (One, Zero, One, One))
  val Reverse = DenseMatrix.horzcat(DenseMatrix.eye[GF2](4), DenseMatrix.zeros[GF2](4, 3))
  val Generator = DenseMatrix.vertcat(DenseMatrix.eye[GF2](4), Core)
  val Check = DenseMatrix.horzcat(Core, DenseMatrix.eye[GF2](3))
  
  type SeqGF2 = Seq[GF2]
  type DenseVecGF2 = DenseVector[GF2]
  
  implicit def Seq2Vec(s: SeqGF2): DenseVecGF2 = DenseVector(s:_*)
  val str2Bits = (s: String) => for (c <- s ; v <- vs ; val res = c & v) yield if (res > 0) One else Zero
  val bits2Chars = (xs: SeqGF2) => for (v <- xs grouped 8) yield (v zip vs map { case (x, y) => x * y } sum).toChar
  val majorityVote = (vs: SeqGF2) => vs.groupBy(identity).maxBy(_._2.size)._1
  val doCheck = (v: DenseVecGF2) => correctBits(Check * v, v)
  val zeroVec = DenseVector(Zero, Zero, Zero)
  lazy val vs = 0 until 8 map (1 << _)
  
  def correctBits(res: DenseVecGF2, v: DenseVecGF2) = {
    if (zeroVec == res) v else (for (n <- 0 until Check.cols) yield if (Check(::, n) == res) One else Zero) + v
  }
  
  def encodeWithRepetition(message: String, re: Int) = {
    val bitMessage = str2Bits(message) grouped 4 flatMap (Generator * Seq2Vec(_) toArray)
    val repeatedBitMessage = for (x <- bitMessage) yield List.fill(re)(x)
    repeatedBitMessage.flatten
  }
}
