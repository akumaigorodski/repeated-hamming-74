import breeze.linalg.operators._
import breeze.numerics._
import breeze.storage._
import breeze.linalg._
import scala.reflect._
import breeze.math._

object GF2Semirings {
  implicit object GF2DefaultArrayValue extends DefaultArrayValue[GF2] {
    override def value = Zero
  }
  
  implicit object GF2Semiring extends Semiring[GF2] {
  	def defaultArrayValue = GF2DefaultArrayValue
    def manifest = classTag[GF2]
    def zero = Zero
    def one = One
    
    def ==(a: GF2, b: GF2) = a == b
    def !=(a: GF2, b: GF2) = a != b
    def +(a: GF2, b: GF2) = a + b
    def *(a: GF2, b: GF2) = a * b
  }
 	
  implicit def opMulMatrix_DM_DV_Semiring[T:Semiring:ClassTag:DefaultArrayValue]:OpMulMatrix.Impl2[DenseMatrix[T], 
 	DenseVector[T], DenseVector[T]] = new OpMulMatrix.Impl2[DenseMatrix[T], DenseVector[T], DenseVector[T]] {
 	  
    implicit val ring = implicitly[Semiring[T]]
    override def apply(a: DenseMatrix[T], b: DenseVector[T]) = {
      require(a.cols == b.length)
      val res = DenseVector.zeros[T](a.rows)
      var c = 0
      while(c < a.cols) {
        var r = 0
        while (r < a.rows) {
          val v = a(r, c)
          res(r) = ring.+(res(r), ring.*(v, b(c)))
          r += 1
        }
        c += 1
      }
      
      res
    }
  }
  
  implicit def mulMatrix_DM_S_Semiring[T:Semiring:ClassTag:DefaultArrayValue]:OpMulMatrix.Impl2[DenseMatrix[T], 
    T, DenseMatrix[T]] = new OpMulMatrix.Impl2[DenseMatrix[T], T, DenseMatrix[T]] {
    
    implicit val ring = implicitly[Semiring[T]]
    override def apply(a: DenseMatrix[T], b: T) = {
      val res = DenseMatrix.zeros[T](a.rows, a.cols)
      var c = 0
      while(c < a.cols) {
        var r = 0
        while (r < a.rows) {
          res(r, c) = ring.*(a(r, c), b)
          r += 1
        }
        c += 1
      }
      
      res
    }
  }
  
  implicit def opAdd_DM_S_Semiring[T:Semiring:ClassTag:DefaultArrayValue]:OpAdd.Impl2[DenseMatrix[T], 
    T, DenseMatrix[T]] = new OpAdd.Impl2[DenseMatrix[T], T, DenseMatrix[T]] {
    
    implicit val ring = implicitly[Semiring[T]]
    override def apply(a: DenseMatrix[T], b: T) = {
      val res = DenseMatrix.zeros[T](a.rows, a.cols)
      var c = 0
      while(c < a.cols) {
        var r = 0
        while (r < a.rows) {
          res(r, c) = ring.+(a(r, c), b)
          r += 1
        }
        c += 1
      }
      
      res
    }
  }
  
  implicit def opAdd_DM_DM_Semiring[T:Semiring:ClassTag:DefaultArrayValue]:OpAdd.Impl2[DenseMatrix[T], 
    DenseMatrix[T], DenseMatrix[T]] = new OpAdd.Impl2[DenseMatrix[T], DenseMatrix[T], DenseMatrix[T]] {
    
    implicit val ring = implicitly[Semiring[T]]
    override def apply(a: DenseMatrix[T], b: DenseMatrix[T]) = {
      require(a.rows == b.rows)
      require(a.cols == b.cols)
      
      val res = DenseMatrix.zeros[T](a.rows, a.cols)
      var c = 0
      while(c < a.cols) {
        var r = 0
        while (r < a.rows) {
          res(r, c) = ring.+(a(r, c), b(r, c))
          r += 1
        }
        c += 1
      }
      
      res
    }
  }
}