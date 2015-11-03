import scala.math.Ordering

trait GF2 {
  def + (that: GF2): GF2
  def * (that: GF2): GF2
  def * (that: Int): Int
  
  def / (that: GF2) = that match {
    case Zero => throw new IllegalArgumentException("Div by 0")
    case _ => this
  }
}

object Zero extends GF2 {
  override def toString = "Zero"
  def + (that: GF2) = that
  def * (that: GF2) = this
  def * (that: Int) = 0
}

object One extends GF2 {
  override def toString = "One"
  def + (that: GF2) = that match { case One => Zero ; case _ => this }
  def * (that: GF2) = that match { case One => this ; case _ => that }
  def * (that: Int) = that
}

// Type class implementation
trait GF2IsNumeric extends Numeric[GF2] {
  def minus(x: GF2, y: GF2): GF2 = plus(x, y)
  def times(x: GF2, y: GF2): GF2 = x * y
  def plus(x: GF2, y: GF2): GF2 = x + y
  def negate(x: GF2): GF2 = x
  
  def fromInt(x: Int): GF2 = if (x > 0) One else Zero
  def toInt(x: GF2): Int = if (x == One) 1 else 0
  def toDouble(x: GF2): Double = toInt(x)
  def toFloat(x: GF2): Float = toInt(x)
  def toLong(x: GF2): Long = toInt(x)
}

trait GF2Ordering extends Ordering[GF2] {
  override def compare(a: GF2, b: GF2) = if (a == b) 0 else if (b == One) 1 else -1
}
