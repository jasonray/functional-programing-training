package week3

class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")

  def this(x: Int) = this(x, 1)

  def numer = x / g;
  def denom = y / g;

  private def gcd(a: Int, b: Int): Int = { if (b == 0) a else gcd(b, a % b) }
  private val g = gcd(x, y)

  def negative = new Rational(-1 * numer, denom)
  def unary_- = this.negative

  def add(x: Rational) = new Rational(numer * x.denom + x.numer * denom, denom * x.denom)
  def +(x: Rational) = this.add(x)

  def minus(x: Rational) = this.add(x.negative)
  def -(x: Rational) = this.minus(x)

  def less(x: Rational) = numer * x.denom < x.numer * denom
  def more(x: Rational) = !less(x)
  def max(x: Rational) = if (this.less(x)) x else this

  override def toString = numer + "/" + denom + "   (" + 1.0 * numer / denom + ")";
}