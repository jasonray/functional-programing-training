package example

import math.abs

object Week2 {
  val a = new Rational(2, 1)                      //> a  : example.Rational = 2/1   (2.0)
  a.toString()                                    //> res0: String = 2/1   (2.0)
  a.toString()                                    //> res1: String = 2/1   (2.0)

  val b = new Rational(1, 2)                      //> b  : example.Rational = 1/2   (0.5)
  val c = new Rational(1, 3)                      //> c  : example.Rational = 1/3   (0.3333333333333333)
  val d = new Rational(2, 3)                      //> d  : example.Rational = 2/3   (0.6666666666666666)

  c.negative                                      //> res2: example.Rational = 1/-3   (-0.3333333333333333)

  val e = a.add(b)                                //> e  : example.Rational = 5/2   (2.5)
  val f = a.minus(b)                              //> f  : example.Rational = 3/2   (1.5)

  val x = new Rational(1, 3)                      //> x  : example.Rational = 1/3   (0.3333333333333333)
  val y = new Rational(5, 7)                      //> y  : example.Rational = 5/7   (0.7142857142857143)
  val z = new Rational(3, 2)                      //> z  : example.Rational = 3/2   (1.5)

  (x.minus(y)).minus(z)                           //> res3: example.Rational = -79/42   (-1.880952380952381)
  x.minus(y).minus(z)                             //> res4: example.Rational = -79/42   (-1.880952380952381)

  x.less(y)                                       //> res5: Boolean = true
  y.more(x)                                       //> res6: Boolean = true
  x.max(y)                                        //> res7: example.Rational = 5/7   (0.7142857142857143)
  y.max(y)                                        //> res8: example.Rational = 5/7   (0.7142857142857143)

  y.less(z)                                       //> res9: Boolean = true
  z.more(y)                                       //> res10: Boolean = true
  y.max(z)                                        //> res11: example.Rational = 3/2   (1.5)
  z.max(y)                                        //> res12: example.Rational = 3/2   (1.5)

  val w = new Rational(5)                         //> w  : example.Rational = 5/1   (5.0)
  new Rational(4, 2)                              //> res13: example.Rational = 2/1   (2.0)

  x.add(y)                                        //> res14: example.Rational = 22/21   (1.0476190476190477)
  x add y                                         //> res15: example.Rational = 22/21   (1.0476190476190477)
  x + y                                           //> res16: example.Rational = 22/21   (1.0476190476190477)

  y.minus(x)                                      //> res17: example.Rational = 8/21   (0.38095238095238093)
  y minus x                                       //> res18: example.Rational = 8/21   (0.38095238095238093)
  y - x                                           //> res19: example.Rational = 8/21   (0.38095238095238093)
  
  x.negative                                      //> res20: example.Rational = 1/-3   (-0.3333333333333333)
  -x                                              //> res21: example.Rational = 1/-3   (-0.3333333333333333)
  
  new Rational(-1,2)                              //> res22: example.Rational = 1/-2   (-0.5)
  new Rational(1,-2)                              //> res23: example.Rational = 1/-2   (-0.5)
  new Rational(-1,-2)                             //> res24: example.Rational = 1/2   (0.5)

}

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