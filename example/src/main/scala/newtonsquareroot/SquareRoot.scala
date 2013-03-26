package newtonsquareroot

/**
 * Created with IntelliJ IDEA.
 * User: jasonray
 * Date: 3/26/13
 * Time: 10:26 AM
 * To change this template use File | Settings | File Templates.
 */
class SquareRoot {

  def squareRoot(x: Double): Double = {
    val PRECISION: Double = x / 1000000

    def closeEnough(guess: Double): Boolean = {
      if (delta(guess) < PRECISION)
        true
      else
        false
    }

    def delta(guess: Double): Double = {
      Math.abs((guess * guess) - x)
    }


    def improve(guess: Double): Double = {
      (guess + x / guess) / 2
    }

    def squareRootIteration(guess: Double): Double = {
      //println("current guess is " + guess + "; delta=" + delta(guess));
      if (closeEnough(guess)) guess
      else squareRootIteration(improve(guess))
    }

    println("finding square root of " + x + " with precision=" + PRECISION)
    val n = squareRootIteration(1);
    println("final estimate is " + n + "; delta=" + delta(n));
    return n

  }

}
