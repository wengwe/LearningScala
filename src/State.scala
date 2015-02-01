import scala.annotation.tailrec

/**
 * User: Jason Weng
 */

trait RNG {
  def nextInt():(Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt(): (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
    val nextRNG = SimpleRNG(newSeed) // The next state, which is an `RNG` instance created from the new seed.
    val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
    (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
  }
}

object State {

  def ints(count: Int)(rng: RNG) :(List[Int], RNG) = {
    if (count == 0) {
      (List(), rng)
    } else {
      val (v, nextRNG) = rng.nextInt()
      val (vt, xrng) = ints(count-1) (nextRNG)
      (v::vt, xrng)
    }
  }

  def ints2(count: Int)(rng: RNG) :(List[Int], RNG) = {
     @tailrec
     def getInts(count: Int, rng: RNG, xs: List[Int]) : (List[Int], RNG) = {
       if (count == 0) {
         (xs, rng)
       } else {
         val (x, nextRNG) = rng.nextInt()
         getInts(count - 1, nextRNG, x :: xs)
         //val (lx, xrng) = getInts(count-1)(nextRNG)(l::v)
       }

     }
    getInts(count, rng, List())
  }

  def main(args: Array[String]) : Unit = {
    val initRNG = SimpleRNG(1)
    val rlist = ints2(6)(initRNG)
    println(rlist)
  }
}
