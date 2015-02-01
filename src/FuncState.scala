/**
 * User: Jason Weng
 */


class FuncState {
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt()

  def unit[A](a: A) :Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }


  val doubleRand : Rand[Double] = map(int)((x) => math.abs(x.toDouble) / Int.MaxValue)


}


object test {
  def main(args: Array[String]):Unit = {
    val state: FuncState = new FuncState()
    val rng1 = SimpleRNG(1)
    val rand2: (Double, RNG) = state.doubleRand(rng1)
    val rand3: (Double, RNG) = state.doubleRand(rand2._2)
    println(rand2._1)
    println(rand3._1)

  }
}


