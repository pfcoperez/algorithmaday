package org.pfcoperez.dailyalgorithm

import scala.collection.immutable.Stream

object Numbers {

  private lazy val N: Stream[BigInt] = BigInt(1) #:: N.map(_ + BigInt(1))

  /**
   * Infinite stream of prime numbers.
   *
   * Asking for n elements takes: O(n^2)
   *
   */
  lazy val primesStream: Stream[BigInt] =
    BigInt(2) #:: N.drop(2).filter(i =>
      primesStream takeWhile (p => p * p <= i) forall (i % _ != 0))

  /**
   * Binomial coefficient-(n,k) or ...
   * number of ways of distributing n elements in groups of size k.
   * O(n^2)
   */
  def binomialCoef(n: Int, k: Int): Option[Long] = {
    type Cache = Map[(Int, Int), Long]
    def memoizedBinCoef(ni: Int, ki: Int)(cache: Cache): (Long, Cache) =
      if (ni == ki) 1L -> cache
      else if (ki == 1) ni.toLong -> cache
      else cache.get(ni -> ki).map(_ -> cache) getOrElse {
        val (a, aCache) = memoizedBinCoef(ni - 1, ki - 1)(cache)
        val (b, bCache) = memoizedBinCoef(ni - 1, ki)(aCache)
        (a + b, bCache.updated(ni -> ki, a + b))
      }
    if (n < 1 || k < 1 || n < k) None
    else Some(memoizedBinCoef(n, k)(Map.empty)._1)
  }

  import scala.language.experimental.macros
  import scala.reflect.macros.blackbox

  /**
   * Compile time prime numbers generation. Values get generated at compilation time and a vector with the generated
   * constant is built at run-time.
   *
   * Run-time: O(n), as the vector instance is built at run-time.
   * Compile-time: O(n^2)
   * @param n Number of prime to generate.
   * @return a [[Vector[BigInt]] with `n` primes
   */
  def compiledPrimes(n: Int): Vector[BigInt] = macro compiledPrimesImp

  def compiledPrimesImp(ctx: blackbox.Context)(n: ctx.Expr[Int]): ctx.Expr[Vector[BigInt]] = {
    import ctx.universe._

    val nn: Int = n match {
      case ctx.Expr(Literal(Constant(constantN: Int))) => constantN
      case _ => throw new IllegalArgumentException("Compiled primes generation size must be a literal value")
    }

    val generatedValues = primesStream.take(nn).toList

    implicit val bigIntLiftable = Liftable[BigInt] { bi =>
      ctx.Expr(q"scala.math.BigInt.apply(${bi.toString})").tree
    }

    ctx.Expr(q"scala.collection.immutable.Vector(..$generatedValues)")
  }

}
