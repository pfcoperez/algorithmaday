import scala.collection.immutable.Stream

object Numbers {

  private lazy val N: Stream[BigInt] = BigInt(1) #:: N.map(_ + BigInt(1))

  /** Infinite stream of prime numbers.
    * 
    * Asking for n elements takes: O(n^2)
    * 
    */
  lazy val primesStream: Stream[BigInt] =
    BigInt(2) #:: N.drop(2).filter ( i =>
      primesStream takeWhile (p => p*p <= i) forall (i % _ != 0)
    )

}
