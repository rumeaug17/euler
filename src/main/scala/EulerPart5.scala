package org.rg.euler

import org.rg.su3.*
import org.rg.su3.Primes.isPrime
import org.rg.su3.mesure.*

import scala.annotation.tailrec

object EulerPart5:

  /**
   * Problème 37
   */
  val euler37: Mesure[BigInt] = Mesure {

    def truncLeft(n: String) =
      n.scanLeft("") {
        (s: String, c: Char) => s"$s$c"
      }.tail.init

    def truncRight(n: String) =
      n.scanRight("") {
        (c: Char, s: String) => s"$c$s"
      }.init.tail

    val l = for
      p <- Primes()
      if p > 7
      if truncLeft(p.toString).forall(sp => Primes.isPrime(sp.toInt))
      if truncRight(p.toString).forall(sp => Primes.isPrime(sp.toInt))
    yield p

    l.take(11).sum
  } named "euler37"

  /**
   * Problème 38
   */
  val euler38: Mesure[String] = Mesure {
    val r = for
      n <- LazyList.from(9999, -1)
      i <- 1 to 9
      r = (1 to i).map(_ * n).map(_.toString).foldLeft("")(_ + _)
      if EulerPart3.isPandigital(9)(r)
    yield r
    r.head
  } named "euler38"

  /**
   * Problème 39
   */
  val euler39: Mesure[Int] = Mesure {
    val triangles = for
      a <- 499 to 1 by -1
      b <- a to 499
      c <- (a * a + b * b).sqrtI if c > 0
      p = a + b + c
      if p <= 1000
    yield p
    triangles.groupBy(identity).maxBy(_._2.size)._1
  } named "euler39"

  val euler44: Mesure[(Any, Int, Int)] = Mesure {
    inline def p(n: Int) = n * (3 * n - 1) / 2 //> p: (n: Int)Int

    /*  y  = p(x) = x(3x-1)/2
     *  2y = 3x² - x
     *  3x² - x - 2y = 0
     *  x = (-b + sqrt(b² - 4ac))/2a avec a = 3, b = -1, c = -2y (formule quadratique)
     *  x =  (1 + sqrt(1 + 24y))/6
     */
    def isPentagonNumber(p: Int) =
      val t = p * 24 + 1
      val s = scala.math.sqrt(t)
      s * s == t && s % 6 == 5

    val pn = for
      j <- LazyList.from(2)
      i <- j - 1 to 1 by -1
      if isPentagonNumber(p(i) + p(j))
      d = p(j) - p(i)
      if isPentagonNumber(d)
    yield (scala.math.abs(d), i, j)
    pn.head //c'est forcément le premier qui a la plus petite différence
  } named "euler44"

  val euler46: Mesure[Int] = Mesure {
    def isSum(n: Int) =
      val sum = LazyList.from(1).map(s => n - 2 * s * s).takeWhile(_ > 0)
      sum.exists(p => p != 1 && Primes.isPrime(p))

    val candidates = LazyList.from(35, 2).filter(!Primes.isPrime(_)).filter(!isSum(_))
    candidates.head
  } named "euler46"

  val euler47: Mesure[(Int, Int, Int, Int)] = Mesure {
    def distinctFactors(i: BigInt) =
      primeFactors(i).groupBy(identity).toList.map(t => t._2.product)

    (for
      i1 <- LazyList.from(200)
      i2 = i1 + 1
      i3 = i1 + 2
      i4 = i1 + 3
      di1 = distinctFactors(i1) if di1.length == 4
      di2 = distinctFactors(i2) if di2.length == 4
      if di1 != di2
      di3 = distinctFactors(i3) if di3.length == 4
      if di1 != di3 && di2 != di3
      di4 = distinctFactors(i4) if di4.length == 4
      if di1 != di4 && di2 != di4 && di3 != di4
    yield (i1, i2, i3, i4)).head

  } named "euler47"

  inline def checkLetter(s1 : String, s2 : String) : Boolean =
      s1.sorted == s2.sorted

  val euler49: Mesure[Seq[String]] = Mesure {
    // trouver une séquence croissante de trois nombres de 4 chiffres, permutations, qui sont premiers et dont l'écart enbtre chaque est constant
    val primes = Primes().dropWhile(_ < 1000).takeWhile(_ < 10000)
    (for
      p <- primes
      if p != BigInt(1487)
      ecart <- 1 to 10000
      p2 = p + ecart
      if p2 < 10000
      if checkLetter(p.toString, p2.toString)
      if primes.contains(p2)
      p3 = p2 + ecart
      if p3 < 10000
      if checkLetter(p.toString, p3.toString)
      if primes.contains(p3)
    yield s"${p}_${p2}_${p3}").toList
  } named "euler49"

  val euler92: Mesure[Int] = Mesure {

    def sumDigitSquare(n: Long) : Long =
      n match
        case 0 => 0
        case 1 => 1
        case _ =>
          val mod = n % 10
          val div = n / 10
          mod * mod + sumDigitSquare(div)

    @tailrec
    def reduce(n : Long): Boolean =
      val r = sumDigitSquare(n)
      r match
        case 1 | 44 | 32 | 13 | 10 => false
        case 89 | 85 | 145 | 42 | 20 | 4 | 16 | 37 | 58 => true
        case _ => reduce(r)

    val lst = for
      i <- 2 to 10000000
      if reduce(i)
    yield i

    lst.length
  } named "euler92"

  @main def run_5() : Unit =
    val ListOfProblems : Seq[Mesure[_]] = Seq(
      euler37,
      euler38,
      euler39,
      euler44,
      euler46,
      euler47,
      euler49,
      euler92
    )

    ListOfProblems.foreach( m =>
      Console.println(m.collect(Total))
    )