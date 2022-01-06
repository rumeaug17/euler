package org.rg.euler

import EulerPart2.TriangleSumProblems

import org.rg.euler.EulerPart1.isPalindrome
import org.rg.su3.*
import org.rg.su3.mesure.*

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.io.Source

/**
 * Euler partie 4
 */
object EulerPart4:

  val euler67: Mesure[Long] = Mesure {
    val source = scala.io.Source.fromFile("p067_triangle.txt")
    // TODO: add file to ressources

    val input = try source.getLines().toArray finally source.close()
    TriangleSumProblems(input)
  } named "euler67"

  //-- 56. Somme maximale des chiffres des nombres de la forme a^b avec a et b < 100
  val euler56: Mesure[Int] = Mesure {
    (for
      a <- 1 to 100
      b <- 1 to 100
    yield sumOfDigits(BigInt(a).pow(b))) max
  } named "euler56"

  //-- 52. Trouver le plus petit nombre positif tel que 2x, 3x, 4x, 5x et 6x sont des permutations de x
  val euler52: Mesure[Int] = Mesure {
    def pre(x: BigInt, l: Seq[String]): Boolean =
      val pp = (2 to 6).map(i => (x * i).toString)
      pp.forall(s => l.contains(s))

    (for
      x <- LazyList.from(10)
      l = perms(x)
      if pre(x, l)
    yield x) head
  } named "euler52"

  // -- 53. Combien y a-t-il de valeurs non forcément distinctes, supérieurs 1000000 pour C(n,p) = n! / (p! * (n-p)!) avec  1 <= n <= 100
  val euler53: Mesure[Int] = Mesure {
    def combs(n: Int, p: Int): BigInt =
      fact(n) / (fact(p) * (fact(n - p)))

    val c = for
      n <- 1 to 100
      p <- 1 to n
    yield combs(n, p)

    c.filter(_ > 1000000).length
  } named "euler53"

  /**
   * -- 55.
   * -- Donner le nombre des nombres dits de Lychrel inférieurs à 10 000 ?
   * -- Un nombre de lychred est un nombre qui ajouté au nombre fait par l'ordre inverse de ses chiffres ne donne jamais (après n itérations), un palindrome.
   * -- Ex. 349 + 943 = 1292 ; 1292 + 2921 = 4213 ; 4213 + 3124 = 7337 (qui est un palindrome. 349, (comme 1292 et 4213) ne sont pas des nombres de lychred. Par contre 196 en est un.
   */
  @tailrec
  def isLychred(n: BigInt, iter: Int): Boolean =
    iter match
      case x if x >= 50 => true
      case _ =>
        val r = BigInt(n.toString.reverse) + n
        if isPalindrome(r.toString) then
          false
        else
          isLychred(r, iter + 1)

  val euler55: Mesure[Int] = Mesure {
    (1 until 10000) filter (isLychred(_, 0)) length
  } named "euler55"

  //-- 97. Les 10 derniers chiffres du nombre 28433 * 2^7830457 + 1 (nombre premier non-Mersene)
  val euler97: Mesure[String] = Mesure {
    val n = (BigInt(28433) * BigInt(2).pow(7830457)) + 1
    // brute force. Pour faire mieux, utiliser le modulo pour ne garder dés le départ que les 10 dernier chiffres
    // avec une boucle
    n.toString.takeRight(10)
  } named "euler97"

  val euler27: Mesure[(BigInt, (Int, BigInt, Int))] = Mesure {
    val l = for
      b <- Primes().takeWhile { _ < 1000 }
      a <- Range(-999, 1000, 2)

      vl = LazyList.from(0).takeWhile { x => { val r = x * x + a * x + b; Primes.isPrime(r) } } length
    yield (a, b, vl)

    val m = l.maxBy(_._3)
    (m._1 * m._2, m)
  } named "euler27"

  val euler206 = Mesure {
    val regex = java.util.regex.Pattern.compile("1.2.3.4.5.6.7.8.9")
    val r = Range(scala.math.sqrt(10203040506070809l).toInt, scala.math.sqrt(19293949596979899l).toInt)
    10 * r.find(n => regex.matcher(BigInt(n).pow(2).toString).matches).get
  } named "euler206"

  /**
   * Problème 26
   *
   * fraction 1/d qui a le plus grand cycle  dans son développement décimal ( d < 1000)
   */
  val euler26: Mesure[Int] = Mesure {
    @tailrec
    def cycle_length(den: Int, reste: Int = 10, i: Int = 0): Int =
      (reste, i) match
        case (10, i) if i > 0 => i
        case _                => cycle_length(den, (reste % den) * 10, i + 1)

    Range(2, 1000).filter(i => i % 2 != 0 && i % 5 != 0).map(cycle_length(_) + 1).max
  } named "euler26"

  val euler42 = Mesure {
    val triangleNumbers = LazyList.from(1).map(n => n * (n + 1) / 2)
    inline def isTriangular(n: Int) = triangleNumbers.takeWhile(_ <= n).last == n
    inline def wordValue(s: String) = s.map(c => c.toInt - 'A'.toInt + 1).sum
    inline def res(l: List[String]) = l.map(wordValue(_)).filter(isTriangular(_)).length

    
    val resourceReader : Iterator[String] = Source.fromResource("euler42.data").getLines
    // TODO: create list from line
    val englishWords = List()
    res(englishWords)
  } named "euler42"

  @main def run_4() : Unit =
    val ListOfProblems : Seq[Mesure[_]] = Seq(
      euler52,
      euler53,
      euler55,
      euler56,
      euler97,
      euler27,
      euler206,
      euler26,
      euler42
    )

    ListOfProblems.foreach( m =>
      Console.println(m.collect(Total))
    )
