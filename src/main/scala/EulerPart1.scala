package org.rg.euler

import org.rg.su3.*
import org.rg.su3.mesure.*

import scala.annotation.tailrec
import scala.language.postfixOps

/**
 * Euler partie 1
 * Problèmes 1 à 12
 */
object EulerPart1:

  /**
   * Problème 1
   *
   * Somme des multiples de 3 ou 5 inférieurs à 1000
   * http://projecteuler.net/problem=1
   */
  val euler01: Mesure[Int] = Mesure { (3 until 1000).filter(i  => i % 3 == 0 || i % 5 == 0).sum } named "euler01"

  /**
   * Problème 2
   *
   * Somme des termes pairs de la suite de fibonacci inférieurs é 4 million
   */
  val euler02: Mesure[BigInt] = Mesure {
    fibStream filter (_ % 2 == 0)  takeWhile (_ <= 4000000) sum
  } named "euler02"

  /**
   * Problème 3
   *
   * Quel est le plus grand facteur premier du nombre 600851475143 ?
   */
  val euler03: Mesure[BigInt] = Mesure {
    primeFactors(600851475143l).last
  } named "euler03"

  /**
   * Problème 4
   *
   * Le plus grand palindrome construit à partir du produit de deux nombres de trois chiffres
   */
  inline def isPalindrome(l: String): Boolean =
    l == l.reverse

  val euler04: Mesure[Int] =  Mesure {
    (for
      n1 <- 100 until 1000
      n2 <- 100 until 1000
      p = n1 * n2
      if isPalindrome(p.toString)
    yield p) max
  } named "euler04"

  /**
   * Problème 5
   *
   * Le plus petit entier positif divisible par tous les nombres entre 1 et 20
   *
   * Le plus petit divisible par les nombres entre 1 et 10 est 2520 c'est dans l'énoncé.
   * Donc notre nombre est forcément un multiple de 2520, divisible par tous les nombres entre 11 et 20
   *
   * ou encore utiliser lcm (plus petit diviseur commun) cf http://basildoncoder.com/blog/2008/06/10/project-euler-problem-5/
   */
  val euler05: Mesure[Int] =  Mesure {
    (for
      i <- LazyList.from(2520, 2520)
      if (11 to 20).forall(i % _ == 0)
    yield i) head
  } named "euler05"

  val euler05bis: Mesure[Int] = Mesure {
    List.range(1, 20).foldLeft(1)( (x, y) => lcm(x, y))
  } named "euler05 bis"

  /**
   * Problème 6
   *
   * Différence entre la somme des carrés et le carré de la somme pour les 100 premiers nombres naturels
   */
  val euler06: Mesure[BigInt] = Mesure {
    val range = BigInt(1) to BigInt(100)
    range.sum.pow(2) - (range map (_.pow(2)) sum)
  } named "euler06"

  /**
   * Problème 7
   *
   *  Trouver le 10001iéme nombre premier
   */
  val euler07: Mesure[BigInt] = Mesure {
    Primes(10000)
  } named "euler07"

  /**
   * Problème 8
   *
   * Trouver le plus grand produit de 13 chiffres continus dans un nombre de 1000 chiffres
   */
  val euler08: Mesure[Long] = Mesure {
    val conslen = 13
    @tailrec def prod(l : List[Long], acc : List[Long]) : List[Long] = l match
      case _ if l.length == conslen => l.product :: acc
      case s :: xs => prod(xs, l.take(conslen).product :: acc)

    val s =
      "73167176531330624919225119674426574742355349194934"+
      "96983520312774506326239578318016984801869478851843"+
      "85861560789112949495459501737958331952853208805511"+
      "12540698747158523863050715693290963295227443043557"+
      "66896648950445244523161731856403098711121722383113"+
      "62229893423380308135336276614282806444486645238749"+
      "30358907296290491560440772390713810515859307960866"+
      "70172427121883998797908792274921901699720888093776"+
      "65727333001053367881220235421809751254540594752243"+
      "52584907711670556013604839586446706324415722155397"+
      "53697817977846174064955149290862569321978468622482"+
      "83972241375657056057490261407972968652414535100474"+
      "82166370484403199890008895243450658541227588666881"+
      "16427171479924442928230863465674813919123162824586"+
      "17866458359124566529476545682848912883142607690042"+
      "24219022671055626321111109370544217506941658960408"+
      "07198403850962455444362981230987879927244284909188"+
      "84580156166097919133875499200524063689912560717606"+
      "05886116467109405077541002256983155200055935729725"+
      "71636269561882670428252483600823257530420752963450"

    val p = prod(s.map(_.asDigit toLong).toList, List())
    p.max
    //s.toList.map(_.asDigit toLong).sliding(conslen).map(_.product).max
  } named "euler08"

  /**
   * Problème 9
   *
   * Trouver le triplet de pythagore pour lequel a + b + c = 1000 et donner le produit abc.
   * Un triplet de pythagore doit vérifier a^2 + b^2 = c^2
   */
  val euler09: Mesure[Int] = Mesure {
    (for
      a <- 1 to 1000
      b <- 1 to (1000 - a)
      if a < b
      c = 1000 - a - b
      if a*a + b*b == c*c
    yield a*b*c) head
  } named "euler09"

  /**
   * Problème 10
   *
   * Somme des nombres premiers inférieurs à 2 000 000
   * Attention au débordement avec  des Int
   */
  val euler10: Mesure[BigInt] = Mesure {
    Primes() takeWhile (_ < 2000000) sum
  } named "euler10"

  val euler11: Mesure[Option[Int]] = Mesure {
    val grid =  (
      "08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08 " +
      "49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00 " +
      "81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65 " +
      "52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91 " +
      "22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80 " +
      "24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50 " +
      "32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70 " +
      "67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21 " +
      "24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72 " +
      "21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95 " +
      "78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92 " +
      "16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57 " +
      "86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58 " +
      "19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40 " +
      "04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66 " +
      "88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69 " +
      "04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36 " +
      "20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16 " +
      "20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54 " +
      "01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"
      ).split(' ').map(_.toInt)

    val res = for
      i <- grid.indices
      p1 = (i % 20 < 17).option(grid(i) * grid(i+1) * grid(i+2) * grid(i+3))
      p2 = (i < 340).option(grid(i) * grid(i+20) * grid(i+40) * grid(i+60))
      p3 = (i % 20 < 17 && i < 340).option(grid(i) * grid(i+21) * grid(i+42) * grid(i+63))
      p4 = (i % 20 > 3 && i < 340).option(grid(i) * grid(i+19) * grid(i+38) * grid(i+57))
    yield List(p1, p2, p3, p4)

    res.flatten.max
  } named "euler11"

  /**
   * Problème 12
   *
   * Quel est le premier nombre triangulaire à avoir 500 diviseurs ou plus ?
   *
   * Un nombre triangulaire de rang n est la somme des entiers naturels inférieurs ou égal à n
   * t(1) = 1
   * t(2) = 1+2 = 3
   * t(3) = 1+2+3 = 6
   * t(4) = 1+2+3+4 = 10
   * t(7) = 1+2+3+4+5+6+7 = 28 [ 28 a 6 diviseurs : 1, 2, 4, 7, 14, 28]
   *
   */

  lazy val triangles: Seq[Int] = LazyList.from(1).scanLeft(0)(_ + _)
  val euler12: Mesure[Option[(Int, Int)]] = Mesure {
    triangles.map(n => divisors(BigInt(n))).zip(triangles).find(x => x._1 >= 500)
  } named "euler12"

  @main def run_1() : Unit =
    val ListOfProblems: Seq[Mesure[_]] = Seq(
      euler01,
      euler02,
      euler03,
      euler04,
      euler05, euler05bis,
      euler06,
      euler07,
      euler08,
      euler09,
      euler10,
      euler11,
      euler12
    )

    ListOfProblems.foreach( m =>
      Console.println(m.collect(Total))
    )



