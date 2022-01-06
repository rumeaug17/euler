package org.rg.euler

import EulerPart1.isPalindrome
import org.rg.su3.*
import org.rg.su3.mesure.*

import scala.annotation.tailrec
import scala.language.postfixOps

/**
 * Euler partie 3
 * Problèmes 25 à xxx
 */
object EulerPart3:

  /**
   * Problème 25
   *
   * Premier terme de Fibonacci qui contient 1000 chiffres
   */
  val euler25: Mesure[Int] = Mesure {
    fibStream.takeWhile(_ < BigInt(10).pow(999)).length
  } named "euler25"

  /**
   * Problème 28
   *
   * somme des nombres se trouvant sur les diagonales d'une spirale 1001x1001
   *
   */
  val euler28: Mesure[Long] = Mesure {
    (
      for
        k <- Range(3,1003, 2)
      yield 4L * k * k - 6 * (k - 1)
    ).sum + 1
  } named "euler28"

  /**
   * Problème 29
   *
   * Combien y a-t-il de termes distincs dans la séquence fabriquée par a**b avec 2 <= a <= 100 et 2 <= b <= 100 ?
   */
  val euler29: Mesure[Int] = Mesure {
    val l = for
      a <- 2 to 100
      b <- 2 to 100
    yield BigInt(a).pow(b)
    l.distinct.length
  } named "euler29"

  /**
   *  Problème 30
   *
   *  Somme des nombres qui peuvent s'écrire sous la forme de la somme de leurs chiffres à la puissance 5
   */
  val euler30: Mesure[Any] = Mesure {
    def isSum(n: Long) = n == n.toString.map(c => BigInt(c.asDigit).pow(5)).sum
    (2 to 1000000).filter(isSum).sum
  } named "euler30"

  /**
   * Problème 32
   *
   * Un nombre de n chiffres est pandigital s'il utilise exactement une fois chaque chiffre de 1 à n
   * Un produit est 9-pandigital si la concaténation des deux opérandes et du résultat donne un nombre 9-pandigital
   *
   * Trouver la somme des produits 9-pandigitaux (chaque produit ne doit étre considéré qu'une fois)
   */
  inline def isPandigital(n: Int)(s: String): Boolean =
    s.length == n && (1 to n).forall(i => s.map(_.asDigit).contains(i))

  inline private def prodIsPandigital(a: Long, b: Long, p: Long) =
    isPandigital(9)(s"$p$a$b")

  val euler32: Mesure[Long] = Mesure {
    (for
      a <- 2L to 10000
      b <- 2L to 10000 / a
      p = a * b
      if prodIsPandigital(a, b, p)
    yield p).distinct.sum
  } named "euler32"

  /**
   * Probléme 34
   *
   * Somme de tous les nombres qui sont égaux à la somme de la factorielle de ses chiffres
   * 1! + 4! + 5! = 145
   */
  val euler34: Mesure[Int] = Mesure {
    (for
      i <- 3 to 1000000
      if i.toString.map(c => fact(c.asDigit)).sum == i
    yield i).sum
  } named "euler34"

  /**
   * Problème 35
   *
   * Un nombre premier est circulaire si toutes ses rotations sont premiers.
   * Combien y a-t-il de nombre premiers circulaires inférieurs à un million ?
   */
  val euler35: Mesure[Int] = Mesure {
    (for
      i <- Primes() takeWhile (_ < 1000000)
      if perms(i).forall(x => BigInt(x).isProbablePrime(40))
    yield i).length
  } named "euler35"

  /**
   * Probléme 36
   *
   * Somme des nombres inférieurs à 1000000 qui sont palindrome en base deux et en base 10
   */
  val euler36: Mesure[Int] = Mesure {
    (1 until 1000000).filter(i => isPalindrome(i.toString) && isPalindrome(i |> base(2))).sum
  } named "euler36"

  /**
   * Problème 40
   *
   * Soit d une fraction décimale irrationelle construite  par la concaténation des nombres naturels
   * => 0.123456789101112131415161718192021...
   * d(n) est le niéme chiffre de la partie décimale
   * Quelle est la valeur de "d(1) x d(10) x d(100) x d(1000) x d(10000) x d(100000) x d(1000000)"
   */
  private lazy val dfd = (1 to 1000000).mkString
  private inline def d(n : Int) = dfd(n-1).asDigit
  val euler40: Mesure[Int] = Mesure {
    d(1) * d(10) * d(100) * d(1000) * d(10000) * d(100000) * d(1000000)
  } named "euler40"

  /**
   * Problème 41
   * Le plus grand nombre premier qui soit n-pandigital
   *
   * Les nombres 9-pandigitaux, 8-p, 6-p, 5-p, 3-p et 2-p sont tous divisibles par 3
   * tips : tester la primalité des permutations, elles sont toutes pandigitales par construction
   */
  val euler41: Mesure[Int] = Mesure {
    ("1234567".permutations ++ "1234".permutations).map(x => x.mkString.toInt).filter( x => Primes.isPPrime(x)).max
  } named "euler41"

  /**
   * Probléme 45
   *
   * Trouver le premier nombre triangulaire supérieur é 40755 qui est également pentagonal et hexagonal.
   * Un nombre est triangulaire s'il exite n tq t = n(n+1)/2
   * Un nombre est pentagonal si P = n(3n-1)/2
   * Un nombre est hexagonal si H = n(2n-1)
   *
   * tips : tous les nombres hexa sont aussi triangulaire !
   */
  /* il suffit de trouver un nombre hexa qui est aussi pentagonal.
   * Pour le vérifier, on effectue un test qui au lieu de générer la liste des nombres, par la formule quadratique (équation du segond degré)
   * P = n(3n-1)/2
   *   => 3n**2 - n - 2P = 0
   *   => (racine positive (-b + sqrt (b**2-4ac)) / 2a)
   *     n = (1 + sqrt (1+24P)) / 6 est solution,
   *  donc pour P donné, si on a un n entier c'est bon
   */

  lazy val lofHex: Seq[Long] = LazyList.from(144).map(n => n * (2L * n - 1))
  inline private def isValidLong(d : Double) = d.ceil == d.floor
  inline private def isPen(n : Long) = isValidLong((1 + scala.math.sqrt(1d+24*n)) / 6)

  val euler45: Mesure[Any] = Mesure {
    lofHex.filter(isPen(_)).head
  } named "euler45"

  /**
   * Problème 48
   *
   *
   */
  val euler48: Mesure[String] = Mesure {
    (1 to 1000).map(i => BigInt(i).pow(i)).sum.toString.takeRight(10)
  } named "euler48"

  /**
   * Problème 43
   */
  val euler43 = Mesure {
    def filter(p: String)(begin: Int, end: Int, modulo: Int): Boolean =
      p.substring(begin, end).toInt % modulo == 0

    val pSS = for
      p <- "1234567890".permutations
      if p.charAt(0) != '0'
      f = filter(p) _
      if f(1, 4, 2)
      if f(2, 5, 3)
      if f(3, 6, 5)
      if f(4, 7, 7)
      if f(5, 8, 11)
      if f(6, 9, 13)
      if f(7, 10, 17)
    yield p

    pSS map (BigInt(_)) sum
  } named "euler43"

  @main def run_3() : Unit =
    val ListOfProblems : Seq[Mesure[_]] = Seq(
      euler25,
      euler28,
      euler29,
      euler30,
      euler32,
      euler34,
      euler35,
      euler36,
      euler40,
      euler41,
      euler45,
      euler48,
      euler43
    )

    ListOfProblems.foreach( m =>
      Console.println(m.collect(Total))
    )