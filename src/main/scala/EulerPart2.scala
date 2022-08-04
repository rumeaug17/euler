package org.rg.euler

import org.rg.su3.*
import org.rg.su3.mesure.*

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.io.Source

/**
 * Euler partie 2
 * Problèmes 13 à 24
 */
object EulerPart2:

  /**
   * Probléme 13
   *
   * Les 10 premiers chiffres de la somme de 100 nombres de 50 chiffres chacun
   *
   */

  def listProb13 = List(
    BigInt("37107287533902102798797998220837590246510135740250"),
    BigInt("46376937677490009712648124896970078050417018260538"),
    BigInt("74324986199524741059474233309513058123726617309629"),
    BigInt("91942213363574161572522430563301811072406154908250"),
    BigInt("23067588207539346171171980310421047513778063246676"),
    BigInt("89261670696623633820136378418383684178734361726757"),
    BigInt("28112879812849979408065481931592621691275889832738"),
    BigInt("44274228917432520321923589422876796487670272189318"),
    BigInt("47451445736001306439091167216856844588711603153276"),
    BigInt("70386486105843025439939619828917593665686757934951"),
    BigInt("62176457141856560629502157223196586755079324193331"),
    BigInt("64906352462741904929101432445813822663347944758178"),
    BigInt("92575867718337217661963751590579239728245598838407"),
    BigInt("58203565325359399008402633568948830189458628227828"),
    BigInt("80181199384826282014278194139940567587151170094390"),
    BigInt("35398664372827112653829987240784473053190104293586"),
    BigInt("86515506006295864861532075273371959191420517255829"),
    BigInt("71693888707715466499115593487603532921714970056938"),
    BigInt("54370070576826684624621495650076471787294438377604"),
    BigInt("53282654108756828443191190634694037855217779295145"),
    BigInt("36123272525000296071075082563815656710885258350721"),
    BigInt("45876576172410976447339110607218265236877223636045"),
    BigInt("17423706905851860660448207621209813287860733969412"),
    BigInt("81142660418086830619328460811191061556940512689692"),
    BigInt("51934325451728388641918047049293215058642563049483"),
    BigInt("62467221648435076201727918039944693004732956340691"),
    BigInt("15732444386908125794514089057706229429197107928209"),
    BigInt("55037687525678773091862540744969844508330393682126"),
    BigInt("18336384825330154686196124348767681297534375946515"),
    BigInt("80386287592878490201521685554828717201219257766954"),
    BigInt("78182833757993103614740356856449095527097864797581"),
    BigInt("16726320100436897842553539920931837441497806860984"),
    BigInt("48403098129077791799088218795327364475675590848030"),
    BigInt("87086987551392711854517078544161852424320693150332"),
    BigInt("59959406895756536782107074926966537676326235447210"),
    BigInt("69793950679652694742597709739166693763042633987085"),
    BigInt("41052684708299085211399427365734116182760315001271"),
    BigInt("65378607361501080857009149939512557028198746004375"),
    BigInt("35829035317434717326932123578154982629742552737307"),
    BigInt("94953759765105305946966067683156574377167401875275"),
    BigInt("88902802571733229619176668713819931811048770190271"),
    BigInt("25267680276078003013678680992525463401061632866526"),
    BigInt("36270218540497705585629946580636237993140746255962"),
    BigInt("24074486908231174977792365466257246923322810917141"),
    BigInt("91430288197103288597806669760892938638285025333403"),
    BigInt("34413065578016127815921815005561868836468420090470"),
    BigInt("23053081172816430487623791969842487255036638784583"),
    BigInt("11487696932154902810424020138335124462181441773470"),
    BigInt("63783299490636259666498587618221225225512486764533"),
    BigInt("67720186971698544312419572409913959008952310058822"),
    BigInt("95548255300263520781532296796249481641953868218774"),
    BigInt("76085327132285723110424803456124867697064507995236"),
    BigInt("37774242535411291684276865538926205024910326572967"),
    BigInt("23701913275725675285653248258265463092207058596522"),
    BigInt("29798860272258331913126375147341994889534765745501"),
    BigInt("18495701454879288984856827726077713721403798879715"),
    BigInt("38298203783031473527721580348144513491373226651381"),
    BigInt("34829543829199918180278916522431027392251122869539"),
    BigInt("40957953066405232632538044100059654939159879593635"),
    BigInt("29746152185502371307642255121183693803580388584903"),
    BigInt("41698116222072977186158236678424689157993532961922"),
    BigInt("62467957194401269043877107275048102390895523597457"),
    BigInt("23189706772547915061505504953922979530901129967519"),
    BigInt("86188088225875314529584099251203829009407770775672"),
    BigInt("11306739708304724483816533873502340845647058077308"),
    BigInt("82959174767140363198008187129011875491310547126581"),
    BigInt("97623331044818386269515456334926366572897563400500"),
    BigInt("42846280183517070527831839425882145521227251250327"),
    BigInt("55121603546981200581762165212827652751691296897789"),
    BigInt("32238195734329339946437501907836945765883352399886"),
    BigInt("75506164965184775180738168837861091527357929701337"),
    BigInt("62177842752192623401942399639168044983993173312731"),
    BigInt("32924185707147349566916674687634660915035914677504"),
    BigInt("99518671430235219628894890102423325116913619626622"),
    BigInt("73267460800591547471830798392868535206946944540724"),
    BigInt("76841822524674417161514036427982273348055556214818"),
    BigInt("97142617910342598647204516893989422179826088076852"),
    BigInt("87783646182799346313767754307809363333018982642090"),
    BigInt("10848802521674670883215120185883543223812876952786"),
    BigInt("71329612474782464538636993009049310363619763878039"),
    BigInt("62184073572399794223406235393808339651327408011116"),
    BigInt("66627891981488087797941876876144230030984490851411"),
    BigInt("60661826293682836764744779239180335110989069790714"),
    BigInt("85786944089552990653640447425576083659976645795096"),
    BigInt("66024396409905389607120198219976047599490197230297"),
    BigInt("64913982680032973156037120041377903785566085089252"),
    BigInt("16730939319872750275468906903707539413042652315011"),
    BigInt("94809377245048795150954100921645863754710598436791"),
    BigInt("78639167021187492431995700641917969777599028300699"),
    BigInt("15368713711936614952811305876380278410754449733078"),
    BigInt("40789923115535562561142322423255033685442488917353"),
    BigInt("44889911501440648020369068063960672322193204149535"),
    BigInt("41503128880339536053299340368006977710650566631954"),
    BigInt("81234880673210146739058568557934581403627822703280"),
    BigInt("82616570773948327592232845941706525094512325230608"),
    BigInt("22918802058777319719839450180888072429661980811197"),
    BigInt("77158542502016545090413245809786882778948721859617"),
    BigInt("72107838435069186155435662884062257473692284509516"),
    BigInt("20849603980134001723930671666823555245252804609722"),
    BigInt("53503534226472524250874054075591789781264330331690"))

  val euler13: Mesure[String] = Mesure {
    listProb13.sum.toString.take(10)
  } named "euler13"

  /**
   *
   * Problème 14
   *
   * Soit la fonction suivante
   *  - si n pair n -> n/2
   *    - si n impair n -> 3n +1
   * Si on applique cette fonction itérativement jusqu'à obtenir 1,
   * pour quel nombre de départ < 1 million la suite est la plus longue ?
   *
   * tips on calcule la longueur de la liste, pas la liste elle méme
   *
   */
  val euler14: Mesure[Int] = Mesure {
    @tailrec
    def genSeq(n: Long, len: Long = 1): Long = n match
      case 1               => len
      case n if n % 2 == 0 => genSeq(n / 2, len + 1)
      case n               => genSeq(3 * n + 1, len + 1)

    (1 until 1000000).view.map(n => (genSeq(n), n)).max._2
  } named "euler14"

  /**
   * Problème 15
   *
   * Trouver le nombre de chemins possibles dans une grille de 20*20 pour relier le coin
   * haut-gauche avec le coin bas-droit
   */
  val euler15: Mesure[Long] = Mesure {
    @tailrec
    def f(row: Seq[Long], c: Int): Long =
      val next = row.scanLeft(0L)(_ + _)
      if c == 0 then next.last else f(next, c - 1)

    def r(n: Int) = f(List.fill(n + 1)(1L), n - 1)

    r(20)
  } named "euler15"

  /**
   * Problème 16
   *
   * Somme des chiffres du nombre 2**1000
   */
  val euler16: Mesure[Int] = Mesure {
    BigInt(2).pow(1000) |> sumOfDigits
  } named "euler16"

  /**
   * Problème 17
   * Nombre de lettres (sans les espaces) pour écrire tous les nombres de 1 à mille en anglais
   */
  val euler17: Mesure[Int] = Mesure {
    val units = Seq(0, 3, 3, 5, 4, 4, 3, 5, 5, 4, 3, 6, 6, 8, 8, 7, 7, 9, 8, 8)
    val tens = Seq(0, 0, 6, 6, 5, 5, 5, 7, 6, 6)
    def name(n: Int): Int =
      if n < 20 then units(n)
      else if n < 100 then tens(n / 10) + (if n % 10 > 0 then units(n % 10) else 0)
      else if n < 1000 then name(n / 100) + 7 + (if n % 100 > 0 then 3 + name(n % 100) else 0)
      else 11

    (1 to 1000).map(name).sum
  } named "euler17"

  def TriangleSumProblems(input: IndexedSeq[String]): Long =
    val reverseTriangleArray = input.map(lines => lines.split(' ').map(_.toLong)).reverse

    inline def tour(n: Int)(predLine: IndexedSeq[Long]): IndexedSeq[Long] =
      val line = reverseTriangleArray(n)
      for
        j <- line.indices
        k = line(j)
      yield k + Math.max(predLine(j), predLine(j + 1))

    @tailrec
    def recTour(n: Int)(predLine: IndexedSeq[Long]): IndexedSeq[Long] =
      if n >= reverseTriangleArray.length then
        predLine
      else
        val next = tour(n)(predLine)
        recTour(n + 1)(next)

    recTour(1)(reverseTriangleArray(0).toIndexedSeq).last
  end TriangleSumProblems
  
  val euler18: Mesure[Long] = Mesure {
    val input = """|75
                   |95 64
                   |17 47 82
                   |18 35 87 10
                   |20 04 82 47 65
                   |19 01 23 75 03 34
                   |88 02 77 73 07 63 67
                   |99 65 04 28 06 16 70 92
                   |41 41 26 56 83 40 80 70 33
                   |41 48 72 33 47 32 37 16 94 29
                   |53 71 44 65 25 43 91 52 97 51 14
                   |70 11 33 28 77 73 17 78 39 68 17 57
                   |91 71 52 38 17 14 91 43 58 50 27 29 48
                   |63 66 04 68 89 53 67 30 73 16 69 87 40 31
                   |04 62 98 27 23 09 70 98 73 93 38 53 60 04 23""".stripMargin.split("\n")

    TriangleSumProblems(input.toIndexedSeq)
  } named "euler18"

  /**
   * Problème 19
   *
   * Nombre de dimanches entre le 01/01/1901 et le 31/12/2000
   */
  val euler19: Mesure[Int] = Mesure {
    def mod(x: Long, y: Long): Long =
      val x1 = x % y
      if y > 0 && x1 < 0 || y < 0 && x1 > 0 then
        x1 + y
      else
        x1

    def dayOfWeeks(year: Int, month: Int, day: Int): Int =
      val m1 = mod(month - 3, 4800l)
      val y = mod(year + m1 / 12, 400)
      val m = m1 % 12
      (y + (y / 4) - (y / 100) + ((13 * m + 2) / 5) + day + 2) % 7 toInt

    val s = for
      y <- 1901 to 2000
      m <- 1 to 12
      if dayOfWeeks(y, m, 1) == 0
    yield 1

    s.sum
  } named "euler19"

  /**
   * Problème 20
   *
   * Somme des chiffres du nombre 100!
   */
  val euler20: Mesure[Int] = Mesure {
    fact(100) |> sumOfDigits
  } named "euler20"

  /**
   * Problème 21
   *
   * Soit d(n) la somme des diviseurs propres de n
   * Si d(a) = b et d(b) = a avec a != b, alors a et b sont dits nombres amicaux
   *
   * Calculer la somme des nombres amicaux inférieurs é 10000
   */
  inline def amicable(m: Long, n: Long): Boolean = m < n && sumOfDiv(n) == m
  val euler21: Mesure[Long] = Mesure {
    (for
      i <- 2L until 10000
      j = sumOfDiv(i)
      if amicable(i, j)
    yield i + j).sum
  } named "euler21"

  /* Euler 22 */
  val euler22: Mesure[BigInt] = Mesure {
    val l = Source.fromResource("euler22.data").mkString.split(',').toIndexedSeq
    def ord(c: Char): Int = c - 'A' + 1
    val listOfScores = l.sorted.zipWithIndex.map { case (s, i) => BigInt(s.map(ord).sum) * (i + 1) }
    listOfScores sum
  } named "euler22"

  /**
   * Problème 23
   *
   * Un nombre parfait est un nombre pour lequel la somme de ses diviseurs est égal à lui-méme.
   * Un nombre est déficient si la somme de ses diviseurs est inférieur à lui-méme
   * Un nombre est abondant si la somme de ses diviseurs est supérieur à lui-méme
   * 24 est le plus petit qui peut s'écrire comme la somme de deux nombres abondants (12 + 12)
   *
   * Trouver la somme de tous les nombres positifs qui ne peuvent pas s'écrire comme la somme de nombres abondants.
   * A partir de 28123, tous les nombres peuvent s'écrire sous cette forme.
   */
  val euler23: Mesure[Long] = Mesure {
    // Soit n, on vérifie s'il peut s'écrire comme la somme de deux nombres abondants
    def isAbundant(n: Long): Boolean = sumOfDiv(n) > n

    lazy val abunds = (12L until 28124).filter(isAbundant)

    def isabds(n: Long): Boolean =
      val abds = for
        p <- abunds.iterator
        q = n - p
        if q > 11
        if isAbundant(q)
      yield (p, q)
      abds.hasNext

    (12L until 28124).filter(!isabds(_)).sum + (1 to 11).sum
  } named "euler23"

  /**
   * Problème 24
   *
   * millionième permutation lexicographique des chiffres de 0 à 9
   */
  val euler24: Mesure[String] = Mesure {
    val r = "0123456789".permutations //permutations déjà triée dans l'ordre lexicographique, par construction
    r.drop(999999).next()
  } named "euler24"

  @main def run_2() : Unit =
    val ListOfProblems : Seq[Mesure[_]] = Seq(
      euler13,
      euler14,
      euler15,
      euler16,
      euler17,
      euler18,
      euler19,
      euler20,
      euler21,
      euler22,
      euler23,
      euler24
    )

    ListOfProblems.foreach( m =>
      Console.println(m.collect(Total))
    )

