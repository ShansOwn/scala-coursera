object Worksheet {

  def isPrime(n: Int): Boolean = (2 until n) forall (n % _ != 0)

  def scalarProduct(xs: List[Double], ys: List[Double]): Double =
    (for ((x, y) <- xs zip ys) yield x * y).sum

  def forTest: List[List[Int]] =
    for {
      xs <- List(List(1, 2, 3), List(4, 5, 6))
      n <- 0 until 6
      if !(xs contains n)
    } yield n :: xs

  def queens(n: Int): Set[List[Int]] = {
    def placeQueens(k: Int): Set[List[Int]] =
      if (k == 0) Set(List())
      else
        for {
          queens <- placeQueens(k - 1)
          col <- 0 until n
          if isSafe(col, queens)
        } yield col :: queens
    placeQueens(n)
  }

  def isSafe(col: Int, queens: List[Int]): Boolean = {
    val row = queens.length
    val queensWithRow = (row - 1 to 0 by -1) zip queens
    queensWithRow forall {
      case (r, c) => col != c && math.abs(col - c) != row - r
    }
  }

  def show(queens: List[Int]) = {
    val lines =
      for (col <- queens.reverse)
        yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
    "\n" + (lines mkString "\n")
  }

  isPrime(17)

  scalarProduct(List(1, 2, 3), List(4))

  forTest

  val res = queens(8) take 3
  (res map show) mkString "\n"
}