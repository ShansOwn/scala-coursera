import math.Ordering

object Worksheet {
  def removeAt[T](n: Int, xs: List[T]) = (xs take n) ::: (xs drop n + 1)

  def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, _) => ys
        case (_, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (ord.lt(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }
      val (fst, snd) = xs splitAt n
      merge(msort(fst), msort(snd))
    }
  }

  def squareList(xs: List[Int]): List[Int] = xs map (x => x * x)

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 =>
      val (fst, rest) = xs span (y => y == x)
      fst :: pack(rest)
  }

  def encode[T](xs: List[T]): List[(T, Int)] =
    pack(xs) map (ys => (ys.head, ys.size))

  def concat[T](xs: List[T], ys: List[T]): List[T] =
    (xs foldRight ys)(_ :: _)

  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]())(f(_) :: _)

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)((_, acc) => acc + 1)

  removeAt(1, List('a', 'b', 'c', 'd'))
  val nums = List(2, -4, 5, 7, 1)
  val fruits = List("apple", "pineapple", "orange", "banana")
  msort(nums)
  msort(fruits)
  nums filter (x => x > 0)
  nums filterNot (x => x > 0)
  nums partition (x => x > 0)
  nums takeWhile (x => x > 0)
  nums dropWhile (x => x > 0)
  nums span (x => x > 0)

  pack(List('a', 'a', 'a', 'b', 'c', 'c', 'a'))
  encode(List('a', 'a', 'a', 'b', 'c', 'c', 'a'))
}