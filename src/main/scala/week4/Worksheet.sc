object Expressions {
  def show(e: Expr): String = e match {
    case Number(x) => x.toString
    case Sum(l, r) => show(l) + " + " + show(r)
    case Prod(Sum(x, y), r) => "(" + show(Sum(x, y)) + ")" + " x " + show(r)
    case Prod(l, Sum(x, y)) => show(l) + " x " + "(" + show(Sum(x, y)) + ")"
    case Prod(l, r) => show(l) + " x " + show(r)
    case Var(s) => s
  }

  trait Expr

  case class Number(n: Int) extends Expr

  case class Sum(e1: Expr, e2: Expr) extends Expr

  case class Prod(e1: Expr, e2: Expr) extends Expr

  case class Var(x: String) extends Expr

  show(Sum(Prod(Number(2), Var("x")), Var("y")))
  show(Prod(Sum(Number(2), Var("x")), Var("y")))
  show(Prod(Var("y"), Sum(Number(2), Var("x"))))
}

object Sort {
  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
  }
}