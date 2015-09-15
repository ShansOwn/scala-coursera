package week2.funsets

object Main extends App {

  import FunSets._

  println(contains(singletonSet(1), 1))
  exists(x => x > 0 && x < 10, x => x == -1)
}
