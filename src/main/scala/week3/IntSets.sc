object Intsets {

  val t1 = new NonEmpty(3, Empty, Empty)
  val t2 = t1 incl 1 incl 7

  val t3 = new NonEmpty(3, Empty, Empty)
  val t4 = t3 incl 5 incl 4

  val union = t2 union t4

  abstract class IntSet {
    def contains(x: Int): Boolean

    def incl(x: Int): IntSet

    def union(other: IntSet): IntSet
  }

  object Empty extends IntSet {
    def contains(x: Int): Boolean = false

    def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

    override def union(other: IntSet): IntSet = other

    override def toString = "."
  }

  class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
    def contains(x: Int): Boolean =
      if (x < elem) left contains x
      else if (x > elem) right contains x
      else true

    def incl(x: Int): IntSet =
      if (x < elem) new NonEmpty(elem, left incl x, right)
      else if (x > elem) new NonEmpty(elem, left, right incl x)
      else this

    override def union(other: IntSet): IntSet = {
      println("elem: " + elem + " left: " + left + " right: " + right)
      ((left union right) union other) incl elem
    }

    override def toString = "{" + left + elem + right + "}"
  }

}