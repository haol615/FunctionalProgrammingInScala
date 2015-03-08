object Trees {
  sealed trait Tree[+A] {
    def size: Int

    def depth: Int

    def map[B](f: A => B): Tree[B]
  }

  case class Leaf[A](value: A) extends Tree[A] {
    override def size: Int = 1

    override def depth: Int = 1

    override def map[B](f: (A) => B): Tree[B] = Leaf(f(value))
  }

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
    override def size: Int = left.size + right.size + 1

    override def depth: Int = left.depth.max(right.depth) + 1

    override def map[B](f: (A) => B): Tree[B] = Branch(left.map(f), right.map(f))
  }


  def maximumForIntTree(intTree: Tree[Int]): Int = {
    intTree match {
      case Leaf(leaf) => leaf
      case Branch(left, right) => maximumForIntTree(left).max(maximumForIntTree(right))
    }
  }

}