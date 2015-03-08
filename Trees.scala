object Trees {
  sealed trait Tree[+A] {
    def size: Int

    def depth: Int

    def map[B](f: A => B): Tree[B]

    def fold[B](init: A => B)(f: (B, B) => B): B

    def swf: Int

    def dwf: Int
  }

  object Tree {
    def apply[A](one: A): Tree[A] = Leaf(one)

    def apply[A](one: A, another: A): Tree[A] = Branch(one, another)

    def apply[A](one: A, another: Tree[A]): Tree[A] = Branch(Leaf(one), another)

    def apply[A](one: Tree[A], another: A): Tree[A] = Branch(one, Leaf(another))
  }

  case class Leaf[A](value: A) extends Tree[A] {
    override def size: Int = 1

    override def depth: Int = 0

    override def map[B](f: (A) => B): Tree[B] = Leaf(f(value))

    override def fold[B](init: A => B)(f: (B, B) => B): B = init(value)

    override def swf: Int = fold(_ => 1) { case (a, b) => 1 }

    override def dwf: Int = fold(_ => 0) { case (a, b) => 0 }
  }

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
    override def size: Int = left.size + right.size + 1

    override def depth: Int = left.depth.max(right.depth) + 1

    override def map[B](f: (A) => B): Tree[B] = Branch(left.map(f), right.map(f))

    override def fold[B](init: A => B)(f: (B, B) => B): B = f(left.fold(init)(f), right.fold(init)(f))

    override def swf: Int = fold(_ => 1){ case (a, b) => a + b + 1 }

    override def dwf: Int = fold(_ => 0){ case (a, b) => 1 + a.max(b)}
  }

  object Branch {
    def apply[A](one: A, another: A): Branch[A] = Branch(Leaf(one), Leaf(another))
  }


  def maximumForIntTree(intTree: Tree[Int]): Int = {
    intTree match {
      case Leaf(leaf) => leaf
      case Branch(left, right) => maximumForIntTree(left).max(maximumForIntTree(right))
    }
  }

}