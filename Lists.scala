object Lists {
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    if (sub.size > sup.size)
      false
    else if (sub.size == sup.size) {
      sup.size == 0 ||
        sup.head == sub.head && startsWith(sup.tail, sub.tail)
    } else {
      // sup.size > sub.size
      (sup, sub) match {
        case (_, Nil) => true
        case (Nil, _) => false
        case (h :: t, h1 :: t1) => if (h == h1) startsWith(t, t1) else hasSubsequence(t, h1 :: t1)
      }
    }
  }

  private def startsWith[A](list: List[A], sub: List[A]): Boolean = (list, sub) match {
    case (_, Nil) => true
    case (h :: t, h1 :: t1) if h == h1 => startsWith(t, t1)
    case _ => false
  }
}
