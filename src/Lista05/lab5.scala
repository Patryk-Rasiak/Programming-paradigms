
def skipTakeL[A](xs: LazyList[A]): LazyList[A] =
    def helper(xss: LazyList[A], i: Int, j: Int): LazyList[A] =
        (xss, i) match
            case (LazyList(), _) => LazyList()
            case (h #:: t, 0) => h #:: helper(t, j + 1, j + 1)
            case (h #:: t, _) => helper(t, i - 1, j)

    helper(xs, 0, 0)

skipTakeL(LazyList.from(1)).take(15).toList