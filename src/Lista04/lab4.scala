// Zadanie 1
sealed trait BT[+A]
    case object Empty extends BT[Nothing]
    case class Node[A](value: A, t1: BT[A], t2: BT[A], t3: BT[A]) extends BT[A]

def mapTree3[A, B](f: A => B)(bt: BT[A]): BT[B] =
    bt match
        case Empty => Empty
        case Node(value, t1, t2, t3) => Node(f(value), mapTree3(f)(t1), mapTree3(f)(t2), mapTree3(f)(t3))

val tt = Node(1,
            Node(2,
                 Empty,
                 Empty,
                 Node(3,
                      Empty,
                      Empty,
                      Empty)
                ),
            Node(4,
                 Empty,
                 Node(5, Empty, Empty, Empty),
                 Node(7, Empty, Empty, Empty)
            ),
            Empty)

val sum = (x: Int) => x + x
mapTree3(sum)(tt)

// Zadanie 2

case class File(name: String)
case class Catalog(name: String, catalogs: List[Catalog], files: List[File])
case class Disc(name: String, catalogs: List[Catalog], files: List[File])



