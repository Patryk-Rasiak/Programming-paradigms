import scala.annotation.tailrec

// Zadanie 1

def find[A](xs: List[A])(x: A): Boolean =
    xs match 
       case a::b if a == x => true
       case Nil => false
       case a::b => find(b)(x)
    


val find123 = find(List(1, 2, 3))
val findxyz = find(List('x', 'y', 'z'))
val findNothing = find(List())

find123(4) == false
find123(3) == true
findxyz('y') == true
findxyz('a') == false
findNothing(3) == false


// Zadanie 2a

def split3Rec[A](xs: List[A]): (List[A], List[A], List[A]) =
    xs match {
        case Nil => (Nil, Nil, Nil)
        case List(x) => (List(x), List(), List())
        case List(x, y) => (List(x), List(y), List()) 
        case h1 :: h2 :: h3 :: t => val (first, second, third) = split3Rec(t) 
        (h1 :: first, h2 :: second, h3 :: third)
    }

split3Rec(List(1, 2, 3, 4, 5, 6))
split3Rec(List())
split3Rec((List('a', 'b', 'c', 'd')))


// Zadanie 2b

def split3Tail[A](xs: List[A]): (List[A], List[A], List[A]) = {
  @tailrec
  def split3TailIter[A](xs1: List[A], xs2: List[A], xs3: List[A], xs: List[A], acc: Int): (List[A], List[A], List[A]) =
    
    xs match {
     case Nil => (xs1, xs2, xs3)
     case h::t if (acc % 3 == 0) => split3TailIter(h::xs1, xs2, xs3, t, acc+1)
     case h::t if (acc % 3 == 1) => split3TailIter(xs1, h::xs2, xs3, t, acc+1)
     case h::t if (acc % 3 == 2) => split3TailIter(xs1, xs2, h::xs3, t, acc+1)
  }

  split3TailIter(Nil, Nil, Nil, xs, 0)
}


split3Tail(List(1, 2, 3, 4, 5, 6))
split3Tail(List())
split3Tail((List('a', 'b', 'c', 'd')))

