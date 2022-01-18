def mirror4[A](xss: (A, A, A, A)): (A, A, A, A) =
  (xss._2, xss._1, xss._4, xss._3)

mirror4((1, 2, 3, 4))
mirror4(2.5, 1, "Ala", 'a')
mirror4((2.5, 1, "Ala", 'a'))

def substitute[A](xss: List[A], a: A, b: A): List[A] =
  if xss == Nil then Nil
  else if xss.head == a then b :: substitute(xss.tail, a, b)
  else xss.head :: substitute(xss.tail, a, b)

substitute(List(1, 2, 3, 4), 3, 8)


def remove[A](xss: List[A], pos: Int): List[A] =
  if pos == 0 then xss.tail
  else xss.head :: remove(xss.tail, pos - 1)

remove(List(1, 2, 3, 4, 5), 3)