def nComp[A](f: A => A)(n: Int)(x: A): List[A] =
    if n <= 0 then Nil
    else
        def helper(elem: A, i: Int): List[A] =
            if i == 0 then Nil
            else elem :: helper(f(elem), i - 1)

        helper(x, n)

nComp((x: Int) => x * x)(4)(3) == List(3, 9, 81, 6561)
nComp((x: Int) => x / 2)(-1)(8) == List()
nComp((x: Int) => x * x * x)(3)(-2) == List(-2, -8, -512)


def area(a: Double, b: Double)(f: Double => Double)(n: Int): Double =
    val xs = List.range(1, n)
    val interval_len = (b - a) / n
    val areas = xs map (x => (f(a + interval_len * x)).abs * interval_len)
    (areas foldLeft 0.0)((acc, x) => acc + x)


area(0.0, 3.0)(x => x * x)(100) == 8.86545
area(1.0, 2.0)(x => x * x)(1500) == 2.3316667407407423
area(0.0, 4.0)(x => 4 - x)(100000) == 7.999920000000001
area(1.0, 2.0)(x => 4 - x)(2000) == 2.4987500000000002