// def pascalTrapezoidF(num: Int): List[Int] = 
//     def helper(currArr: List[Int], prevArr: List[Int], i: Int): List[Int] =
//         if i > num then arr
//         else currArr
            

//     helper(List(1, 1, 1))

def pascalTrapezoidI(num: Int): Array[Int] =
    var i = 1
    var prevArray = Array[Int](1, 1, 1)
    var currArray: Array[Int] = null

    while i <= num do
        currArray = new Array[Int](2*i + 1)
        currArray(0) = 1
        currArray(1) = 1
        var j = 0
        while j < (prevArray.length - 2) do
            currArray(j+2) = prevArray(j) + prevArray(j+1) + prevArray(j+2)
            j += 1
        currArray(2*i) = 1
        currArray(2*i - 1) = 1
        prevArray = currArray
        i += 1
    currArray
        
pascalTrapezoidI(0)
pascalTrapezoidI(1)
pascalTrapezoidI(2)
pascalTrapezoidI(3)
pascalTrapezoidI(4)