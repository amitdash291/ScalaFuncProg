object MyModule {
  def absolute(num: Int): Int = {
    if (num < 0) -num
    else num
  }

  def fibonacci(num: Int): Int = {
    def go(count: Int, acc1: Int, acc2: Int): Int = {
      //println(acc1)
      if (num <= 0) 0
      else if (count == num) acc1
      else go(count + 1, acc2, acc1 + acc2)
    }
    go(1, 0, 1)
  }

  def printFormattedResult(name: String, num: Int, func: Int => Int) {
    val msg = "%s(%d) = %d"
    val formattedMsg = msg.format(name, num, func(num))
    println(formattedMsg)
  }

  def findInArrayAndGetIndex[T](arr: Array[T], p: T => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if (n == arr.length) -1
      else if (p(arr(n))) n + 1
      else loop(n + 1)
    }

    loop(0)
  }

  def isArraySorted[T](array: Array[T], comparison: (T, T) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(i: Int): Boolean = {
      if (i >= array.length - 1) true
      else if (!comparison(array(i), array(i + 1))) false
      else loop(i + 1)
    }

    loop(0)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a,b)
  }

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    f compose g
    //g andThen f
    //a => f(g(a))
  }

  def matchAny(obj: Any) = {
    val matchResult = obj match {
      case i: Int => (i + 10).toString()
      case b: Book => b.name match {
        case "test" => "Valid book"
        case _ => "Invalid book"
      }
    }

    println(matchResult)
  }

  def main(args: Array[String]) {
    val array = Array(1, 2, 4, 7, 21)
    print("Array: ");
    array foreach (p => print(p + " "));
    println();
    println("Array sorted in increasing order: " + isArraySorted(array, (a: Int, b: Int) => a < b))
    println("Index of 2 in array: " + findInArrayAndGetIndex(array, (i: Int) => i == 2))
    //matchAny(new Book("test"))
    //printFormattedResult("absolute", 5, absolute)
    //printFormattedResult("fibonacci", 5, fibonacci)
  }
}

class Book(n: String) {
  var name: String = n
}