sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

val test:Any = List(1,2,3,4,5)
val x = test match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x:Int, Cons(y:Int, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t:List[Int]) => h + sum(t)
  case _ => 101
}