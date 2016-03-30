package prasalov

/**
 * Created by kirillprasalov on 23.03.16.
 */
object App {

  def main(args: Array[String]): Unit = {
    println("ex3_1: " + ex3_1)

    println()
    println("ex3_2:")
    println(List.tail(List(1, 2, 3, 4, 5)).get)
    println(List.tail(List(1)).get)
    try {
      println(List.tail(Nil).get)
      List.tail(Nil).get
    } catch {
      case e: Exception => println("Nil has no tail!")
    }

    println()
    println("ex3_3:")
    println(List.setHead(239, List(1, 2, 3, 4, 5)).get)
    println(List.setHead(239, List(1)).get)
    try {
      println(List.setHead(239, Nil).get)
      List.tail(Nil).get
    } catch {
      case e: Exception => println("Nil has no head/tail!")
    }

    println()
    println("ex3_4:")
    println(List.drop(3)(List(1, 2, 3, 4, 5)).get)
    println(List.drop(5)(List(1, 2, 3, 4, 5)).get)
    try {
      println(List.drop(239)(List(1, 2, 3, 4, 5)).get)
      List.tail(Nil).get
    } catch {
      case e: Exception => println("Can't drop more than the list has!")
    }
    try {
      println(List.drop(239)(Nil).get)
      List.tail(Nil).get
    } catch {
      case e: Exception => println("Can't drop from Nil!")
    }

    println()
    println("ex3_5:")
    println(List.dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x < 3))
    println(List.dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x < 239))
    println(List.dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x < 0))

    println()
    println("ex3_6:")
    println(List.init(List(1, 2, 3, 4, 5)))
    println(List.init(List(1)))
    try {
      println(List.init(Nil).get)
    } catch {
      case e: Exception => println("Can't remove elements from Nil!")
    }
  }

  // should be 3...
  def ex3_1 = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + Utils.sum(t)
    case _ => 101
  }

}

object Utils {
  def sum(as: List[Int]): Int = as match {
    case Nil => 0
    case Cons(h, t) => h + sum(t)
  }
}