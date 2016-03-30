package prasalov

import scala.annotation.tailrec

/**
 * Created by kirillprasalov on 23.03.16.
 */
object List {

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def optionalCons[A](a: A, as: Option[List[A]]): Option[List[A]] = as.map(Cons(a, _))

  def oldTail[A](as: List[A]): Option[List[A]] = as match {
    case Nil => None
    case Cons(h, t) => Some(t)
  }

  def drop[A](n: Int): List[A] => Option[List[A]] = {
    @tailrec def _drop[A](n1: Int, as: List[A]): Option[List[A]] = (n1, as) match {
      case (0, x) => Some(x)
      case (_, Nil) => None
      case (n2, Cons(h, t)) => _drop(n2 - 1, t)
    }
    _drop(n, _)
  }

  @tailrec def dropWhile[A](l: List[A], p: A => Boolean): List[A] = l match {
    case Nil => l
    case Cons(h, t) => if (p(h)) dropWhile(t, p) else l
  }

  def tail[A](as: List[A]) = drop(1)(as)

  def setHead[A](h: A, as: List[A]): Option[List[A]] = optionalCons(h, tail(as))
  /*as match {
    case Nil => None
    case Cons(h1, t) => Some(Cons(h, t))
  }*/

  // WARN: not @tailrec!
  def append[A](as: List[A], bs: List[A]): List[A] = as match {
    case Nil => bs
    case Cons(h, t) => Cons(h, append(t, bs))
  }

  def init[A](l: List[A]): Option[List[A]] = l match {
    case Nil => None
    case Cons(h, t) => optionalCons(h, init(t))
  }
  /*{
    @tailrec def _init[A](res: List[A], as: List[A]): Option[List[A]] = (res, as) match {
      case (r, Cons(h, Nil)) => Some(r)
      case (_, Nil) => None
      case (r, Cons(h, t)) => Cons(r, h)
    }
  }*/
/*
    l match {
    case Nil => None
    case Cons(h, Nil) =>
  }*/
}

sealed trait List[+A] {
  final override def toString = {
    val s = this match {
      case Nil => ""
      case Cons(h, t) => _toString(h.toString, t)
    }
    "List(" + s + ")"
  }

  @tailrec private def _toString[A](s: String, l: List[A]): String = l match {
    case Nil => s
    case Cons(h, t) => _toString(s + ", " + h, t)
  }
}
case class Cons[+A](head: A, tail: List[A]) extends List[A]
case object Nil extends List[Nothing]