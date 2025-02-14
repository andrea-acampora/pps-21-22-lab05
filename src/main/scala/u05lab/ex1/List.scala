package u05lab.ex1

import u05lab.ex1.List
import u05lab.ex1.List.Nil


import scala.annotation.tailrec

enum List[A]:
  case ::(h: A, t: List[A])
  case Nil()
  def ::(h: A): List[A] = List.::(h, this)

  def head: Option[A] = this match
    case h :: t => Some(h)
    case _ => None

  def tail: Option[List[A]] = this match
    case h :: t => Some(t)
    case _ => None

  def append(list: List[A]): List[A] = this match
    case h :: t => h :: t.append(list)
    case _ => list

  def foreach(consumer: A => Unit): Unit = this match
    case h :: t => consumer(h); t.foreach(consumer)
    case _ =>

  def get(pos: Int): Option[A] = this match
    case h :: t if pos == 0 => Some(h)
    case h :: t if pos > 0 => t.get(pos - 1)
    case _ => None

  def filter(predicate: A => Boolean): List[A] = this match
    case h :: t if predicate(h) => h :: t.filter(predicate)
    case _ :: t => t.filter(predicate)
    case _ => Nil()

  def map[B](fun: A => B): List[B] = this match
    case h :: t => fun(h) :: t.map(fun)
    case _ => Nil()

  def flatMap[B](f: A => List[B]): List[B] =
    foldRight[List[B]](Nil())(f(_) append _)

  def foldLeft[B](z: B)(op: (B, A) => B): B = this match
    case h :: t => t.foldLeft(op(z, h))(op)
    case Nil() => z

  def foldRight[B](z: B)(f: (A, B) => B): B = this match
    case h :: t => f(h, t.foldRight(z)(f))
    case _ => z

  def length: Int = foldLeft(0)((l, _) => l + 1)

  def isEmpty: Boolean = this match
    case Nil() => true
    case _ => false

  def reverse(): List[A] = foldLeft[List[A]](Nil())((l, e) => e :: l)

  /** EXERCISES */

  def zipRight: List[(A, Int)] = this.map(elem => (elem, span(_ != elem)._1.length))

  def zipRightWithFoldLeft: List[(A, Int)] =
    foldLeft[List[(A, Int)]](Nil())((acc, elem) =>
      if acc.isEmpty then List((elem, 0)) else acc.append(List((elem, acc.get(acc.length - 1).get._2 + 1))))

  def zipRightWithRecursion: List[(A, Int)] =
    @tailrec
    def _zipRight(l: List[A], index: Int, result: List[(A, Int)]): List[(A, Int)] = l match
      case h :: t => _zipRight(t, index + 1, result.append((h, index) :: Nil()))
      case _ => result
    _zipRight(this, 0, Nil())

  def partition(predicate: A => Boolean): (List[A], List[A]) = (filter(predicate), filter(!predicate(_)))

  def partitionWithFoldLeft(predicate: A => Boolean): (List[A], List[A]) =
    foldLeft(Nil(), Nil())((acc, elem) =>
      if predicate(elem) then (acc._1.append(List(elem)), acc._2) else (acc._1, acc._2.append(List(elem))))

  def partitionWithRecursion(predicate: A => Boolean): (List[A], List[A]) =
    @tailrec
    def _partition(myList: List[A], trueList: List[A], falseList: List[A]): (List[A], List[A]) = myList match
      case h :: t if predicate(h) => _partition(t, trueList.append(h :: Nil()), falseList)
      case h :: t => _partition(t,trueList,falseList.append(h :: Nil()))
      case _ => (trueList, falseList)
    _partition(this, Nil(), Nil())

  def span(predicate: A => Boolean): (List[A], List[A]) =
    this.foldLeft((List.Nil[A](), List.Nil[A]()))((acc, elem) =>
      if predicate(elem) && acc._2.isEmpty
        then (acc._1.append(elem :: Nil()), acc._2)
        else (acc._1, acc._2.append(elem :: Nil())))

  def spanWithRecursion(predicate: A => Boolean): (List[A], List[A]) =
    @tailrec
    def _span(myList: List[A], trueSplitList: List[A], falseSplitList: List[A], predicate: A => Boolean): (List[A], List[A]) = myList match
      case h :: t if predicate (h) => _span(t, trueSplitList.append(h :: Nil()), falseSplitList, predicate)
      case h :: t => _span(t, trueSplitList, falseSplitList.append(h :: Nil()), _ => false)
      case _ => (trueSplitList, falseSplitList)
    _span(this, Nil(), Nil(), predicate)

  /** @throws UnsupportedOperationException if the list is empty */
  def reduce(op: (A, A) => A): A = this match
    case h :: t => t.foldLeft(h)(op)
    case _ => throw UnsupportedOperationException()

  def takeRight(n: Int): List[A] = span(_ != get(length- n).get)._2

  def takeRightWithRecursion(n: Int): List[A] =
    @tailrec
    def _takeRight(list: List[A], n: Int): List[A] = list match
      case h :: t if n > 0 => _takeRight(t, n - 1)
      case _ => list
    _takeRight(this, length - n)

  def collect[B](partialFunction: PartialFunction[A, B]): List[B] = filter(x => partialFunction.isDefinedAt(x)).map(partialFunction.apply)

// Factories
object List:

  def apply[A](elems: A*): List[A] =
    var list: List[A] = Nil()
    for e <- elems.reverse do list = e :: list
    list

  def of[A](elem: A, n: Int): List[A] =
    if n == 0 then Nil() else elem :: of(elem, n - 1)
