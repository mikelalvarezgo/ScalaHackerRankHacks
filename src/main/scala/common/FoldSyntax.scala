package common

import scala.annotation.tailrec

object FoldSyntax {

  implicit class FoldWhile[T](val items: Iterable[T]) extends AnyVal {

    def foldWhile[A](zero: A)(until: A => Boolean)(op: (A, T) => A): A = {
      @tailrec def loop(acc: A, remaining: Iterable[T]): A =
        if (remaining.isEmpty || !until(acc)) acc else loop(op(acc, remaining.head), remaining.tail)

      loop(zero, items)
    }
  }
}
