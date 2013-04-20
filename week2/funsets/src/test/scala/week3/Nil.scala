package week3

class Nil[T] extends List[T] {
  def isEmpty = true;
  def head: Nothing = throw new NoSuchElementException("nil.head");
  def tail: Nothing = throw new NoSuchElementException("nil.head");
  override def toString() = "[nil]"
}
