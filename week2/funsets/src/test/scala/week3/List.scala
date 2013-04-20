package week3

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]

  def get(n: Int): T = {
    if (isEmpty) throw new IndexOutOfBoundsException
    else if (n == 0) head;
    else tail.get(n - 1);
  }

}



