package funsets

object week3scratch {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  val t1: IntSet = new NonEmpty(10, Empty, Empty) //> t1  : funsets.IntSet = {.10.}

  val t2: IntSet = t1 incl 4                      //> t2  : funsets.IntSet = {{.4.}10.}

  val t3: IntSet = t2 incl 6;                     //> t3  : funsets.IntSet = {{.4{.6.}}10.}

  val t4 = t3 incl 5;                             //> t4  : funsets.IntSet = {{.4{{.5.}6.}}10.}
  val t5 = t4 incl 1;                             //> t5  : funsets.IntSet = {{{.1.}4{{.5.}6.}}10.}
  val t6 = t5 incl 11;                            //> t6  : funsets.IntSet = {{{.1.}4{{.5.}6.}}10{.11.}}

  println(t6 contains 1);                         //> true
  println(t6 contains 2);                         //> false
  println(t6 contains 3);                         //> false
  println(t6 contains 4);                         //> true

  val t357 = new NonEmpty(5, Empty, Empty) incl 3 incl 7;
                                                  //> t357  : funsets.IntSet = {{.3.}5{.7.}}

  val t12689 = new NonEmpty(6, Empty, Empty) incl 1 incl 2 incl 8 incl 9;
                                                  //> t12689  : funsets.IntSet = {{.1{.2.}}6{.8{.9.}}}

  val u = t357 union t12689;                      //> u  : funsets.IntSet = {{.1{.2{.3{.5.}}}}6{{.7.}8{.9.}}}

}

object Empty extends IntSet {
  def contains(x: Int) = false;
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty);
  def union(other: IntSet): IntSet = other;
  override def toString = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int) =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true;

  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this;

  def union(other: IntSet): IntSet =
    ((left union right) union other) incl elem;

  override def toString = "{" + left + elem + right + "}"
}

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}