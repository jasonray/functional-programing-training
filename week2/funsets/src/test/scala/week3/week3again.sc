package week3

object week3again {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  val r = new Rational(5, 2);                     //> r  : week3.Rational = 5/2   (2.5)

  val l: List[Int] = null;                        //> l  : week3.List[Int] = null
  val n: List[Int] = new Nil[Int];                //> n  : week3.List[Int] = [nil]
  val c: List[Int] = new Cons[Int](5, new Nil);   //> c  : week3.List[Int] = [5],[nil]

  def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])
                                                  //> singleton: [T](elem: T)week3.Cons[T]

  val s: List[Int] = singleton(5);                //> s  : week3.List[Int] = [5],[nil]

  println(singleton(5).get(0));                   //> 5
  //println(findElementByIndex(singleton(5), 1));

  val abc = new Cons("A", new Cons("B", new Cons("C", new Nil)));
                                                  //> abc  : week3.Cons[String] = [A],[B],[C],[nil]
  println(abc.get(0));                            //> A
  println(abc.get(1));                            //> B
  println(abc.get(2));                            //> C
  println(abc.get(3));                            //> java.lang.IndexOutOfBoundsException
                                                  //| 	at week3.List$class.get(List.scala:9)
                                                  //| 	at week3.Nil.get(Nil.scala:3)
                                                  //| 	at week3.List$class.get(List.scala:11)
                                                  //| 	at week3.Cons.get(Cons.scala:3)
                                                  //| 	at week3.List$class.get(List.scala:11)
                                                  //| 	at week3.Cons.get(Cons.scala:3)
                                                  //| 	at week3.List$class.get(List.scala:11)
                                                  //| 	at week3.Cons.get(Cons.scala:3)
                                                  //| 	at week3.week3again$$anonfun$main$1.apply$mcV$sp(week3.week3again.scala:
                                                  //| 23)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at week3.week3again$.main(week3.week3again.scala:3)
                                                  //| 	at week3.week3again.main(week3.week3again.scala)

}