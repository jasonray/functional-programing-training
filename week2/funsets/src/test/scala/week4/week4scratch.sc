package week4

object week4scratch {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  val f = new Function1[Int, Int] {
    def apply(x: Int) = x * x
  }                                               //> f  : Int => Int = <function1>

  println ( concat() );                           //> 
  println ( concat("a") );                        //> a
  println ( concat("a","b") );                    //> ab
  println ( concat("a","b","c") );                //> abc
}


object concat {
	def apply() = ""
	def apply(x:String) = x
	def apply(x:String,y:String) = x + y
	def apply(x:String,y:String,z:String) = x + y + z
}