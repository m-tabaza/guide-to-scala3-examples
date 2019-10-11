
trait A  {
  val a: String
}

trait B {
  val b: Int
}

case class C(a: String, b: Int) 
  extends A with B

def f(c: A & B) = c.a + " & " + c.b

val a = new A { val a = "Some String" }
val b = new B { val b = 42 }

type E = Either[A, B]
val l: E = Left(a)
val r: E = Right(b)

def g(e: E): String = e match {
  case Left(a) => s"String value a: ${a.a}"
  case Right(b) => s"Int value b: ${b.b}"
}

type U = A | B

def h(v: A | Double | B | Boolean): String = v match {
  case a: A => s"String value a: ${a.a}"
  case d: Double => s"Double value d: $d"
  case b: B => s"Int value b: ${b.b}"
  case bool: Boolean => s"Boolean value bool: $bool"
}
