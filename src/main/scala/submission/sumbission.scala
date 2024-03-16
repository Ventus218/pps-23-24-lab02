package submission

// Task 1
object Hello extends App:
    println("Hello, Scala!")


// Task 2
object Es3 extends App:
    // A
    val intToStringVal: Int => String = 
        i => i match
            case i if i >= 0 => "positive"
            case _ => "negative"
    
    println("should be positive: " + intToStringVal(2))
    println("should be positive (zero): " + intToStringVal(0))
    println("should be negative: " + intToStringVal(-2))
    println()
    
    def intToStringDef(i: Int): String = i match
        case i if i >= 0 => "positive"
        case _ => "negative"
        
    println("should be positive: " + intToStringDef(2))
    println("should be positive (zero): " + intToStringDef(0))
    println("should be negative: " + intToStringDef(-2))
    println()


    // B
    val negVal: (String => Boolean) => String => Boolean =
        (predicate: String => Boolean) => 
            (string: String) => !predicate(string)
    

    val empty: String => Boolean = _ == ""
    val notEmptyVal = negVal(empty)
    println("should be true: " + notEmptyVal("foo"))
    println("should be false: " + notEmptyVal(""))
    println("should be true: " + (notEmptyVal("foo") && !notEmptyVal("")))
    println()

    def negDef(predicate: String => Boolean): String => Boolean = 
        !predicate(_)
    
    val notEmptyDef = negDef(empty)
    println("should be true: " + notEmptyDef("foo"))
    println("should be false: " + notEmptyDef(""))
    println("should be true: " + (notEmptyDef("foo") && !notEmptyDef("")))
    println()


    // C
    def neg[T](predicate: T => Boolean): T => Boolean = 
        !predicate(_)
        
    val notEmpty = neg(empty)
    println("should be true: " + notEmpty("foo"))
    println("should be false: " + notEmpty(""))
    println("should be true: " + (notEmpty("foo") && !notEmpty("")))
    println()


object Es4 extends App:
    def testFormula(x: Int, y: Int, z: Int, actual: Boolean) =
        actual match
            case actual if actual == (x<=y && y==z) =>
                println(s"X=$x  Y=$y  Z=$z  was correctly $actual")
            case _ => 
                println(s"X=$x  Y=$y  Z=$z  should be ${(x<=y && y==z)}, but was $actual")

    val nonCurriedVal = 
        (x: Int, y: Int, z: Int) => x <= y && y == z
    
    testFormula(1, 2, 3, nonCurriedVal(1, 2, 3))
    testFormula(1, 2, 2, nonCurriedVal(1, 2, 2))
    testFormula(2, 2, 2, nonCurriedVal(1, 2, 2))
    println()

    val curriedVal: Int => Int => Int => Boolean = 
        x =>
            y =>
                z => 
                    x <= y && y == z
    
    testFormula(1, 2, 3, curriedVal(1)(2)(3))
    testFormula(1, 2, 2, curriedVal(1)(2)(2))
    testFormula(2, 2, 2, curriedVal(1)(2)(2))
    println()
    
    def nonCurriedDef(x: Int, y: Int, z: Int): Boolean =
        x<=y && y==z
        
    testFormula(1, 2, 3, nonCurriedDef(1, 2, 3))
    testFormula(1, 2, 2, nonCurriedDef(1, 2, 2))
    testFormula(2, 2, 2, nonCurriedDef(1, 2, 2))
    println()
    
    def curriedDef(x: Int)(y: Int)(z: Int): Boolean =
        x<=y && y==z
        
    testFormula(1, 2, 3, curriedDef(1)(2)(3))
    testFormula(1, 2, 2, curriedDef(1)(2)(2))
    testFormula(2, 2, 2, curriedDef(1)(2)(2))
    println()


object Es5 extends App:

    def compose(f: Int => Int, g: Int => Int): Int => Int =
        x => f(g(x))

    println(compose(_ - 1, _ * 2)(5))

    def genericCompose[FOut, GIn, GOut](f: GOut => FOut, g: GIn => GOut): GIn => FOut =
        x => f(g(x))

    println(genericCompose((fIn: Int) => fIn - 1, (gIn: Int) => gIn * 2)(5))


// Task 3
object Es6 extends App:

    @annotation.tailrec
    def gcd(a: Int, b: Int): Int = (a, b) match
        case (a, b) if a == b => a
        case (a, b) if a == 0 => b
        case (a, b) if b == 0 => a
        case (a, b) if a > b => gcd(b, a % b)
        case _ => gcd(a, b % a)

    println(gcd(12, 8) == 4)
    println(gcd(14, 7) == 7)


// Task 4
object Es7 extends App:
    enum Shape:
        case Rectangle(w: Double, h: Double)
        case Square(s: Double)
        case Circle(r: Double)
    
    object Shape:
        def perimeter(s: Shape): Double = s match
            case Shape.Rectangle(w, h) => (w+h)*2
            case Shape.Square(s) => s*4
            case Shape.Circle(r) => 2*Math.PI*r
        
        def scale(s: Shape, factor: Double): Shape = s match
            case Shape.Rectangle(w, h) => Shape.Rectangle(w * factor, h * factor)
            case Shape.Square(s) => Shape.Square(s * factor)
            case Shape.Circle(r) => Shape.Circle(r * factor)

class ShapesTests:

    import task4.Es7.Shape
    import task4.Es7.Shape.*

    val rectWidth = 2
    val rectHeight = 4
    val rect = Rectangle(rectWidth, rectHeight)

    val squareSide = 2
    val square = Square(squareSide)

    val circleRadius = 2
    val circle = Circle(circleRadius)

    val scaleFactor = 2

    val delta = 0d

    @Test def testPerimeterRectangle(): Unit =
        assertEquals((rectWidth + rectHeight) * 2d, perimeter(rect), delta)

    @Test def testPerimeterSquare(): Unit =
        assertEquals(squareSide * 4d, perimeter(square), delta)
        
    @Test def testPerimeterCircle(): Unit =
        assertEquals(circleRadius * 2 * Math.PI, perimeter(circle), delta)
        
    @Test def testScaleRectangle(): Unit =
        assertEquals(Rectangle(rectWidth * scaleFactor, rectHeight * scaleFactor), scale(rect, scaleFactor))

    @Test def testScaleSquare(): Unit =
        assertEquals(Square(squareSide * scaleFactor), scale(square, scaleFactor))
        
    @Test def testScaleCircle(): Unit =
        assertEquals(Circle(circleRadius * scaleFactor), scale(circle, scaleFactor))


// Task 5
object Optionals:
  /**
   * Optional is a type that represents a value that may or may not be present.
   * Similar to Optional in Java but using the ADT concept.
   * Therefore, an Optional is a sum type with two cases: Maybe and Empty.
   * Maybe contains the value, and Empty represents the absence of a value.
   *
   * @tparam A
   */
  enum Optional[A]:
    case Maybe(value: A)
    case Empty()

  object Optional:
    /**
     * isEmpty returns true if the optional is Empty, false otherwise.
     * Example:
     *
     * isEmpty(Empty()) == true
     * isEmpty(Maybe(1)) == false
     *
     * @param optional the optional to check
     * @tparam A the type of the optional
     * @return true if the optional is Empty, false otherwise
     */
    def isEmpty[A](optional: Optional[A]): Boolean = optional match
      case Empty() => true
      case _ => false

    /**
     *
     * getOrElse returns the value of the optional if it is Maybe, otherwise it returns the default value.
     * Example:
     * orElse(Maybe(1), 0) == 1
     * orElse(Empty(), 0) == 0
     *
     * @param optional the optional to get the value from
     * @param default the default value to return if the optional is Empty
     * @tparam A the type of the optional
     * @tparam B the type of the default value
     * @return the value of the optional if it is Maybe, otherwise the default value
     */
    def orElse[A, B >: A](optional: Optional[A], default: B): B = optional match
      case Maybe(value) => value
      case Empty() => default

    /**
     * map applies the function f to the value of the optional if it is Maybe, otherwise it returns Empty.
     * Example:
     *
     * map(Maybe(1), (x: Int) => x + 1) == Maybe(2)
     * map(Empty(), (x: Int) => x + 1) == Empty()
     *
     *
     * @param optional the optional to apply the function to
     * @param f the function to apply to the value of the optional
     * @tparam A the type of the optional
     * @tparam B the type of the result of the function
     * @return the result of applying the function to the value of the optional if it is Maybe, otherwise Empty
     */
    def map[A, B](optional: Optional[A], f: A => B): Optional[B] = optional match
      case Maybe(value) => Maybe(f(value))
      case Empty() => Empty()
    
    def filter[A](optional: Optional[A], f: A => Boolean): Optional[A] = optional match
      case Maybe(value) if !f(value) => Maybe(value)
      case _ => Empty()

class OptionalTest:
    @Test def emptyOptionalShouldBeEmpty(): Unit = {
        val empty = Optional.Empty()
        assertTrue(Optional.isEmpty(empty))
    }

    @Test def nonEmptyOptionalShouldNotBeEmpty(): Unit = {
        val nonEmpty = Optional.Maybe(0)
        assertFalse(Optional.isEmpty(nonEmpty))
    }

    @Test def orElseShouldReturnDefaultWhenEmpty(): Unit = {
        val nonEmpty = Optional.Maybe(0)
        assertEquals(0, Optional.orElse(nonEmpty, 1))
    }

    @Test def orElseShouldReturnValueWhenNonEmpty(): Unit = {
        val empty = Optional.Empty()
        assertEquals(1, Optional.orElse(empty, 1))
    }

    /** Task 5 -- Look the behaviour of map operator */
    @Test def mapShouldReturnEmptyWhenEmpty(): Unit = {
        val empty: Optional[Int] = Optional.Empty()
        val result = Optional.map(empty, _ + 1)
        assertTrue(Optional.isEmpty(result))
    }

    @Test def mapShouldReturnTransformedValueWhenNonEmpty(): Unit = {
        val nonEmpty = Optional.Maybe(0)
        val result = Optional.map(nonEmpty, _ + 1)
        assertEquals(1, Optional.orElse(result, 1))
    }

    @Test def filterFiltersOutValue(): Unit = {
        assertEquals(Optional.Empty(), Optional.filter(Optional.Maybe(5), _ > 1))
    }

    @Test def filterDoesNotFilterOutValue(): Unit = {
        val optional = Optional.Maybe(1)
        assertEquals(optional, Optional.filter(optional, _ > 1))
    }

    @Test def filterDoesNothingOnEmpty(): Unit = {
        val empty = Optional.Empty[Int]()
        assertEquals(empty, Optional.filter(empty, _ > 1))
}
