package task2

object Es4 extends App:
    def testFormula(x: Int, y: Int, z: Int, actual: Boolean) = actual match
        case actual if actual == (x<=y && y==z) =>
            println(s"X=$x  Y=$y  Z=$z  was correctly $actual")
        case _ => 
            println(s"X=$x  Y=$y  Z=$z  should be ${(x<=y && y==z)}, but was $actual")

    val nonCurriedVal: (Int, Int, Int) => Boolean = (x, y, z) => x <= y && y == z
    
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

