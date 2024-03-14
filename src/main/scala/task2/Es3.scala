package task2

object Es3 extends App:

    // A
    val intToStringVal: Int => String = 
        (i: Int) => i match
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
        (string: String) => !predicate(string)
    
    val notEmptyDef = negDef(empty)
    println("should be true: " + notEmptyDef("foo"))
    println("should be false: " + notEmptyDef(""))
    println("should be true: " + (notEmptyDef("foo") && !notEmptyDef("")))
    println()


    // C
    def neg[T](predicate: T => Boolean): T => Boolean = 
        (input: T) => !predicate(input)
        
    val notEmpty = neg(empty)
    println("should be true: " + notEmpty("foo"))
    println("should be false: " + notEmpty(""))
    println("should be true: " + (notEmpty("foo") && !notEmpty("")))
    println()
