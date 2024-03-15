package task3

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
