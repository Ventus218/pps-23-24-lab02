package task4

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.*

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
