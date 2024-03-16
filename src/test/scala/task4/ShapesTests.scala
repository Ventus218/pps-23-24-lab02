package task4

import org.junit.Test
import org.junit.Assert.*

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
