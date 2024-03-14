package task2;

object Es5 extends App:

    def compose(f: Int => Int, g: Int => Int): Int => Int =
        x => f(g(x))

    println(compose(_ - 1, _ * 2)(5))

    def genericCompose[FOut, GIn, GOut](f: GOut => FOut, g: GIn => GOut): GIn => FOut =
        x => f(g(x))

    println(genericCompose((fIn: Int) => fIn - 1, (gIn: Int) => gIn * 2)(5))
    
