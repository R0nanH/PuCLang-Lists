fun main() {
    val parsed = parseFile("C:/Users/Ronan/Documents/TH Koeln/PuC/PuC-SS23/compiler/test.puc")
    val (type, errors) = Typechecker().inferProg(parsed)
    errors.forEach { println(it) }
    val evaled = closureEval(parsed)
    print("$evaled")
    println(": ${type.print()}")
}