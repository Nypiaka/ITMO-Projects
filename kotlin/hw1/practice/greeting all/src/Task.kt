fun greet(name: String): String {
    return "Hello, $name!"
}

fun main(args: Array<String>) {
    args.also { list -> if (list.isEmpty()) println(greet(readlnOrNull() ?: "Anonymous")) }
        .forEach { s -> println(greet(s)) }
}
