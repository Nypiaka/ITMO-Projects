fun mapSquares(values: IntArray): IntArray {
    return IntArray(values.size) { i -> values[i] * values[i] }
}
