class IntMatrix(rows: Int, columns: Int) {

    private val body: IntArray
    val rows: Int
    val columns: Int

    init {
        require(rows > 0) { "Rows should be more than 0" }
        require(columns > 0) { "Columns should be more than 0" }
        this.rows = rows
        this.columns = columns
        this.body = IntArray(rows * columns)
    }

    operator fun get(x: Int, y: Int): Int {
        checkIndexes(x, y)
        return body[getPosition(x, y)]
    }

    operator fun set(x: Int, y: Int, value: Int) {
        checkIndexes(x, y)
        body[getPosition(x, y)] = value
    }

    private fun checkIndexes(x: Int, y: Int) {
        require(x in 0..<rows) { "x should be more than 0 or 0 and less than $rows" }
        require(y in 0..<columns) { "y should be more than 0 or 0 and less than $columns" }
    }

    private fun getPosition(x: Int, y: Int) = x * columns + y
}
