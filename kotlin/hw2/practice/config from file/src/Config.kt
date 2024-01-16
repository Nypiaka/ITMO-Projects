import java.io.InputStreamReader
import kotlin.properties.ReadOnlyProperty
import kotlin.reflect.KProperty

class Config(config: String) {
    private val map: HashMap<String, String> = HashMap()

    init {
        val inputStream = getResource(config)
        requireNotNull(inputStream)
        InputStreamReader(inputStream).forEachLine { line ->
            val parsed = parse(line)
            require(parsed.size == 2)
            map[parsed[0]] = parsed[1]
        }
    }

    private fun parse(line: String) =
        line.split("=").map { s -> s.trim() }

    operator fun provideDelegate(thisRef: Any?, property: KProperty<*>): ReadOnlyProperty<Any?, String> {
        checkProperty(property.name)
        return ResourceDelegate(map[property.name])
    }

    private fun checkProperty(name: String) {
        require(map.containsKey(name)) { "$name not found" }
    }
}

class ResourceDelegate(private val value: String?) : ReadOnlyProperty<Any?, String> {
    override fun getValue(thisRef: Any?, property: KProperty<*>) = value!!
}
