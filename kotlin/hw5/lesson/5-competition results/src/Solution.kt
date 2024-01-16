import kotlin.time.Duration
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.transform

fun Flow<Cutoff>.resultsFlow(): Flow<Results> {
    val map = mutableMapOf<String, Duration>()
    return transform { cutoff ->
        map[cutoff.number] = cutoff.time
        val newMap = map.toMutableMap()
        emit(Results(newMap))
    }
}
