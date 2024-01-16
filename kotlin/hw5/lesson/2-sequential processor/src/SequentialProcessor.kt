import kotlinx.coroutines.DelicateCoroutinesApi
import kotlinx.coroutines.ExperimentalCoroutinesApi
import kotlinx.coroutines.newSingleThreadContext
import kotlinx.coroutines.withContext

class SequentialProcessor(private val handler: (String) -> String) : TaskProcessor {

    @OptIn(ExperimentalCoroutinesApi::class, DelicateCoroutinesApi::class)
    private val context = newSingleThreadContext("i-hate-kotlin")
    override suspend fun process(argument: String): String = withContext(context) {
        return@withContext handler(argument)
    }
}
