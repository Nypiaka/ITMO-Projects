import kotlin.time.Duration.Companion.seconds
import kotlinx.coroutines.*

fun CoroutineScope.runApplication(
    runUI: suspend () -> Unit,
    runApi: suspend () -> Unit,
) {
    launch {
        runUI.invoke()
    }
    launch {
        while (true) {
            try {
                runApi()
                break
            } catch (e: Exception) {
                if (e is CancellationException) {
                    e.printStackTrace()
                }
                delay(1.seconds)
            }
        }
    }
}
