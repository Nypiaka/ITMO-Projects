import kotlin.coroutines.CoroutineContext
import kotlinx.coroutines.CoroutineScope

class ParallelEvaluator {

    suspend fun run(task: Task, n: Int, context: CoroutineContext) {
        for (i in 0 until n) {
            CoroutineScope(context).run {
                try {
                    task.run(i)
                } catch (e: Exception) {
                    throw TaskEvaluationException(e)
                }
            }
        }
    }
}
