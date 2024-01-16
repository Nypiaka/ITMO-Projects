package chatbot.dsl

import chatbot.api.ChatContextsManager
import chatbot.api.LogLevel

@BotCreator
class ChatBotWrapper(private val chatBot: ChatBotBuilder) {

    private var contextsManager: ChatContextsManager? = null
    fun use(level: LogLevel) {
        chatBot.logLevel = level
    }

    fun use(contextsManager: ChatContextsManager) {
        chatBot.contextsManager = contextsManager
        this.contextsManager = contextsManager
    }

    fun behaviour(block: ContextBuilder.() -> Unit) {
        val handler = ContextBuilder(chatBot, contextsManager)
        handler.block()
    }

    operator fun LogLevel.unaryPlus() {
        chatBot.logLevel = this
    }
}
