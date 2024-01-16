package chatbot.dsl

import chatbot.api.ChatBot
import chatbot.api.Client

fun chatBot(client: Client, block: ChatBotWrapper.() -> Unit): ChatBot {
    val bot = ChatBotBuilder(client)
    val wrap = ChatBotWrapper(bot)
    wrap.block()
    return bot.build()
}

@DslMarker
annotation class BotCreator
