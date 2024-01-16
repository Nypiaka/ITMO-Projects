package chatbot.dsl

import chatbot.api.ChatBot
import chatbot.api.ChatContext
import chatbot.api.ChatContextsManager
import chatbot.api.Client
import chatbot.api.LogLevel
import chatbot.api.Message
import kotlin.reflect.KClass

class ChatBotBuilder(var client: Client) {
    var logLevel: LogLevel = LogLevel.ERROR
    val rules =
        mutableMapOf<
            KClass<out ChatContext>,
            MutableMap<ChatContext, MutableList<OnSomething<out ChatContext>>>,
            >()
    operator fun ChatBotBuilder.plusAssign(level: LogLevel) {
        logLevel = level
    }

    var contextsManager: ChatContextsManager? = null
    fun <C : ChatContext> addRule(rule: OnSomething<C>) {
        rules.getOrPut(rule.contextClass) {
            mutableMapOf(Pair(rule.getCurrentContext(), mutableListOf()))
        }.getOrPut(rule.getCurrentContext()) { mutableListOf() }.add(rule)
    }

    fun build() =
        object : ChatBot {
            override val logLevel = this@ChatBotBuilder.logLevel

            override fun processMessages(message: Message) {
                val getOrDefaultContext =
                    contextsManager?.getContext(message.chatId) ?: DefaultContext
                try {
                    val oneContextClassRules = rules[getOrDefaultContext::class]!!
                    val noDiffInstances =
                        oneContextClassRules.size > 1 ||
                            !oneContextClassRules.containsKey(DefaultContext)
                    val key =
                        if (noDiffInstances) {
                            getOrDefaultContext
                        } else {
                            DefaultContext
                        }
                    oneContextClassRules[key]!!
                        .first { rule -> rule.matches(message) }
                        .doAction(message, client)
                } catch (e: NullPointerException) {
                    throw IllegalArgumentException(
                        "Rule for message {$message} is not registered in ChatBot",
                    )
                }
            }
        }
}
