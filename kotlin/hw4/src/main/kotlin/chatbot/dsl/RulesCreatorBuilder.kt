package chatbot.dsl

import chatbot.api.ChatContext
import chatbot.api.ChatContextsManager
import chatbot.api.Message
import kotlin.reflect.KClass

open class RulesCreatorBuilder<C : ChatContext>(
    private val chatBot: ChatBotBuilder,
    private val contextClass: KClass<C>,
    private val default: C?,
    private val contextsManager: ChatContextsManager?,
) {
    fun onCommand(command: String, block: OnSomethingBuilder<C>.() -> Unit) {
        val rule = OnSomethingBuilder(
            block,
            contextClass,
            contextsManager,
            default,
        ) { message -> message.text.startsWith("/$command") }
        chatBot.addRule(rule.build())
    }

    fun onMessage(block: OnSomethingBuilder<C>.() -> Unit) {
        val rule = OnSomethingBuilder(block, contextClass, contextsManager, default) { true }
        chatBot.addRule(rule.build())
    }

    fun onMessage(
        predicate: ChatBotBuilder.(Message) -> Boolean,
        block: OnSomethingBuilder<C>.() -> Unit,
    ) {
        val rule =
            OnSomethingBuilder(block, contextClass, contextsManager, default) { message -> chatBot.predicate(message) }
        chatBot.addRule(rule.build())
    }

    fun onMessagePrefix(preffix: String, block: OnSomethingBuilder<C>.() -> Unit) {
        val rule =
            OnSomethingBuilder(
                block,
                contextClass,
                contextsManager,
                default,
            ) { message -> message.text.startsWith(preffix) }
        chatBot.addRule(rule.build())
    }

    fun onMessageContains(text: String, block: OnSomethingBuilder<C>.() -> Unit) {
        val rule =
            OnSomethingBuilder(block, contextClass, contextsManager, default) { message -> message.text.contains(text) }
        chatBot.addRule(rule.build())
    }

    fun onMessage(messageTextExactly: String, block: OnSomethingBuilder<C>.() -> Unit) {
        val rule =
            OnSomethingBuilder(
                block,
                contextClass,
                contextsManager,
                default,
            ) { message -> message.text == messageTextExactly }

        chatBot.addRule(rule.build())
    }
}
