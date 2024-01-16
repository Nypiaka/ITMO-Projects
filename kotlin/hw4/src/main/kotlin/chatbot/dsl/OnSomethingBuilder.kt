package chatbot.dsl

import chatbot.api.*
import java.util.function.Predicate
import kotlin.reflect.KClass
import kotlin.reflect.cast

@BotCreator
class OnSomethingBuilder<C : ChatContext>(
    private val action: OnSomethingBuilder<C>.() -> Unit,
    private val contextClass: KClass<C>,
    private val contextsManager: ChatContextsManager?,
    private val startContext: C?,
    private val matches: Predicate<Message>,
) {
    lateinit var context: C
    lateinit var client: Client
    lateinit var message: Message
    fun build() = object : OnSomething<C> {
        override val contextClass = this@OnSomethingBuilder.contextClass
        override fun doAction(message: Message, client: Client) {
            this@OnSomethingBuilder.message = message
            this@OnSomethingBuilder.client = client
            this@OnSomethingBuilder.context =
                contextClass.cast(
                    if (contextsManager != null) {
                        contextsManager.getContext(message.chatId) ?: DefaultContext
                    } else {
                        DefaultContext
                    },
                )
            this@OnSomethingBuilder.action()
        }

        override fun matches(message: Message): Boolean {
            return matches.test(message)
        }

        override fun getCurrentContext(): ChatContext {
            if (startContext != null) {
                context = startContext
            }
            return if (this@OnSomethingBuilder::context.isInitialized) {
                context
            } else {
                DefaultContext
            }
        }
    }

    fun sendMessage(chatId: ChatId, block: MessageInfoBuilder.() -> Unit) {
        if (this::client.isInitialized && this::message.isInitialized) {
            val handler = MessageInfoBuilder(message)
            handler.block()
            if (handler.maySendMessage()) {
                client.sendMessage(
                    chatId,
                    handler.text,
                    handler.getKeyboard(),
                    handler.replyTo,
                )
            }
        }
    }

    fun setContext(context: ChatContext): ChatContext {
        this.contextsManager!!.setContext(message.chatId, context)
        return context
    }
}
