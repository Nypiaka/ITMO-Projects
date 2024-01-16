package chatbot.dsl

import chatbot.api.*
import kotlin.reflect.KClass

interface OnSomething<C : ChatContext> {
    val contextClass: KClass<C>

    fun doAction(message: Message, client: Client)

    fun matches(message: Message): Boolean

    fun getCurrentContext(): ChatContext
}
