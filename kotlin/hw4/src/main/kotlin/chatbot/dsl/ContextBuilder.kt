package chatbot.dsl

import chatbot.api.*

@BotCreator
class ContextBuilder(val chatBot: ChatBotBuilder, val contextsManager: ChatContextsManager?) :
    RulesCreatorBuilder<DefaultContext>(chatBot, DefaultContext::class, DefaultContext, contextsManager) {

        inline fun <reified C : ChatContext> into(block: RulesCreatorBuilder<C>.() -> Unit) {
            val rulesCreatorBuilder = RulesCreatorBuilder(chatBot, C::class, null, contextsManager)
            rulesCreatorBuilder.block()
        }

        inline infix fun <reified C : ChatContext> C.into(block: RulesCreatorBuilder<C>.() -> Unit) {
            val rulesCreatorBuilder = RulesCreatorBuilder(chatBot, C::class, this, contextsManager)
            rulesCreatorBuilder.block()
        }
    }
