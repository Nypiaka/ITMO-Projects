package chatbot.dsl

import chatbot.api.Keyboard
import chatbot.api.Message
import chatbot.api.MessageId

@BotCreator
class MessageInfoBuilder(val message: Message) {
    var text: String = ""
    var replyTo: MessageId? = null
    private var keyboard: Keyboard? = null
    private var presentsAndEmpty = true
    fun removeKeyboard() {
        presentsAndEmpty = false
        keyboard = Keyboard.Remove
    }

    fun maySendMessage() = (text.isNotEmpty()) || !presentsAndEmpty

    fun withKeyboard(block: KeyBoardBuilder.() -> Unit) {
        val result = KeyBoardBuilder()
        result.block()
        presentsAndEmpty = result.keyboard.none { list -> list.isNotEmpty() }
        this.keyboard = Keyboard.Markup(result.oneTime, result.keyboard)
    }

    fun getKeyboard() = keyboard
}
