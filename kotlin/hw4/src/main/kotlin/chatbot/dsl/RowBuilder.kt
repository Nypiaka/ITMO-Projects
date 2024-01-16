package chatbot.dsl

import chatbot.api.Keyboard

@BotCreator
class RowBuilder(private val keyboard: MutableList<MutableList<Keyboard.Button>>) {
    fun button(text: String) {
        keyboard.last().add(Keyboard.Button(text))
    }

    operator fun String.unaryMinus() {
        keyboard.last().add(Keyboard.Button(this))
    }
}
