package chatbot.dsl

import chatbot.api.Keyboard

@BotCreator
class KeyBoardBuilder {
    var oneTime = false
    var keyboard: MutableList<MutableList<Keyboard.Button>> = mutableListOf()
    fun row(block: RowBuilder.() -> Unit) {
        keyboard += mutableListOf<Keyboard.Button>()
        val row = RowBuilder(keyboard)
        row.block()
    }
}
