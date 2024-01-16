package airline.service

import airline.api.Ticket
import kotlinx.coroutines.channels.Channel

class BufferedEmailService(private val emailService: EmailService) :
    EmailService {
        private val privateLetters = Channel<Pair<String, String>>(capacity = 1000)
        private val massLetters = Channel<Pair<String, MutableCollection<Ticket>>>(capacity = 1000)
        override suspend fun send(to: String, text: String) {
            privateLetters.send(Pair(to, text))
        }

        suspend fun send(letter: String, tickets: MutableCollection<Ticket>) {
            massLetters.send(Pair(letter, tickets))
        }

        suspend fun runPrivate() {
            while (true) {
                val message = privateLetters.receive()
                emailService.send(message.first, message.second)
            }
        }

        suspend fun runMass() {
            while (true) {
                val messages = massLetters.receive()
                messages.second.forEach {
                    emailService.send(it.passengerEmail, "Dear ${it.passengerName}, " + messages.first)
                }
            }
        }
    }
