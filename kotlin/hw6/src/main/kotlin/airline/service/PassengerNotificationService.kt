package airline.service

import airline.api.Ticket

class PassengerNotificationService(private val emailService: BufferedEmailService) {
    suspend fun sendNotification(message: String, tickets: MutableCollection<Ticket>) {
        emailService.send(message, tickets)
    }

    suspend fun send(message: String, email: String) {
        emailService.send(email, message)
    }
}
