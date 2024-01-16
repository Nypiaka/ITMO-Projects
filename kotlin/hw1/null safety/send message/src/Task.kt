fun sendMessageToClient(
    client: Client?,
    message: String?,
    mailer: Mailer
) {
    mailer.sendMessage(client?.personalInfo?.email ?: return, message ?: "Hello!")
}
