package airline

import airline.api.AirlineConfig
import airline.api.AudioAlerts
import airline.api.Plane
import airline.service.EmailService
import kotlin.time.Duration.Companion.hours
import kotlin.time.Duration.Companion.milliseconds
import kotlin.time.Duration.Companion.minutes
import kotlin.time.Duration.Companion.seconds
import kotlinx.coroutines.*
import kotlinx.coroutines.channels.Channel
import kotlinx.datetime.Clock
import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.Test

class ExampleTestFirst {

    private suspend fun checkEmails(emailService: InChannelEmailService, email: String, vararg text: String) {
        val (serviceEmail, serviceText) = emailService.messages.receive()
        Assertions.assertEquals(email, serviceEmail)
        for (part in text) {
            Assertions.assertTrue(part in serviceText)
        }
    }

    private fun <T> eq(a: T, b: T) {
        Assertions.assertEquals(a, b)
    }

    @Test
    fun simpleTest() {
        val emailService = InChannelEmailService()
        val config = AirlineConfig(
            audioAlertsInterval = 1.seconds,
            displayUpdateInterval = 10.milliseconds,
            ticketSaleEndTime = 3599.seconds,
        )
        val airlineApplication = AirlineApplication(config, emailService)
        val plane1 = Plane("A312", setOf("1A", "1B", "2A", "2B"))
        val flightId = "111"
        val flightTime = Clock.System.now() + 1.hours

        testAndCancel {
            launch { airlineApplication.run() }
            sleep()

            val booking = airlineApplication.bookingService
            val management = airlineApplication.managementService
            val display = airlineApplication.airportInformationDisplay(this)

            management.scheduleFlight(flightId, flightTime, plane1)
            sleep()

            eq(1, display.value.departing.size)
            eq("111", display.value.departing[0].flightId)

            eq(1, booking.flightSchedule.size)
            eq("111", booking.flightSchedule[0].flightId)

            sleep()

            booking.buyTicket("111", flightTime, "1A", "1", "Konstantin Bats", "kbats@itmo.ru")
            delay(100.milliseconds)

            checkEmails(emailService, "kbats@itmo.ru", "111", "1A")
            eq(booking.freeSeats(flightId, flightTime), setOf("1B", "2A", "2B"))

            eq(1, display.value.departing.size)
            management.setCheckInNumber(flightId, flightTime, "checkin1")
            sleep()

            checkEmails(emailService, "kbats@itmo.ru", "checkin1")

            eq(1, display.value.departing.size)
            eq("111", display.value.departing[0].flightId)
            eq("checkin1", display.value.departing[0].checkInNumber)

            management.delayFlight(flightId, flightTime, flightTime + 1.hours)
            sleep()

            checkEmails(emailService, "kbats@itmo.ru", "delayed")

            eq(1, display.value.departing.size)
            eq("111", display.value.departing[0].flightId)
            eq("checkin1", display.value.departing[0].checkInNumber)
            eq(flightTime, display.value.departing[0].departureTime)
            eq(flightTime + 1.hours, display.value.departing[0].actualDepartureTime)
        }

    }

    @Test
    fun multipleFlightsTest() {
        val emailService = InChannelEmailService()
        val config = AirlineConfig(
            audioAlertsInterval = 1.seconds,
            displayUpdateInterval = 10.milliseconds,
            ticketSaleEndTime = 3599.seconds,
            registrationOpeningTime = 19.9999.seconds,
            registrationClosingTime = 1.seconds,
            boardingOpeningTime = 19.9999.seconds,
            boardingClosingTime = 0.5.seconds,
        )
        val airlineApplication = AirlineApplication(config, emailService)
        val plane1 = Plane("A312", setOf("1A", "1B", "2A", "2B"))
        val plane2 = Plane("B747", setOf("1A", "1B", "2A", "2B", "3A", "3B"))
        val plane3 = Plane("C666", setOf("1A", "1B", "2A", "2B", "3A", "3B", "4A", "5A"))
        val flightId1 = "111"
        val flightTime1 = Clock.System.now() + 1.hours
        val flightId2 = "222"
        val flightTime2 = Clock.System.now() + 3.hours
        val flightId3 = "333"
        val flightTime3 = Clock.System.now() + 20.seconds

        testAndCancel {
            launch { airlineApplication.run() }
            sleep()

            val booking = airlineApplication.bookingService
            val management = airlineApplication.managementService
            val display = airlineApplication.airportInformationDisplay(this)

            management.scheduleFlight(flightId1, flightTime1, plane1)
            management.scheduleFlight(flightId2, flightTime2, plane2)
            management.scheduleFlight(flightId3, flightTime3, plane3)
            sleep()

            eq(3, display.value.departing.size)
            eq(flightId1, display.value.departing[0].flightId)
            eq(flightId2, display.value.departing[1].flightId)
            eq(flightId3, display.value.departing[2].flightId)
            sleep()

            eq(2, booking.flightSchedule.size)
            eq(flightId1, booking.flightSchedule[0].flightId)
            eq(flightId2, booking.flightSchedule[1].flightId)

            sleep()

            booking.buyTicket(flightId1, flightTime1, "1A", "1", "Konstantin Bats", "kbats@itmo.ru")
            booking.buyTicket(flightId2, flightTime2, "1A", "1", "Ivan Petrov", "ipetrov@mail.ru")
            delay(100.milliseconds)
            checkEmails(emailService, "kbats@itmo.ru", flightId1, "1A")
            checkEmails(emailService, "ipetrov@mail.ru", flightId2, "1A")

            eq(booking.freeSeats(flightId1, flightTime1), setOf("1B", "2A", "2B"))
            eq(booking.freeSeats(flightId2, flightTime2), setOf("1B", "2A", "2B", "3A", "3B"))

            eq(3, display.value.departing.size)
            management.setCheckInNumber(flightId1, flightTime1, "checkin1")
            management.setCheckInNumber(flightId2, flightTime2, "checkin2")
            management.setCheckInNumber(flightId3, flightTime3, "checkin3")

            checkEmails(emailService, "kbats@itmo.ru", "checkin1")
            checkEmails(emailService, "ipetrov@mail.ru", "checkin2")
            sleep()

            eq(3, display.value.departing.size)
            eq(flightId1, display.value.departing[0].flightId)
            eq("checkin1", display.value.departing[0].checkInNumber)
            eq(flightId2, display.value.departing[1].flightId)
            eq("checkin2", display.value.departing[1].checkInNumber)

            management.delayFlight(flightId1, flightTime1, flightTime1 + 30.minutes)
            management.delayFlight(flightId2, flightTime2, flightTime2 + 2.hours)
            sleep()

            checkEmails(emailService, "kbats@itmo.ru", "111", "delayed")
            checkEmails(emailService, "ipetrov@mail.ru", "222", "delayed")

            eq(3, display.value.departing.size)
            eq(flightId1, display.value.departing[0].flightId)
            eq("checkin1", display.value.departing[0].checkInNumber)
            eq(flightTime1, display.value.departing[0].departureTime)
            eq(flightTime1 + 30.minutes, display.value.departing[0].actualDepartureTime)
            eq(flightId2, display.value.departing[1].flightId)
            eq("checkin2", display.value.departing[1].checkInNumber)
            eq(flightTime2, display.value.departing[1].departureTime)
            eq(flightTime2 + 2.hours, display.value.departing[1].actualDepartureTime)

            management.cancelFlight(flightId1, flightTime1)
            sleep()
            checkEmails(emailService, "kbats@itmo.ru", "111", "cancelled")
            eq(true, display.value.departing[0].isCancelled)

            management.setGateNumber(flightId3, flightTime3, "gate3")

            val audio = airlineApplication.airportAudioAlerts
            val counter = mutableSetOf<AudioAlerts>()
            audio.collect { notification ->
                when (notification) {
                    is AudioAlerts.RegistrationOpen -> {
                        counter.add(notification)
                    }

                    is AudioAlerts.RegistrationClosing -> {
                        counter.add(notification)
                    }

                    is AudioAlerts.BoardingOpened -> {
                        counter.add(notification)
                    }

                    is AudioAlerts.BoardingClosing -> {
                        counter.add(notification)
                        if (counter.size == 4) {
                            Assertions.assertTrue(counter.contains(AudioAlerts.RegistrationOpen(flightId3, "checkin3")))
                            Assertions.assertTrue(
                                counter.contains(
                                    AudioAlerts.RegistrationClosing(
                                        flightId3,
                                        "checkin3",
                                    ),
                                ),
                            )
                            Assertions.assertTrue(counter.contains(AudioAlerts.BoardingOpened(flightId3, "gate3")))
                            Assertions.assertTrue(counter.contains(AudioAlerts.BoardingClosing(flightId3, "gate3")))
                            this.cancel()
                        }
                    }
                }
            }
        }
    }

    @Test
    fun ticketSaleEndTimeTest() {
        val emailService = InChannelEmailService()
        val config = AirlineConfig(
            audioAlertsInterval = 1.seconds,
            displayUpdateInterval = 10.milliseconds,
            ticketSaleEndTime = 3598.seconds,
        )
        val airlineApplication = AirlineApplication(config, emailService)
        val plane1 = Plane("A312", setOf("1A", "1B", "2A", "2B"))
        val flightId = "111"
        val flightTime = Clock.System.now() + 1.hours

        testAndCancel {
            launch { airlineApplication.run() }
            sleep()

            val booking = airlineApplication.bookingService
            val management = airlineApplication.managementService
            val display = airlineApplication.airportInformationDisplay(this)

            management.scheduleFlight(flightId, flightTime, plane1)
            sleep()

            eq(1, display.value.departing.size)
            eq("111", display.value.departing[0].flightId)

            eq(1, booking.flightSchedule.size)
            eq("111", booking.flightSchedule[0].flightId)

            booking.buyTicket("111", flightTime, "1A", "1", "Konstantin Bats", "kbats@itmo.ru")
            sleep()
            eq(setOf("1B", "2A", "2B"), booking.freeSeats(flightId, flightTime))

            booking.buyTicket("111", flightTime, "10A", "1", "Konstantin Bats", "kbats@itmo.ru")
            sleep()
            eq(setOf("1B", "2A", "2B"), booking.freeSeats(flightId, flightTime))

            delay(3.seconds)
            eq(0, booking.flightSchedule.size)

            booking.buyTicket("111", flightTime, "1B", "2", "Ivan Petrov", "ipetrov@mail.ru")

            eq(1, display.value.departing.size)
            eq("111", display.value.departing[0].flightId)

            eq(0, booking.flightSchedule.size)
            eq(setOf("1B", "2A", "2B"), booking.freeSeats(flightId, flightTime))
        }

    }

    private fun testAndCancel(block: suspend CoroutineScope.() -> Unit) {
        try {
            runBlocking {
                block()
                cancel()
            }
        } catch (ignore: CancellationException) {
        }
    }

    private suspend fun sleep() {
        delay(50.milliseconds)
    }

    private class InChannelEmailService : EmailService {
        val messages = Channel<Pair<String, String>>()

        override suspend fun send(to: String, text: String) {
            messages.send(to to text)
        }
    }
}
