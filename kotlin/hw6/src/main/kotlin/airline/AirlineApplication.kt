package airline

import airline.api.*
import airline.service.*
import kotlin.time.Duration
import kotlin.time.Duration.Companion.minutes
import kotlinx.coroutines.*
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.flow.*
import kotlinx.datetime.Clock
import kotlinx.datetime.Instant

class AirlineApplication(val config: AirlineConfig, emailService: EmailService) {

    val managements = MutableSharedFlow<ManagementMsg>(extraBufferCapacity = Int.MAX_VALUE)

    var flights = MutableStateFlow<List<Flight>>(emptyList())

    private val bufferedEmailService = BufferedEmailService(emailService)

    private val notifier = PassengerNotificationService(bufferedEmailService)

    val bookingService: BookingService =
        object : BookingService {
            override val flightSchedule: List<FlightInfo>
                get() =
                    flights.value
                        .filter { cur ->
                            (cur.actualDepartureTime - Clock.System.now()) >=
                                config.ticketSaleEndTime
                        }
                        .map { cur ->
                            flightToFlightInfo(cur)
                        }

            override fun freeSeats(flightId: String, departureTime: Instant): Set<String> {
                val flight =
                    flights.value.findLast { flight ->
                        flight.flightId == flightId && flight.departureTime == departureTime
                    }
                if (flight == null) return emptySet()
                return flight.plane.seats subtract flight.tickets.keys
            }

            override suspend fun buyTicket(
                flightId: String,
                departureTime: Instant,
                seatNo: String,
                passengerId: String,
                passengerName: String,
                passengerEmail: String,
            ) {
                managements.emit(
                    BuyTicket(
                        flightId,
                        departureTime,
                        seatNo,
                        passengerId,
                        passengerName,
                        passengerEmail,
                    ),
                )
            }
        }

    val managementService: AirlineManagementService =
        object : AirlineManagementService {
            override suspend fun scheduleFlight(
                flightId: String,
                departureTime: Instant,
                plane: Plane,
            ) {
                managements.emit(ScheduleFlight(flightId, departureTime, plane))
            }

            override suspend fun delayFlight(
                flightId: String,
                departureTime: Instant,
                actualDepartureTime: Instant,
            ) {
                managements.emit(DelayFlight(flightId, departureTime, actualDepartureTime))
            }

            override suspend fun setCheckInNumber(
                flightId: String,
                departureTime: Instant,
                checkInNumber: String,
            ) {
                managements.emit(SetCheckInNumber(flightId, departureTime, checkInNumber))
            }

            override suspend fun cancelFlight(flightId: String, departureTime: Instant) {
                managements.emit(CancelFlight(flightId, departureTime))
            }

            override suspend fun setGateNumber(
                flightId: String,
                departureTime: Instant,
                gateNumber: String,
            ) {
                managements.emit(SetGateNumber(flightId, departureTime, gateNumber))
            }
        }

    @OptIn(FlowPreview::class)
    fun airportInformationDisplay(coroutineScope: CoroutineScope): StateFlow<InformationDisplay> {
        return flights
            .map { flight ->
                InformationDisplay(
                    flight.map { cur -> flightToFlightInfo(cur) },
                )
            }
            .sample(config.displayUpdateInterval)
            .stateIn(
                coroutineScope,
                SharingStarted.Eagerly,
                InformationDisplay(flightsInfo),
            )
    }

    val airportAudioAlerts: Flow<AudioAlerts> = flow {
        while (true) {
            flights.value.filter { flight ->
                flight.isOnRegistrationOpening()
            }.forEach { flight ->
                emit(AudioAlerts.RegistrationOpen(flight.flightId, flight.checkInNumber!!))
            }

            flights.value.filter { flight ->
                flight.isOnRegistrationClosing()
            }.forEach { flight ->
                emit(AudioAlerts.RegistrationClosing(flight.flightId, flight.checkInNumber!!))
            }

            flights.value.filter { flight ->
                flight.isOnBoardingOpening()
            }.forEach { flight ->
                emit(AudioAlerts.BoardingOpened(flight.flightId, flight.gateNumber!!))
            }

            flights.value.filter { flight ->
                flight.isOnBoardingClosing()
            }.forEach { flight ->
                emit(AudioAlerts.BoardingClosing(flight.flightId, flight.gateNumber!!))
            }

            delay(config.audioAlertsInterval)
        }
    }

    suspend fun run() {
        withContext(Dispatchers.Default) {
            launch {
                bufferedEmailService.runMass()
            }
            launch {
                bufferedEmailService.runPrivate()
            }
            launch {
                managements.collect { msg ->
                    when (msg) {
                        is ScheduleFlight -> {
                            flights.value += Flight(msg.flightId, msg.departureTime, plane = msg.plane)
                            flights.emit(flights.value)
                        }

                        is DelayFlight -> {
                            abstractAction(
                                msg.flightId,
                                msg.departureTime,
                                { index, newList ->
                                    newList[index] =
                                        flights.value[index].copy(
                                            actualDepartureTime = msg.actualDepartureTime,
                                        )
                                },
                                "flight (${msg.flightId}): delayed by " +
                                    "${msg.actualDepartureTime - msg.departureTime}",
                            )
                        }

                        is SetCheckInNumber -> {
                            abstractAction(
                                msg.flightId,
                                msg.departureTime,
                                { index, newList ->
                                    newList[index] =
                                        flights.value[index].copy(
                                            checkInNumber = msg.checkInNumber,
                                        )
                                },
                                "flight ${msg.flightId}: your check-in number has been changed to ${msg.checkInNumber}",
                            )
                        }

                        is CancelFlight -> {
                            abstractAction(
                                msg.flightId,
                                msg.departureTime,
                                { index, newList ->
                                    newList[index] = flights.value[index].copy(isCancelled = true)
                                },
                                "flight ${msg.flightId}: your flight was cancelled",
                            )
                        }

                        is SetGateNumber -> {
                            abstractAction(
                                msg.flightId,
                                msg.departureTime,
                                { index, newList ->
                                    newList[index] =
                                        flights.value[index].copy(gateNumber = msg.gateNumber)
                                },
                                "flight ${msg.flightId}: your gate number has been changed to ${msg.gateNumber}",
                            )
                        }

                        is BuyTicket -> {
                            abstractAction(
                                msg.flightId,
                                msg.departureTime,
                                { index, newList ->
                                    run {
                                        if (!newList[index].isCancelled &&
                                            newList[index].actualDepartureTime - Clock.System.now()
                                            >= config.ticketSaleEndTime &&
                                            !newList[index].tickets.containsKey(msg.seatNo) &&
                                            newList[index].plane.seats.contains(msg.seatNo)
                                        ) {
                                            val tickets = newList[index].tickets
                                            tickets[msg.seatNo] =
                                                Ticket(
                                                    msg.flightId,
                                                    msg.departureTime,
                                                    msg.seatNo,
                                                    msg.passengerId,
                                                    msg.passengerName,
                                                    msg.passengerEmail,
                                                )
                                            newList[index] = newList[index].copy(tickets = tickets)
                                            notifier.send(
                                                "Dear ${msg.passengerName}, flight ${msg.flightId}: " +
                                                    "you successfully bought ticket (${msg.seatNo})",
                                                msg.passengerEmail,
                                            )
                                        } else {
                                            notifier.send(
                                                "Dear ${msg.passengerName}, flight ${msg.flightId}: " +
                                                    "Failed to buy ticket on ${msg.seatNo}",
                                                msg.passengerEmail,
                                            )
                                        }
                                    }
                                },
                                notifyAll = false,
                            )
                        }
                    }
                }
            }
        }

    }

    private val flightsInfo: List<FlightInfo>
        get() = flights.value.map { cur ->
            flightToFlightInfo(cur)
        }

    private fun flightToFlightInfo(flight: Flight) = FlightInfo(
        flight.flightId,
        flight.departureTime,
        flight.isCancelled,
        flight.actualDepartureTime,
        flight.checkInNumber,
        flight.gateNumber,
        flight.plane,
    )

    private suspend fun abstractAction(
        flightId: String,
        departureTime: Instant,
        action: suspend (Int, MutableList<Flight>) -> Unit,
        message: String = "",
        notifyAll: Boolean = true,
    ) {
        val index =
            flights.value.indexOfFirst { flight ->
                flight.flightId == flightId && flight.departureTime == departureTime
            }
        if (index != -1) {
            val newList = flights.value.toMutableList()
            action(index, newList)
            flights.emit(newList)
            if (notifyAll) {
                notifier.sendNotification(
                    message,
                    flights.value[index].tickets.values,
                )
            }
        }
    }

    private fun Flight.isOnRegistrationOpening(curTime: Instant = Clock.System.now(), interval: Duration = 3.minutes) =
        this.actualDepartureTime - curTime <= config.registrationOpeningTime &&
            this.actualDepartureTime - curTime >=
            config.registrationClosingTime &&
            config.registrationOpeningTime -
            (this.actualDepartureTime - curTime) <= interval

    private fun Flight.isOnRegistrationClosing(curTime: Instant = Clock.System.now(), interval: Duration = 3.minutes) =
        this.actualDepartureTime - curTime <= config.registrationOpeningTime &&
            this.actualDepartureTime - curTime >=
            config.registrationClosingTime &&
            (this.actualDepartureTime - curTime) -
            config.registrationClosingTime <= interval

    private fun Flight.isOnBoardingOpening(curTime: Instant = Clock.System.now(), interval: Duration = 3.minutes) =
        this.actualDepartureTime - curTime <= config.boardingOpeningTime &&
            this.actualDepartureTime - curTime >=
            config.boardingClosingTime &&
            config.boardingOpeningTime -
            (this.actualDepartureTime - curTime) <= interval

    private fun Flight.isOnBoardingClosing(curTime: Instant = Clock.System.now(), interval: Duration = 3.minutes) =
        this.actualDepartureTime - curTime <= config.boardingOpeningTime &&
            this.actualDepartureTime - curTime >=
            config.boardingClosingTime &&
            (this.actualDepartureTime - curTime) -
            config.boardingClosingTime <= interval
}
