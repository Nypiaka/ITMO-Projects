package airline.api

import kotlinx.datetime.Instant

sealed class ManagementMsg

data class DelayFlight(
    val flightId: String,
    val departureTime: Instant,
    val actualDepartureTime: Instant,
) : ManagementMsg()

data class CancelFlight(val flightId: String, val departureTime: Instant) : ManagementMsg()

data class SetCheckInNumber(
    val flightId: String,
    val departureTime: Instant,
    val checkInNumber: String,
) : ManagementMsg()

data class SetGateNumber(val flightId: String, val departureTime: Instant, val gateNumber: String) :
    ManagementMsg()

data class ScheduleFlight(val flightId: String, val departureTime: Instant, val plane: Plane) :
    ManagementMsg()

data class BuyTicket(
    val flightId: String,
    val departureTime: Instant,
    val seatNo: String,
    val passengerId: String,
    val passengerName: String,
    val passengerEmail: String,
) : ManagementMsg()
