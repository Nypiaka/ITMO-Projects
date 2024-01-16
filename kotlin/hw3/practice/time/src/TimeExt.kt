val Long.hours: Time
    get() = Time(this * 3600, 0)

val Long.minutes: Time
    get() = Time(this * 60, 0)

val Long.seconds: Time
    get() = Time(this, 0)

val Long.milliseconds: Time
    get() = Time(this / 1000, (this % 1000).toInt())

val Int.hours: Time
    get() = this.toLong().hours

val Int.minutes: Time
    get() = this.toLong().minutes

val Int.seconds: Time
    get() = this.toLong().seconds

val Int.milliseconds: Time
    get() = this.toLong().milliseconds

operator fun Time.minus(time: Time): Any {
    val secondsPart = if (this.seconds >= time.seconds) 0 else 1
    return normaliseTime(
        this.seconds - time.seconds - secondsPart,
        this.milliseconds + 1000 * secondsPart - time.milliseconds
    )
}

operator fun Time.plus(time: Time): Any {
    return normaliseTime(
        this.seconds + time.seconds,
        this.milliseconds + time.milliseconds
    )

}

operator fun Time.times(num: Int): Any {
    return normaliseTime(this.seconds * num, this.milliseconds * num)
}

fun normaliseTime(seconds: Long, milliseconds: Int): Time {
    return Time(seconds + milliseconds / 1000, milliseconds % 1000)
}