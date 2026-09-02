test_that("converting to iso8061 works", {
  times <- list(
    list(
      iso8061 = NA_character_,
      event_date_start = as.Date(NA_character_),
      event_date_end = as.Date(NA_character_),
      hour_begin = NA_integer_,
      hour_end = NA_integer_,
      minute_begin = NA_integer_,
      minute_end = NA_integer_
    ),
    list(
      iso8061 = "1966",
      event_date_start = as.Date("1966-01-01"),
      event_date_end = as.Date("1966-12-31"),
      hour_begin = NA_integer_,
      hour_end = NA_integer_,
      minute_begin = NA_integer_,
      minute_end = NA_integer_
    ),
    list(
      iso8061 = "1966/1967",
      event_date_start = as.Date("1966-01-01"),
      event_date_end = as.Date("1967-12-31"),
      hour_begin = NA_integer_,
      hour_end = NA_integer_,
      minute_begin = NA_integer_,
      minute_end = NA_integer_
    ),
    list(
      iso8061 = "1966-01",
      event_date_start = as.Date("1966-01-01"),
      event_date_end = as.Date("1966-01-31"),
      hour_begin = NA_integer_,
      hour_end = NA_integer_,
      minute_begin = NA_integer_,
      minute_end = NA_integer_
    ),
    list(
      iso8061 = "1966-01/02",
      event_date_start = as.Date("1966-01-01"),
      event_date_end = as.Date("1966-02-28"),
      hour_begin = NA_integer_,
      hour_end = NA_integer_,
      minute_begin = NA_integer_,
      minute_end = NA_integer_
    ),
    list(
      iso8061 = "1966-01/1967-01",
      event_date_start = as.Date("1966-01-01"),
      event_date_end = as.Date("1967-01-31"),
      hour_begin = NA_integer_,
      hour_end = NA_integer_,
      minute_begin = NA_integer_,
      minute_end = NA_integer_
    ),
    list(
      iso8061 = "1966-01-01",
      event_date_start = as.Date("1966-01-01"),
      event_date_end = as.Date("1966-01-01"),
      hour_begin = NA_integer_,
      hour_end = NA_integer_,
      minute_begin = NA_integer_,
      minute_end = NA_integer_
    ),
    list(
      iso8061 = "1966-01-01/02",
      event_date_start = as.Date("1966-01-01"),
      event_date_end = as.Date("1966-01-02"),
      hour_begin = NA_integer_,
      hour_end = NA_integer_,
      minute_begin = NA_integer_,
      minute_end = NA_integer_
    ),
    list(
      iso8061 = "1966-01-01/02-01",
      event_date_start = as.Date("1966-01-01"),
      event_date_end = as.Date("1966-02-01"),
      hour_begin = NA_integer_,
      hour_end = NA_integer_,
      minute_begin = NA_integer_,
      minute_end = NA_integer_
    ),
    list(
      iso8061 = "1966-01-01/1967-01-01",
      event_date_start = as.Date("1966-01-01"),
      event_date_end = as.Date("1967-01-01"),
      hour_begin = NA_integer_,
      hour_end = NA_integer_,
      minute_begin = NA_integer_,
      minute_end = NA_integer_
    ),
    list(
      iso8061 = "1966-01-01T00",
      event_date_start = as.Date("1966-01-01"),
      event_date_end = as.Date("1966-01-01"),
      hour_begin = 0L,
      hour_end = NA_integer_,
      minute_begin = NA_integer_,
      minute_end = NA_integer_
    ),
    list(
      iso8061 = "1966-01-01T00/1966-01-01T01",
      event_date_start = as.Date("1966-01-01"),
      event_date_end = as.Date("1966-01-01"),
      hour_begin = 0L,
      hour_end = 1L,
      minute_begin = NA_integer_,
      minute_end = NA_integer_
    ),
    list(
      iso8061 = "1966-01-01T00/1966-01-02T00",
      event_date_start = as.Date("1966-01-01"),
      event_date_end = as.Date("1966-01-02"),
      hour_begin = 0L,
      hour_end = 0L,
      minute_begin = NA_integer_,
      minute_end = NA_integer_
    ),
    list(
      iso8061 = "1966-01-01T00/1966-02-01T00",
      event_date_start = as.Date("1966-01-01"),
      event_date_end = as.Date("1966-02-01"),
      hour_begin = 0L,
      hour_end = 0L,
      minute_begin = NA_integer_,
      minute_end = NA_integer_
    ),
    list(
      iso8061 = "1966-01-01T00/1967-01-01T00",
      event_date_start = as.Date("1966-01-01"),
      event_date_end = as.Date("1967-01-01"),
      hour_begin = 0L,
      hour_end = 0L,
      minute_begin = NA_integer_,
      minute_end = NA_integer_
    ),
    list(
      iso8061 = "1966-01-01T00:00",
      event_date_start = as.Date("1966-01-01"),
      event_date_end = as.Date("1966-01-01"),
      hour_begin = 0L,
      hour_end = NA_integer_,
      minute_begin = 0L,
      minute_end = NA_integer_
    ),
    list(
      iso8061 = "1966-01-01T00:00/1966-01-01T00:01",
      event_date_start = as.Date("1966-01-01"),
      event_date_end = as.Date("1966-01-01"),
      hour_begin = 0L,
      hour_end = 0L,
      minute_begin = 0L,
      minute_end = 1L
    ),
    list(
      iso8061 = "1966-01-01T00:00/1966-01-01T01:00",
      event_date_start = as.Date("1966-01-01"),
      event_date_end = as.Date("1966-01-01"),
      hour_begin = 0L,
      hour_end = 1L,
      minute_begin = 0L,
      minute_end = 0L
    ),
    list(
      iso8061 = "1966-01-01T00:00/1966-01-02T00:00",
      event_date_start = as.Date("1966-01-01"),
      event_date_end = as.Date("1966-01-02"),
      hour_begin = 0L,
      hour_end = 0L,
      minute_begin = 0L,
      minute_end = 0L
    ),
    list(
      iso8061 = "1966-01-01T00:00/1966-02-01T00:00",
      event_date_start = as.Date("1966-01-01"),
      event_date_end = as.Date("1966-02-01"),
      hour_begin = 0L,
      hour_end = 0L,
      minute_begin = 0L,
      minute_end = 0L
    ),
    list(
      iso8061 = "1966-01-01T00:00/1967-01-01T00:00",
      event_date_start = as.Date("1966-01-01"),
      event_date_end = as.Date("1967-01-01"),
      hour_begin = 0L,
      hour_end = 0L,
      minute_begin = 0L,
      minute_end = 0L
    )
  )

  expect_all_true(vapply(times, \(x) identical(get_iso8061(x), x$iso8061), NA))

})
