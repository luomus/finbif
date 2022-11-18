test_that(
  "setting seed works", {

    rm(".Random.seed", pos = 1L)

    sample_with_seed(10, 10, 123)

    expect_false(exists(".Random.seed", 1L))

  }
)

test_that(
  "getting locale works", {

    Sys.setenv(LANGUAGE = "Finnish-Finland")

    expect_match(get_locale(), "fi")

  }
)

test_that(
  "dummy function for conditional package use works", {

    expect_match(value("a"), "a")

  }
)

test_that(
  "variable conversion works", {

    expect_match(from_schema("unit.unitId"), "record_id")

  }
)

test_that(
  "concatenating NA works", {

    expect_equal(concat_string(NA_character_), NA_character_)

  }
)

