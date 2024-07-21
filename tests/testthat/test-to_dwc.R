test_that("field name conversion works", {

  expect_equal(to_dwc("record_id"), "occurrenceID")

  expect_equal(to_native("occurrenceID"), "record_id")

  expect_equal(from_schema("unit.unitId"), "record_id")

})
