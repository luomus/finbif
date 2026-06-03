test_that("field name conversion works", {

  expect_identical(to_dwc("record_id"), "occurrenceID")

  expect_identical(to_native("occurrenceID"), "record_id")

  expect_identical(from_schema("unit.unitId"), "record_id")

})
