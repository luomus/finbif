test_that(
  "returns valid data", {

     expect_type(coords(list(67, 32, "ykj")), "character")
     expect_type(coords(list(67, 32, "ykj", 1)), "character")

  }
)
