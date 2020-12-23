test_that(
  "returns valid data", {

     expect_type(coords(67, 32, "kkj"), "character")
     expect_type(coords(67, 32, "kkj", 1), "character")

  }
)
