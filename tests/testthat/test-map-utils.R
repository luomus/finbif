test_that(
  "hist_xy works", {

    expect_type(
      hist_xy(matrix(runif(50), 25), list(seq(0, 1, .2), seq(0, 1, .2))),
      "list"
    )

  }
)

test_that(
  "breaks_xy works", {

    expect_type(
      breaks_xy(c(5, -45, 67, 100), 10),
      "list"
    )

  }
)
