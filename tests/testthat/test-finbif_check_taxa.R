context('Checking FinBIF taxa')

test_that("returns valid data", {
  vcr::use_cassette("finbif_check_taxa", {
    resp_list_sp_true <- finbif_check_taxa(list(species = "Parus major"))
    resp_list_sp_false <- finbif_check_taxa(list(species = "Parus notmajor"))
    resp_list_rank_false <- finbif_check_taxa(list(genus = "Parus major"))
  }, preserve_exact_body_bytes = TRUE)

  expect_type(resp_list_sp_true, "list")
  expect_type(resp_list_sp_false, "list")
  expect_type(resp_list_rank_false, "list")
})
