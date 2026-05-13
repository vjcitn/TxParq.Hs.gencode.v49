test_that("GTFParquet constructor returns correct class", {
  expect_s4_class(gtf, "GTFParquet")
})

test_that("GTFParquet constructor fails on missing path", {
  expect_error(GTFParquet("/nonexistent/path"))
})

test_that("GTFParquet reports genome as GRCh38", {
  expect_equal(genome(gtf), "GRCh38")
})

test_that("GTFParquet reports all expected tables as available", {
  av <- gtf@available
  expect_true(av[["genes"]])
  expect_true(av[["transcripts"]])
  expect_true(av[["exons"]])
  expect_true(av[["cds"]])
  expect_true(av[["features"]])
  expect_true(av[["metadata"]])
})

test_that("show method prints without error", {
  expect_output(show(gtf), "GTFParquet")
})

test_that("gtf_metadata returns named character vector with expected keys", {
  meta <- gtf_metadata(gtf)
  expect_type(meta, "character")
  expect_true(all(c("genome", "provider", "format") %in% names(meta)))
  expect_equal(unname(meta["genome"]), "GRCh38")
  expect_equal(unname(meta["provider"]), "GENCODE")
})
