test_that("utrs() returns a GRanges", {
  u <- utrs(gtf)
  expect_s4_class(u, "GRanges")
  expect_true(length(u) > 0L)
})

test_that("utrs() mcols contain feature_type, transcript_id, gene_id", {
  u <- utrs(gtf)
  expect_true(all(c("feature_type", "transcript_id", "gene_id") %in% colnames(mcols(u))))
})

test_that("utrs(type='5prime') returns only 5' UTR feature types", {
  u5 <- utrs(gtf, type = "5prime")
  expect_true(all(mcols(u5)$feature_type %in% c("five_prime_utr", "UTR")))
})

test_that("utrs(type='3prime') returns only three_prime_utr", {
  u3 <- utrs(gtf, type = "3prime")
  expect_true(all(mcols(u3)$feature_type == "three_prime_utr"))
})

test_that("utrs(type='both') count >= utrs(type='5prime') + utrs(type='3prime')", {
  u_both <- utrs(gtf)
  u5 <- utrs(gtf, type = "5prime")
  u3 <- utrs(gtf, type = "3prime")
  expect_gte(length(u_both), length(u5) + length(u3))
})

test_that("codons() returns a GRanges", {
  co <- codons(gtf)
  expect_s4_class(co, "GRanges")
  expect_true(length(co) > 0L)
})

test_that("codons() mcols contain feature_type, transcript_id, gene_id, frame", {
  co <- codons(gtf)
  expect_true(all(c("feature_type", "transcript_id", "gene_id", "frame") %in%
                    colnames(mcols(co))))
})

test_that("codons(type='start') returns only start_codon", {
  sc <- codons(gtf, type = "start")
  expect_true(all(mcols(sc)$feature_type == "start_codon"))
})

test_that("codons(type='stop') returns only stop_codon", {
  st <- codons(gtf, type = "stop")
  expect_true(all(mcols(st)$feature_type == "stop_codon"))
})

test_that("start codon count matches expected", {
  sc <- codons(gtf, type = "start")
  expect_equal(length(sc), 221175L)
})
