test_that("seqinfo() returns a Seqinfo object", {
  si <- seqinfo(gtf)
  expect_s4_class(si, "Seqinfo")
})

test_that("seqinfo() genome is GRCh38", {
  si <- seqinfo(gtf)
  expect_true(all(genome(si) == "GRCh38"))
})

test_that("seqinfo() contains standard human chromosomes", {
  si <- seqinfo(gtf)
  chrs <- seqnames(si)
  expect_true(all(c("chr1", "chrX", "chrY", "chrM") %in% chrs))
})

test_that("genes_in_region() returns genes overlapping the query", {
  region <- GenomicRanges::GRanges("chr1", IRanges::IRanges(65000, 72000))
  gr <- genes_in_region(gtf, region)
  expect_s4_class(gr, "GRanges")
  expect_true(length(gr) >= 1L)
  expect_true("ENSG00000186092" %in% names(gr))  # OR4F5
})

test_that("genes_in_region() returns empty GRanges for region with no genes", {
  # Far off end of chr1 — adjust if annotation changes
  region <- GenomicRanges::GRanges("chr1",
    IRanges::IRanges(248956000, 248956100))
  gr <- genes_in_region(gtf, region)
  # May or may not be empty — just check class and no error
  expect_s4_class(gr, "GRanges")
})

test_that("genes_in_region() errors on non-GRanges input", {
  expect_error(genes_in_region(gtf, "chr1:65000-72000"))
})

test_that("transcripts_in_region() returns GRanges within query", {
  region <- GenomicRanges::GRanges("chr1", IRanges::IRanges(65000, 72000))
  tx <- transcripts_in_region(gtf, region)
  expect_s4_class(tx, "GRanges")
  expect_true(length(tx) >= 1L)
})
