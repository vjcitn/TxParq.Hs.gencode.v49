test_that("genes() returns a GRanges", {
  gr <- genes(gtf)
  expect_s4_class(gr, "GRanges")
})

test_that("genes() has expected number of rows (full gencode v49)", {
  gr <- genes(gtf)
  expect_equal(length(gr), 78691L)
})

test_that("genes() names are unversioned Ensembl IDs by default", {
  gr <- genes(gtf)
  expect_false(any(grepl("\\.", names(gr))))
})

test_that("genes() names are versioned when use_versioned_ids=TRUE", {
  gr <- genes(gtf, use_versioned_ids = TRUE)
  expect_true(any(grepl("\\.", names(gr))))
})

test_that("genes() mcols contain default columns", {
  gr <- genes(gtf)
  expected <- c("gene_name", "gene_type", "source", "level", "tags", "havana_gene")
  # columns present in the data (level/tags/havana_gene may be all-NA)
  expect_true(all(expected %in% colnames(mcols(gr))))
})

test_that("genes() mcols level column is integer type", {
  gr <- genes(gtf)
  expect_true(is.integer(mcols(gr)$level))
})

test_that("genes() columns argument restricts mcols", {
  gr <- genes(gtf, columns = c("gene_name", "gene_type"))
  expect_equal(sort(colnames(mcols(gr))), sort(c("gene_name", "gene_type")))
})

test_that("genes() filter by gene_type returns only that type", {
  gr <- genes(gtf, filter = list(gene_type = "protein_coding"))
  expect_true(length(gr) > 0L)
  expect_true(all(mcols(gr)$gene_type == "protein_coding"))
})

test_that("genes() filter by chrom restricts seqnames", {
  gr <- genes(gtf, filter = list(chrom = "chr1"))
  expect_true(all(as.character(seqnames(gr)) == "chr1"))
})

test_that("genes() combined filter intersects correctly", {
  gr <- genes(gtf, filter = list(gene_type = "protein_coding", chrom = "chr1"))
  expect_true(all(mcols(gr)$gene_type == "protein_coding"))
  expect_true(all(as.character(seqnames(gr)) == "chr1"))
})

test_that("genes() seqinfo carries genome build", {
  gr <- genes(gtf)
  expect_true(all(genome(seqinfo(gr)) == "GRCh38"))
})

test_that("genes() returns sorted ranges (chrom then start)", {
  gr <- genes(gtf, filter = list(chrom = "chr1"))
  starts <- start(gr)
  expect_true(all(diff(starts) >= 0))
})

test_that("known gene ENSG00000186092 (OR4F5) is present and correct", {
  gr <- genes(gtf, filter = list(chrom = "chr1"))
  idx <- which(names(gr) == "ENSG00000186092")
  expect_length(idx, 1L)
  expect_equal(mcols(gr)$gene_name[idx], "OR4F5")
  expect_equal(start(gr)[idx], 65419L)
  expect_equal(end(gr)[idx], 71585L)
  expect_equal(as.character(strand(gr))[idx], "+")
})
