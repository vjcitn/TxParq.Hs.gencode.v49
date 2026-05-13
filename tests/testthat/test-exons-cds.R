test_that("exons() returns a GRanges", {
  ex <- exons(gtf, filter = list(chrom = "chr1"))
  expect_s4_class(ex, "GRanges")
  expect_true(length(ex) > 0L)
})

test_that("exons() mcols contain expected columns", {
  ex <- exons(gtf, filter = list(chrom = "chr1"))
  expect_true(all(c("exon_number", "transcript_id", "gene_id") %in% colnames(mcols(ex))))
})

test_that("exons() filter by chrom restricts seqnames", {
  ex <- exons(gtf, filter = list(chrom = "chr22"))
  expect_true(all(as.character(seqnames(ex)) == "chr22"))
})

test_that("OR4F5 (ENSG00000186092) has 3 exon records", {
  ex <- exons(gtf, filter = list(chrom = "chr1"))
  gene_ex <- ex[mcols(ex)$gene_id == "ENSG00000186092.7"]
  expect_equal(length(gene_ex), 3L)
})

test_that("exonsBy(by='tx') returns a GRangesList grouped by transcript", {
  ebt <- exonsBy(gtf, by = "tx")
  expect_s4_class(ebt, "GRangesList")
  expect_true(length(ebt) > 0L)
  # names should be unversioned transcript IDs
  expect_false(any(grepl("\\.", names(ebt))))
})

test_that("exonsBy(by='tx') exons within each element are ordered by exon_number", {
  ebt <- exonsBy(gtf, by = "tx")
  # exon_number is unquoted in GENCODE GTF; skip if old parquet has all-NA
  first_nums <- mcols(ebt[[1]])$exon_number
  if (all(is.na(first_nums))) {
    skip("exon_number is all-NA: regenerate parquet with fixed gtf_to_parquet.py")
  }
  set.seed(42)
  idx <- sample(length(ebt), min(20L, length(ebt)))
  for (i in idx) {
    nums <- mcols(ebt[[i]])$exon_number
    expect_true(isTRUE(all(diff(nums) >= 0)),
                info = paste("exon_number not sorted for element", i))
  }
})

test_that("exonsBy(by='gene') returns GRangesList grouped by gene", {
  ebg <- exonsBy(gtf, by = "gene")
  expect_s4_class(ebg, "GRangesList")
  expect_true("ENSG00000186092" %in% names(ebg))
})

test_that("cds() returns a GRanges with protein_id column", {
  cd <- cds(gtf, filter = list(chrom = "chr1"))
  expect_s4_class(cd, "GRanges")
  expect_true("protein_id" %in% colnames(mcols(cd)))
})

test_that("cds() frame column is integer", {
  cd <- cds(gtf, filter = list(chrom = "chr1"))
  expect_true(is.integer(mcols(cd)$frame))
})

test_that("OR4F5 has 2 CDS records", {
  cd <- cds(gtf, filter = list(chrom = "chr1"))
  gene_cds <- cd[mcols(cd)$gene_id == "ENSG00000186092.7"]
  expect_equal(length(gene_cds), 2L)
})

test_that("cdsBy(by='tx') returns GRangesList", {
  cbt <- cdsBy(gtf, by = "tx")
  expect_s4_class(cbt, "GRangesList")
  expect_true(length(cbt) > 0L)
})

test_that("cdsBy(by='gene') returns GRangesList grouped by gene", {
  cbg <- cdsBy(gtf, by = "gene")
  expect_s4_class(cbg, "GRangesList")
  expect_false(any(grepl("\\.", names(cbg))))
})
