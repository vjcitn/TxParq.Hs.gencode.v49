test_that("transcripts() returns a GRanges", {
  tx <- transcripts(gtf)
  expect_s4_class(tx, "GRanges")
})

test_that("transcripts() has expected row count", {
  tx <- transcripts(gtf)
  expect_equal(length(tx), 507365L)
})

test_that("transcripts() names are unversioned by default", {
  tx <- transcripts(gtf)
  expect_false(any(grepl("\\.", names(tx))))
})

test_that("transcripts() mcols contain default columns", {
  tx <- transcripts(gtf)
  expected <- c("transcript_name", "transcript_type", "gene_id", "gene_name",
                "source", "level", "tags", "transcript_support_level",
                "havana_transcript", "ccdsid", "protein_id")
  expect_true(all(expected %in% colnames(mcols(tx))))
})

test_that("transcripts() filter by transcript_type works", {
  tx <- transcripts(gtf, filter = list(transcript_type = "protein_coding"))
  expect_true(length(tx) > 0L)
  expect_true(all(mcols(tx)$transcript_type == "protein_coding"))
})

test_that("transcripts() filter by chrom restricts seqnames", {
  tx <- transcripts(gtf, filter = list(chrom = "chrX"))
  expect_true(all(as.character(seqnames(tx)) == "chrX"))
})

test_that("transcripts() seqinfo carries genome build", {
  tx <- transcripts(gtf, filter = list(chrom = "chr1"))
  expect_true(all(genome(seqinfo(tx)) == "GRCh38"))
})

test_that("ENSG00000000003 has 13 transcripts", {
  tx <- transcripts(gtf, filter = list(chrom = "chrX"))
  gene_tx <- tx[mcols(tx)$gene_id == "ENSG00000000003.17"]
  expect_equal(length(gene_tx), 13L)
})
