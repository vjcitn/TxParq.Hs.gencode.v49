test_that("transcriptsBy(by='gene') returns GRangesList", {
  tbg <- transcriptsBy(gtf, by = "gene")
  expect_s4_class(tbg, "GRangesList")
  expect_true(length(tbg) > 0L)
})

test_that("transcriptsBy names are unversioned gene IDs", {
  tbg <- transcriptsBy(gtf, by = "gene")
  expect_false(any(grepl("\\.", names(tbg))))
})

test_that("transcriptsBy has one list element per gene", {
  gr_genes <- genes(gtf)
  tbg <- transcriptsBy(gtf, by = "gene")
  # Every name in tbg should correspond to a gene
  expect_true(all(names(tbg) %in% names(gr_genes)))
})

test_that("ENSG00000000003 has 13 transcripts in transcriptsBy", {
  tbg <- transcriptsBy(gtf, by = "gene")
  expect_true("ENSG00000000003" %in% names(tbg))
  expect_equal(length(tbg[["ENSG00000000003"]]), 13L)
})

test_that("transcriptsBy mcols contain transcript_name and transcript_type", {
  tbg <- transcriptsBy(gtf, by = "gene")
  first <- tbg[[1]]
  expect_true(all(c("transcript_name", "transcript_type") %in% colnames(mcols(first))))
})

test_that("transcriptsBy filter reduces the result", {
  tbg_all  <- transcriptsBy(gtf, by = "gene")
  tbg_chr1 <- transcriptsBy(gtf, by = "gene",
                             filter = list(chrom = "chr1"))
  expect_true(length(tbg_chr1) < length(tbg_all))
})

test_that("transcriptsBy with unsupported 'by' raises error", {
  expect_error(transcriptsBy(gtf, by = "tx"), "only supports by='gene'")
})
