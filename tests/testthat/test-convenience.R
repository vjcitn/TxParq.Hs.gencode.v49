test_that("protein_coding_genes() returns only protein_coding genes", {
  gr <- protein_coding_genes(gtf)
  expect_s4_class(gr, "GRanges")
  expect_true(all(mcols(gr)$gene_type == "protein_coding"))
  expect_equal(length(gr), 20097L)
})

test_that("lncRNA_genes() returns only lncRNA genes", {
  gr <- lncRNA_genes(gtf)
  expect_s4_class(gr, "GRanges")
  expect_true(all(mcols(gr)$gene_type == "lncRNA"))
  expect_equal(length(gr), 34880L)
})

test_that("gene_types() returns a table", {
  gt <- gene_types(gtf)
  expect_true(is.table(gt))
  expect_true("protein_coding" %in% names(gt))
  expect_equal(unname(gt["protein_coding"]), 20097L)
})

test_that("transcript_types() returns a table", {
  tt <- transcript_types(gtf)
  expect_true(is.table(tt))
  expect_true("protein_coding" %in% names(tt))
})

test_that("protein_coding_genes() passes extra args (columns) through", {
  gr <- protein_coding_genes(gtf, columns = "gene_name")
  expect_equal(colnames(mcols(gr)), "gene_name")
})
