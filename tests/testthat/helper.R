# Shared fixture: one GTFParquet object per test session
gtf_path <- system.file("gc49", package = "TxParq.Hs.gencode.v49")
gtf <- GTFParquet(gtf_path)
