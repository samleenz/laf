## code to prepare `msigdb_c3TFT` dataset goes here

gsc <- msigdb::getMsigdb(org = "hs", id = "SYM", version = "7.5")


msigs_GTRD <- msigdb::subsetCollection(gsc, subcollection = "TFT:GTRD")
msigs_TFT <- msigdb::subsetCollection(gsc, subcollection = "TFT:TFT_Legacy")

msigdb_c3TFT <-  c(
  setNames(lapply(msigs_GTRD, GSEABase::geneIds), lapply(msigs_GTRD, GSEABase::setName)),
  setNames(lapply(msigs_TFT, GSEABase::geneIds), lapply(msigs_TFT, GSEABase::setName))
)


usethis::use_data(msigdb_c3TFT, overwrite = TRUE)
