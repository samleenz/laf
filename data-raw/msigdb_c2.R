## code to prepare `msigdb_h` dataset goes here

gsc <- msigdb::getMsigdb(org = "hs", id = "SYM", version = "7.5")


msigs_c2 <- msigdb::subsetCollection(gsc, "c2")
msigdb_c2 <-  setNames(lapply(msigs_c2, GSEABase::geneIds), lapply(msigs_c2, GSEABase::setName))

usethis::use_data(msigdb_c2, overwrite = TRUE)
