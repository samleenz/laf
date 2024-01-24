## code to prepare `msigdb_h` dataset goes here

gsc <- msigdb::getMsigdb(org = "hs", id = "SYM", version = "7.5")


msigs_hallmarks <- msigdb::subsetCollection(gsc, "h")
msigdb_h <-  setNames(lapply(msigs_hallmarks, GSEABase::geneIds), lapply(msigs_hallmarks, GSEABase::setName))

usethis::use_data(msigdb_h, overwrite = TRUE)
