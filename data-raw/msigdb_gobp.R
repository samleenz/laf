## code to prepare `msigdb_gobp` dataset goes here

gsc <- msigdb::getMsigdb(org = "hs", id = "SYM", version = "7.5")


msigs_gobp <- msigdb::subsetCollection(gsc, subcollection = "GO:BP")
msigdb_gobp <-  setNames(lapply(msigs_gobp, GSEABase::geneIds), lapply(msigs_gobp, GSEABase::setName))

usethis::use_data(msigdb_gobp, overwrite = TRUE)
