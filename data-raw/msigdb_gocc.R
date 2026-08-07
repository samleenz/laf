## code to prepare `msigdb_gocc` dataset goes here

gsc <- msigdb::getMsigdb(org = "hs", id = "SYM", version = "7.5")


msigs_gocc <- msigdb::subsetCollection(gsc, subcollection = "GO:CC")
msigdb_gocc <-  setNames(lapply(msigs_gocc, GSEABase::geneIds), lapply(msigs_gocc, GSEABase::setName))



usethis::use_data(msigdb_gocc, overwrite = TRUE)
