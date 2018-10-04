### ===============================================================================================
### R package RCarb BUILDSCRIPTS
### BibTeX
### sebastian.kreutzer@u-bordeaux-montaigne.fr
### 2018-10-03
### ===============================================================================================

library(tools)
library(RCarb)

##get version number
temp <- readLines("DESCRIPTION")
temp <- temp[grep("Version", temp)]
temp.version <- sub(" ","",unlist(strsplit(temp,":"))[2])

package.citation <- toBibtex(citation("RCarb"))
write(package.citation, file=paste0("RCarb.BuildResults/RCarb_", temp.version,"-bibliography.bib"))
