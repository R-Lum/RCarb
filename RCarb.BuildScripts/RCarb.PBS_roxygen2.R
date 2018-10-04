### ===============================================================================================
### R package RCarb BUILDSCRIPTS
### roxygen2
### sebastian.kreutzer@u-bordeaux-montaigne.fr
### 2018-10-03
### ===============================================================================================
if(!require("devtools"))
  install.packages("devtools")

library(devtools)
document(pkg = ".", roclets = NULL)
