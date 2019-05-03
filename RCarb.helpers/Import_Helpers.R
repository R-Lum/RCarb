## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Title:   Helper for translating the MATLAB code to R; not part of the package
## Author:  Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
## Contact: sebastian.kreutzer@u-bordeaux-montaigne.fr
## Date:    Tue Oct  2 10:51:03 2018
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rm(list = ls())

# Reference data ------------------------------------------------------------------------------
files <- c("DATAek.txt",
           "DATAet.txt",
           "DATAeu.txt",
           "DATApk.txt",
           "DATApt.txt",
           "DATApu.txt",
           "DATAeu238.txt",
           "DATAeu234.txt",
           "DATAet230.txt",
           "DATApu238.txt",
           "DATApu234.txt",
           "DATApt230.txt"
           )


for(i in files){
  temp <- read.table(
    file = paste0("helpers/carb2007/", i),
    header = FALSE
  )

  rownames(temp) <- as.character(temp[,1])
  colnames(temp) <- as.character(temp[1,])

  temp <- as.matrix(temp)
  temp <- temp[-1,-1]

  assign(
    x = strsplit(i, ".", fixed = TRUE)[[1]][1] ,
    value = temp
  )


}

rm(temp)
rm(i)
rm(files)

mejdahl <- read.table("helpers/carb2007/mejdahl.txt", header = TRUE)
colnames(mejdahl) <- c("D", "K", "Th", "U")
save(list = ls(), file = "data/Reference_Data.RData", compress = "xz")
rm(list = ls())


##Overwrite reference data
##load reference data into global environment
##load reference data into global environment
rm(list = ls())
names <- load(file = "data/Reference_Data.RData")

##write in list
Reference_Data <- lapply(names, get)

##set names
names(Reference_Data) <- names

##remove objects
rm(list = names)
save(list = "Reference_Data", file = "data/Reference_Data.RData", compress = "xz")



# Example data -------------------------------------------------------------------------------
rm(list = ls())
##import example data
temp <- t(read.table("helpers/carb2007/SAMPLE_DATA.txt", header = FALSE))

colnames(temp) <- temp[1,]
rownames(temp) <- NULL
temp <- temp[-1,]
temp <- as.data.frame(temp, stringsAsFactors = FALSE)

for(i in 2:ncol(temp)){
  temp[[i]] <- as.numeric(temp[[i]])

}

##remove the last two columns (ERROR and STEP1), we don't need them here
Example_Data <- temp[,-c(ncol(temp)-1,ncol(temp))]

##pass description to each column

#SAMP_NAME
attr(Example_Data$SAMP_NAME, "UNIT") <- NA
attr(Example_Data$SAMP_NAME, "DESCRIPTION") <- "Sample name, unique identifier"

#K, K_X
attr(Example_Data$K, "UNIT") <- "%"
attr(Example_Data$K, "DESCRIPTION") <- "K concentration"
attr(Example_Data$K_X, "UNIT") <- "%"
attr(Example_Data$K_X, "DESCRIPTION") <- "K concentration standard error"

#T, T_X
attr(Example_Data$T, "UNIT") <- "ppm"
attr(Example_Data$T, "DESCRIPTION") <- "Th concentration"
attr(Example_Data$T_X, "UNIT") <- "ppm"
attr(Example_Data$T_X, "DESCRIPTION") <- "Th concentration standard error"

#U, U_X
attr(Example_Data$U, "UNIT") <- "ppm"
attr(Example_Data$U, "DESCRIPTION") <- "U concentration"
attr(Example_Data$U_X, "UNIT") <- "ppm"
attr(Example_Data$U_X, "DESCRIPTION") <- "U concentration standard error"

#U238, U238_X
attr(Example_Data$U238, "UNIT") <- "ppm"
attr(Example_Data$U238, "DESCRIPTION") <- "U-238 concentration"
attr(Example_Data$U238_X, "UNIT") <- "ppm"
attr(Example_Data$U238_X, "DESCRIPTION") <- "U-238 concentration standard error"

#234_U238,234_U238_X
attr(Example_Data$U234_U238, "UNIT") <- NA
attr(Example_Data$U234_U238, "DESCRIPTION") <- "U-234/U-238 activity ratio"
attr(Example_Data$U234_U238_X, "UNIT") <- NA
attr(Example_Data$U234_U238_X, "DESCRIPTION") <- "U-234/U-238 activity ratio standard error"

#WCI, WCI_X
attr(Example_Data$WCI, "UNIT") <- "% dry wt."
attr(Example_Data$WCI, "DESCRIPTION") <- "Initial water content"
attr(Example_Data$WCI_X, "UNIT") <- "% dry wt."
attr(Example_Data$WCI_X, "DESCRIPTION") <- "Initial water content standard error"

#WCF, WCF_X
attr(Example_Data$WCF, "UNIT") <- "% dry wt."
attr(Example_Data$WCF, "DESCRIPTION") <- "Final water content"
attr(Example_Data$WCF_X, "UNIT") <- "% dry wt."
attr(Example_Data$WCF_X, "DESCRIPTION") <- "Final water content standard error"

#CC, CC_X
attr(Example_Data$CC, "UNIT") <- "% dry wt."
attr(Example_Data$CC, "DESCRIPTION") <- "Carbonate content"
attr(Example_Data$CC_X, "UNIT") <- "% dry wt."
attr(Example_Data$CC_X, "DESCRIPTION") <- "Carbonate content standard error"

#DIAM, DIAM_X
attr(Example_Data$DIAM, "UNIT") <- "m x 10^-6"
attr(Example_Data$DIAM, "DESCRIPTION") <- "Grain diameter"
attr(Example_Data$DIAM_X, "UNIT") <- "m x 10^-6"
attr(Example_Data$DIAM_X, "DESCRIPTION") <- "Grain diameter standard error"

#COSMIC, COSMIC_X+
attr(Example_Data$COSMIC, "UNIT") <- "Gy/ka"
attr(Example_Data$COSMIC, "DESCRIPTION") <- "Cosmic dose rate"
attr(Example_Data$COSMIC_X, "UNIT") <- "Gy/ka"
attr(Example_Data$COSMIC_X, "DESCRIPTION") <- "Cosmic dose rate standard error"

#INTERNAL, INTERNAL_X
attr(Example_Data$INTERNAL, "UNIT") <- "Gy/ka"
attr(Example_Data$INTERNAL, "DESCRIPTION") <- "Internal dose rate"
attr(Example_Data$INTERNAL_X, "UNIT") <- "Gy/ka"
attr(Example_Data$INTERNAL_X, "DESCRIPTION") <- "Internal dose standard error"

#ONSET, ONSET_X
attr(Example_Data$ONSET, "UNIT") <- "ka"
attr(Example_Data$ONSET, "DESCRIPTION") <- "Carbonate onset"
attr(Example_Data$ONSET_X, "UNIT") <- "ka"
attr(Example_Data$ONSET_X, "DESCRIPTION") <- "Carbonate onset standard error"

#FINISH, FINISH_X
attr(Example_Data$FINISH, "UNIT") <- "ka"
attr(Example_Data$FINISH, "DESCRIPTION") <- "Carbonate completion"
attr(Example_Data$FINISH_X, "UNIT") <- "ka"
attr(Example_Data$FINISH_X, "DESCRIPTION") <- "Carbonate completion standard error"

#DE, DE_X
attr(Example_Data$DE, "UNIT") <- "Gy"
attr(Example_Data$DE, "DESCRIPTION") <- "Equivalent dose"
attr(Example_Data$DE_X, "UNIT") <- "Gy"
attr(Example_Data$DE_X, "DESCRIPTION") <- "Equivalent dose standard error"

##add package to data.frame
attr(Example_Data, "package") <- c("RCarb")

save(list = "Example_Data", file = "data/Example_Data.RData", compress = "xz")
#rm(list = ls())