#cd C:/fvs_tools/fvstools
#C:/FVS/FVSSoftware/R/R-4.5.0/bin/x64/RScript dev/build-pkg.R

#Document
devtools::document()

#Build
devtools::build()

#Install
devtools::install(upgrade = FALSE)

