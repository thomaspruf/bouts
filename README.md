# bouts
Compute torpor characteristics R package

R package to compute duration and other specifications of bouts of torpor, alternating with euthermia. First install R from CRAN, next install RStudio. Subsequently install bouts in R. Goto RStudio Console and write: 

install.packages("devtools")
library(devtools)
install_github("thomaspruf/bouts")

You need to install only once. Then and in the future run it:

library(bouts)
countbouts()

Select an iButton file on your hard disk. Always store iButton files as csv files. Use comma (",") as separator between columns. As examples, you find two files in the bouts directory inst/extdata. They are are called M65C6.csv and TempAct_ID66.csv. They are an original iButton file and a FIWI-made logger file. Choose either one.You should see a plot. Select the first and last point for all computations. If recorded, activity of the animals is also shown.

To alter the computation, change the last line in "countbouts()". For example, do

 bcounts(selected,name,thresh=25, min_torpid=24, min_wake=24,t_res=1)

This sets the threshold for  torpor to 25 C. The animal needs to be at least 24 h torpid for the beginning of hibernation an to be euthermic for at least 24 h for the end of hibernation. The temporal resolution is set to 1 minute (using linear interpolation).

Countbouts() returns a data.frame with results. See ?countbouts for a definition of each variable.
