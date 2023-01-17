\name{countbouts}
\alias{countbouts}
\title{
Select hibernation and compute bouts
}
\description{
Does it all
1. Pick data file
2. Pick start and end using mouse

Data file: produce my using Microsoft Excel. Save as comma separated file (.csv). Delete info (several lines) at beginning of ibutton files. Also delete histogram and possibly info at the end of ibutton files. Keep actual recordings and times. See the difference between txt files (=originals) and csv files in bouts/inst/ext.
Keep a single line with variable names. In case of Tb it may be called Value, Temp, or Temperature. If activity was recorded the variable may be called Activity or Act. Also keep a variable Date.Time (within two commas) or separate variables for Date and Time (see examples). Select a threshold for torpor, e.g.30 °C (30). Make sure variable Tb and Act are numers with . as  decinal point,

In case the time variable contains AM or PM reformat using Excel. In Excel, right click on the column. Select format cells, select Custom, select DD.MM YY hh:mm  (or TT.MM.JJ hh:mm if you have, for instance, German regional settings on your computer). Save as csv. Also use Excel to make other changes to input files, use Find & Replace.

Start with countbouts(30). Add more specifications (lookup and change bcounts) if necessary.

}
\usage{
countbouts()
}


\value{
A dataframe with time/duration of each bout, as well as means and sum
file        : file name
index       : bout #
to_onset    : beginning of torpor bout
to_len      : duration of torpor bout
tb_min      : minimum Tb
to_actv     : mean activity during torpor
ar_onset    : beginning of arousal bout
ar_len      : duration of arousal bout
tb_max      : maximum Tv
ar_actv     : mean activity during arousal
}


\author{
Thomas Ruf
}

\seealso{
bcounts
}


\examples{

if (interactive()) {
countbouts (thresh=25)
}

}


