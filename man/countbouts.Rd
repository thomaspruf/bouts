\name{countbouts}
\alias{countbouts}
\title{
Select hibernation and compute bouts
}
\description{
Does it all
1. Pick data file
2. Pick start and end using mouse
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

