\name{bcounts}
\alias{bcounts}

\title{
Bout counts
}
\description{
Determines torpor bout characteristics
}
\usage{
bcounts(data, name = "exm", thresh = 28, min_torpid = NULL, min_wake = NULL, t_res = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Selected data
}
  \item{name}{
Name in table
}
  \item{thresh}{
Threshold for torpor
}
  \item{min_torpid}{
Hibernation  onset: the minimum duration torpid (hours)
}
  \item{min_wake}{
Hibernation end: the minimum duration euthermic (hours)
}
  \item{t_res}{
Data temporal resolution in minutes
}
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

\examples{


rawdata=system.file("extdata","M65C6.csv",package="bouts") #Hibernation, no activity
hib=read_ib(rawdata)
bcounts(hib,"Example")

rawdata=system.file("extdata","TempAct_ID66.csv",package="bouts") #Torpor, with activity
hib=read_ib(rawdata)
bcounts(hib,"Example")
}
