\name{read_butt}
\alias{read_butt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reads ibutton files
}
\description{
Attempts to read files generated from ibutton data or from FIWI logger data. Various date formats ar allowed. See countbouts.
}
\usage{
read_butt(name)
}
\arguments{
  \item{name}{
Selected file
}
}
\value{
Returns data frame with columns Date.Time or Date, Hours (since start), Temp (temperature in °C), and Act (activity for certain FIWI loggers). Formats are POSIXct, double, double
}

\author{
Thomas Ruf
}



%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
bcounts
}
\examples{

rawdata=system.file("extdata","TempAct_ID66.csv",package="bouts")
hib=read_butt(rawdata)
plot(hib$Hours,hib$Temp,type="l",xlab="Hours",ylab="Body Temperature (C)")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{ IO }
\keyword{ file }

