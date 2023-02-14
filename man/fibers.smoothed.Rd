\name{fibers.smoothed}
\alias{fibers.smoothed}
\title{
Smoothing of estimated fiber paths}
\description{
Applies splines to smooth the stepwise fiber paths produced by good.fibes, with the function \emph{ns} from \strong{splines}.
}
\usage{
fibers.smoothed(fib.list, df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fib.list}{
A list of fibers with $fiber.points, produced by \code{\link{good.fibes}} or one of the cleaning and processing functions (e.g., fuse.fibers).
}
  \item{df}{
The degrees of freedom passed to splines::ns. Knots equal to df - 1 - intercept are set as breakpoints in the spline curve. A straight line path has a df of 1.
}
}
\value{
\item{fiber.points}{The original fiber path from good.fibes}
\item{fiber.smoothed}{The curved, smoothed fiber paths}
}
\references{
Arbour, J. In Prep. GoodFibes: an R package for the detection of muscle fibers from diceCT scans.
}
\author{
J. Arbour}
\seealso{
\code{\link{good.fibes}}
}
