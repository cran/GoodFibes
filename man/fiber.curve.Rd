\name{fiber.curve}
\alias{fiber.curve}
\title{
Calculate the relative curvature of muscle fibers
}
\description{
Calculates a metric for fiber curvature. This is the ratio between the total length of the curved smoothed fiber, to the straight line distance between the end points of the fiber. A straight fiber will have a curvature value of ~ 1 (small differences may be due to the calculation of fiber length across a smoothed curve), and values > 1 represent more curvature.

Optionally identified which fibers show unusual curvature (are outliers), for possible removal.
}
\usage{
fiber.curve(fib.list, df, check = TRUE)
}
\arguments{
  \item{fib.list}{
A list of fibers containing $fiber.points. Produced by good.fibes or the various cleaning functions (quality check, fuse.fibers, check.overlap)}
  \item{df}{
Corresponds to the df argument in splines::ns. Determines the shape of the smoothing spline (df = 1 represents straight muscle fibers)}
  \item{check}{
Should unusually curved fibers be identified?}
}
\value{

\item{curvature}{The ratio of fiber length to end-to-end length}
\item{problem.fibers}{Fibers with unusually high curvature. Given as the index of these fibers in the original list.}
}
\references{
Arbour, J. In Prep. GoodFibes: an R package for the detection of muscle fibers from diceCT scans.
}
\author{
J. Arbour
}
\seealso{
\code{\link{fiber.lengths}},\code{\link{fiber.angle}}
}
\examples{

data(ant.final)

\donttest{fcr<-fiber.curve(ant.final,df=2,check=TRUE) 
#fibers reconstructed with a curve here merely to demonstrate function
#ant fibers were fairly straight

sort(fcr$curvature) 
#all fibers are close to 1 even with a "curved" reconstruction}
}
