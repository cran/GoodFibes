\name{fiber.lengths}
\alias{fiber.lengths}
\title{
Calculated the length of smoothed muscle fibers}
\description{
Determines the length of reconstructed and smoothed muscle fibers. Fibers are smoothed using splines::ns and then oversampled (length.out). The sum of all straight line segments on the smoothed paths is taken as the overall fiber length}
\usage{
fiber.lengths(fib.list, res = NULL, df = 2, length.out = 500)
}
\arguments{
  \item{fib.list}{
A list of fibers containing $fiber.points. Produced by good.fibes or the various cleaning functions (quality check, fuse.fibers, check.overlap)}
  \item{res}{
The resolution of the isometric voxels (e.g., linear dimension)}
  \item{df}{
The degrees of freedom passed to splines::ns. A df = 1 produces a straight fiber, while values >1 allow fibers to curve.}
  \item{length.out}{
The number of straight line segments that the smoothed curve will be divided into for calculation of length
}
}
\value{
A vector with fiber lengths
}
\references{
Arbour, J. In Prep. GoodFibes: an R package for the detection of muscle fibers from diceCT scans.
}
\author{
J. Arbour}

\seealso{
\code{\link{fiber.angle}},\code{\link{fiber.curve}}
}
\examples{

data(ant.final)

\donttest{
fl<-fiber.lengths(ant.final, res = 0.000673107, df=1)
mean(fl)
}
}