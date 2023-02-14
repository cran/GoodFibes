\name{fuse.fibers.auto}
\alias{fuse.fibers.auto}
\title{
Automated repeated fusing of fiber paths}
\description{
This function repeatedly compares pairs of muscle fibers for fusing. Repeated applications of fuse.fibers until no further fibers can be merged}
\usage{
fuse.fibers.auto(fiber.list, min.vox, min.improvement = 0.25, 
df = 2, length.out = 50, max.iter = 10, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fiber.list}{
A list of fibers with $fiber.points produced by good.fibes or any of the processing and cleaning functions.
}
  \item{min.vox}{
The voxel distance below which fibers will be compared. Should be <= the voxel width of the muscle fascicles, though lower if interstital spaces are low.
}
  \item{min.improvement}{
The minimum increase (as a proportion) in fiber length for fibers to be worth merging.
}
  \item{df}{
The df to be used in smoothing fiber paths in the calculation of fiber length
}
  \item{length.out}{
The number of divisions to be used in the calculation of fiber lengths (line segments)
}
  \item{max.iter}{
The maximum number of iterations of fuse.fibers that will be attempted.
}
  \item{verbose}{
Should the number of iterations through the function be displayed while running?
}
}
\value{
A list of fibers with $fiber.points
}
\references{
Arbour, J. In Prep. GoodFibes: an R package for the detection of muscle fibers from diceCT scans
}
\author{
J. Arbour
}
\seealso{
\code{\link{fuse.fibers}}
}
