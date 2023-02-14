\name{fuse.fibers}
\alias{fuse.fibers}
\title{
Fuse incomplete fibers}
\description{
This function compares fibers that pass between a minimum number of voxels and determines if merging them into a single fiber produces a well supported path}
\usage{
fuse.fibers(fiber.list, min.vox, min.improvement = 0.25, df = 2, length.out = 100)
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
}
\details{
This function compares pairs of fibers if they come within mix.vox of each other along their path. The fibers will be merged if 1) the mean 3D residual from the new spline through the combined fiber path is less than the mean residual from the two separate fiber paths, and 2) the fiber length of the combined fiber is at least min.improvement (proportionately) greater than the previous fiber lengths.
}
\value{
\item{merged.fibers}{A list of fibers with $fiber.points, with fibers combined based on above thresholds}
\item{fibers.to.merge}{a matrix of pairs of fibers that were merged}
}
\references{
Arbour, J. In Prep. GoodFibes: an R package for the detection of muscle fibers from diceCT scans
}
\author{
J. Arbour
}
\seealso{
\code{\link{quality.check}}, \code{\link{check.overlap}}
}
