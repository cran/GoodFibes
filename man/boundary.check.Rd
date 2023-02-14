\name{boundary.check}
\alias{boundary.check}
\title{
Check if muscle fiber is close to boundary of muscle}
\description{
An internal function that uses a concavehull to determine the bounds of the muscle based on grayscale values on each image and determines how many boundary voxels are within a set distance from the selected image point.
}
\usage{
boundary.check(images, new.zero.image, new.startx, new.starty, bound.buffer, cutoff)
}
\arguments{
  \item{images}{
A vector of image names in the working directory, created using list.files
}
  \item{new.zero.image}{
The image plane being considered
}
  \item{new.startx}{
The x coordinate of the considered point
}
  \item{new.starty}{
The y coordinate of the considered point
}
  \item{bound.buffer}{
The number of voxels a point can be distant from the boundary
}
  \item{cutoff}{
The grayscale value under which voxels will be treated as black
}
}
\details{
For internal use only
}
\value{
The number of boundary voxels within bound.buffer distance of the proposed point.
}
\author{
J. Arbour
}
\keyword{internal}
