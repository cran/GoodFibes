\name{hemisphere.points}
\alias{hemisphere.points}
\title{
Finding end points of possible paths
}
\description{
Internal function that projects a hemisphere or spherical dome through the image stack to determine the end voxels of each possible path from a seed point.
}
\usage{
hemisphere.points(radius, show.plot = FALSE, backstep = 0)
}
\arguments{
  \item{radius}{
The number of images out from the seed point plane to sample
}
  \item{show.plot}{
Generates a 3D plot showing the end points of possible paths
}
  \item{backstep}{
Should images in the seed point plane or behind it be considered. Each value (1, 2, 3, etc.) gives the number of images "behind" the seed point to consider in the spherical dome
}
}
\value{
Returns a matrix with 3D coordinates of the end points of the possible paths
}
\author{
J. Arbour
}

\seealso{
\code{\link{points.generator}}
}

\keyword{internal}

