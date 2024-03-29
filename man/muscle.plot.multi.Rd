\name{muscle.plot.multi}
\alias{muscle.plot.multi}
\title{
Plot multi muscle fibers with a muscle outline
}
\description{
Uses functions from rgl to plot all fibers (smoothed with splines) in a fiber list. Also uses grayscale values from the image stack to determine the external boundaries of the muscle based on concave hulls. Boundaries are plotted as a series of single outlines sampled across the image.

}
\usage{
muscle.plot.multi(fiber.list, images, df = 2, outline = 30, 
cols = NULL, mirror.axis = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fiber.list}{
A list of fibers with $fiber points. Generated by good.fibes or processed cleaned by other functions}
  \item{images}{
A character vector of image stack file names. Generated with list.files}
  \item{df}{
The degrees of freedom to pass to splines2::nsp for smoothing fiber paths. df = 1 gives a straight path, while >1 gives increasingly curved paths}
  \item{outline}{
The number of wireframe "outlines" to draw the muscle boundaries
}
  \item{cols}{
An optional vector of colors, the same order and length of fiber.list}
  \item{mirror.axis}{
Depending on the way the image stack was exported, fibers may be reflected from their original original. mirror.axis = TRUE will reflect the fibers before plotting to correct this
}
}
\value{
Returns a 3D plot
}

\references{
Arbour, J. In Prep. GoodFibes: an R package for the detection of muscle fibers from diceCT scans.
}
\author{
J. Arbour
}
\seealso{
\code{\link{muscle.plot}}, \code{\link{muscle.plot.stl}}
}
\examples{

\donttest{

olddir<-getwd()

#### this downloads the ant dataset image stack
#### if you have it already downloaded you can navigate to that folder
setwd(tempdir())
download.file(url=
"https://github.com/jessica-arbour/Ant-Muscle-Image-Stack/raw/main/Ant_data.zip",
destfile="antdata.zip")

unzip("antdata.zip")
setwd(paste0(getwd(),"/Ant data"))
####

images<-list.files(pattern=".png")
data(ant.final)
muscle.plot.multi(ant.final, images, df=1, mirror.axis = TRUE)


setwd(olddir)
}

}
