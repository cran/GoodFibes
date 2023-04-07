\name{muscle.plot}
\alias{muscle.plot}
\title{
Plot a single muscle fiber}
\description{
Used to compare the muscle fiber path to the smoothed muscle fiber. Plots a single set of $fiber.points from and the smoothed fibers.
}
\usage{
muscle.plot(fiber.dat, images, df = 4, mirror.axis = FALSE, outline = 50, size = 2)
}
\arguments{
  \item{fiber.dat}{
Any set of $fiber.points produced by good.fibes
}
  \item{images}{
A character vector with image names representing the image stack, can be produced using list.files.
}
  \item{df}{
The df to be used in smoothing fiber paths in the calculation of fiber length
}
  \item{mirror.axis}{
Depending on the way the image stack was exported, fibers may be reflected from their original original. mirror.axis = TRUE will reflect the fibers before plotting to correct this
}
  \item{outline}{
The number of wireframe "outlines" to draw the muscle boundaries
}
  \item{size}{
point size for $fiber.points in plot
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
\code{\link{muscle.plot.multi}}, \code{\link{muscle.plot.stl}}
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
muscle.plot(ant.final[[100]]$fiber.points,images,df=1, outline=30, mirror.axis=TRUE)
setwd(olddir)
}
}
