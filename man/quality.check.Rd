\name{quality.check}
\alias{quality.check}
\title{
Quality testing of possible muscle fibers detected by good.fibes}
\description{
Calculates quality as the ratio of grayscale standard deviation and fiber length for each muscle fiber detected using good.fibes. Long, homogenous fibers are considered to be of higher quality. Fibers are smoothed before the calculation of fiber quality.

Fibers with usually low quality (high grayscale variation compared to fiber length) are identified for exclusion.

}
\usage{
quality.check(fib.list, images, res, min.length = NULL, length.out = 200, df = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fib.list}{
A list of muscle fibers with $fiber.points, generated by good.fibes
}
  \item{images}{
A character vector of image stack file names. Generated with list.files
}
  \item{res}{
The isometric resolution of the voxels (i.e., the distance between images). Given as a linear measure (um, mm, etc.)
}
  \item{min.length}{
Optionally exclude fibers below a certain fiber length (e.g., based on anatomical measurements). If resolution is given, then in those units, otherwise in number of voxels.
}
  \item{length.out}{
Number of line segments used in the calculation of fiber length
}
  \item{df}{
Degrees of freedom passed to splines::ns in the smoothing of muscle fibers before calculation. df = 1 produces straight fibers, while values > 1 produce increasingly curved fibers
}
}
\value{
\item{quality}{grayscale sd/fiber length, low values are considered of higher quality}
\item{grey.values}{A list providing the grayscale values for each smoothed fibers}
\item{problem.fibers}{The location of fibers in the original list object that have atypically poor quality and should be excluded from further analyses}
}
\references{
Arbour, J. In Prep. GoodFibes: an R package for the detection of muscle fibers from diceCT scans.

Puffel, F. Pouget, A., Liu, X., Zuber, M., van de Kamp, T., Roces, F., and Labonte, D., 2021. \emph{Journal of the Royal Society Interface} 18: 20210424
}
\author{
J. Arbour
}
\note{
Also produces a plot showing the distribution of quality values, and numbered bars for outliers.
}
\seealso{
\code{\link{fuse.fibers}},\code{\link{check.overlap}},
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

data(ant.raw)
images<-list.files(pattern=".png")

qc<-quality.check(ant.raw[21:50],images, res=0.000673107, df=1)


setwd(olddir)
}

}