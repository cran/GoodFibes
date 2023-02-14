\name{ant.raw}
\alias{ant.raw}
\docType{data}
\title{
Raw ant muscle fibers
}
\description{
Preliminary muscle fibers detected from the mandibular muscle of an ant \emph{(Monomorium pharaonis)}. No quality checking or processing.
}
\usage{data("ant.raw")}
\format{
A list with 247 entries, each representing a muscle fiber reconstructed from an image stack (dataset available in examples below) and a partial, subsampled stack is available in extdata. Each entry contains $fiber.points, the raw reconstructed fiber paths, and $fiber.smoothed, the smoothed paths. }
\details{
These are the initial possible fibers detected using \code{\link{good.fibes}}. Also see \emph{ant.final} for the processed fibers.
}
\source{
Fiber paths were generated from the ant dataset in Katzke et al (2022). Original image files are stored under extdata (and see example).
}
\references{
Katzke, J., Puchenkov, P., Stark, H., and Economo, E. 2022. A Roadmap to Reconstructing Muscle Architecture from CT Data. \emph{Integrative Organismal Biology} 4(1): 1-16.
}
\examples{

\donttest{
data(ant.raw)
fl<-fiber.lengths(ant.raw, res = 0.000673107, df = 1)

#### this downloads the ant dataset image stack to a temp folder
olddir<-getwd()
setwd(tempdir())
download.file(url=
"https://github.com/jessica-arbour/Ant-Muscle-Image-Stack/raw/main/Ant_data.zip",
destfile="antdata.zip")

unzip("antdata.zip")
setwd(paste0(getwd(),"/Ant data"))

setwd(olddir)
}
}
\keyword{datasets}
