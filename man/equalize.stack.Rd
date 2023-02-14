\name{equalize.stack}
\alias{equalize.stack}
\title{
Automated histogram equalization of image state
}
\description{
Conducts histogram equalization to adjust the contrast of the image stack. May improve visibility of muscle fibers prior to fiber detection. Optionally automatically save new image stack in working directory.
}
\usage{
equalize.stack(images, n, save.images = FALSE)
}
\arguments{
  \item{images}{
A vector of png image file names, created using list.files
}
  \item{n}{
The number of the image in the stack to be equalized and plotted
}
  \item{save.images}{
Should the whole image stack be equalized and plotted?
}
}
\value{
Creates a plot and optionally saves an image stack
}
\references{
Arbour, J. In Prep. GoodFibes: an R package for the detection of muscle fibers from diceCT scans}
\author{
J. Arbour
}
\seealso{
\code{\link{crop.stack}}
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
equalize.stack(images, 100)  


setwd(olddir)
}
}
