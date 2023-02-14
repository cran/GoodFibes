\name{crop.stack}
\alias{crop.stack}
\title{
Automated imaging cropping
}
\description{
Automatically crops a stack of png image files to the minimum bounds of non-black values. Or if bounds are supplied can be cropped to the exact size of another stack of images.
}
\usage{
crop.stack(images, bounds = NULL, save.images=FALSE)
}
\arguments{
  \item{images}{
A vector with file names for the image files, can be created with list.files.
}
  \item{bounds}{
An optional vector with four values, given as c(xlim, xmax, ylim, ymax). These are printed at the end of the function when cropping is performed automatically.
}
  \item{save.images}{
When TRUE, images are saved to the current directory using imager::save.image.
}
}
\value{
The bounds to be used for cropping. Optionally crops and saves images to working folder.
}
\references{
Arbour, J. In Prep. GoodFibes: an R package for the detection of muscle fibers from diceCT scans.}
\author{
J. Arbour}
\seealso{
\code{\link{equalize.stack}}
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

crop.stack(images)

setwd(olddir)
}



}
