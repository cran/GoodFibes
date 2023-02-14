\name{thresholdPlot}
\alias{thresholdPlot}
\title{
Plot image from diceCT stack using thresholding
}
\description{
Plot a selected image from the image stack with values below \emph{threshold} set to black (grayscale = 0). Can be used to select \emph{threshold} and \emph{cutoff} values used in good.fibes.
}
\usage{
thresholdPlot(images, n, threshold)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{images}{
A character vector of image stack file names. Generated with list.files
}
  \item{n}{
The number of the selected image in the vector "images"
}
  \item{threshold}{
The cutoff value for grayscale values. All voxels with grayscales below threshold will be displayed as black.
}
}
\value{
Returns a plot
}

\references{
Arbour, J. In Prep. GoodFibes: an R package for the detection of muscle fibers from diceCT scans.
}
\author{
J. Arbour
}
\seealso{
\code{\link{sequencePlot}}
}
\examples{

images <- dir(system.file("extdata", package = "GoodFibes"), ".png", full.names = TRUE)

thresholdPlot(images, 1, 0.3)
thresholdPlot(images, 1, 0.4)
thresholdPlot(images, 1, 0.5)

}