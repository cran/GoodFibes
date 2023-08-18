\name{good.fibes}
\alias{good.fibes}
\title{
Automated detection of muscle fibers from diceCT scans
}
\description{
The function good.fibes uses textural analysis to determine the path of muscle fibers/fascicles through an image stack from a iodine contrast CT scan. Fiber paths are reconstructed using a stepwise algorithm that follows paths of low variation in threshold values. See details for full description of the method

\strong{SOMETIMES ABORTS RSTUDIO BUT STILL RUNS IN R-GUI, PROBLEM IS ONLY IN RSTUDIO}
}
\usage{
good.fibes(images, zero.image, radius, threshold = NULL, 
cutoff, scaler = 1, blackcut = 0.95, seeds = 1, show.plot = TRUE, 
start.seed = NULL, allowed.black = 0, bound.buffer = 0, backstep = 0, verbose=TRUE)
}
\arguments{
  \item{images}{
A character vector with image names representing the image stack from a diceCT scan. The voxels are assumed to be isometric, and the images should be in .png format. The vector can be produced using list.files(pattern = ".png")
}
  \item{zero.image}{
The number of the image in the stack from which seed points should be drawn. Only one image can be selected.
}
  \item{radius}{
The number of images to consider forward or backward from the zero.image at each step of the walk. Maximum 11.
}
  \item{threshold}{
The grayscale value below which voxels will be considered black for the selection of seed points. Must be equal to or greater than cutoff}
  \item{cutoff}{
The grayscale value below which voxels will be considered black in the forwards and backwards walk. Use \code{\link{thresholdPlot}} to determine a value that isolates muscle voxels from other tissues/background noise.
}
  \item{scaler}{
Exponential scaler for the trajectory penalization. Default is 1. At a value of 0 there is no trajectory penalization
}
  \item{blackcut}{
A termination condition. If a specified percentage (as proportion, e.g., 0.95) of voxels in the hemisphere of paths are black, the algorithm will terminate. 
}
  \item{seeds}{
The number of seed points on the starting image. The seed points will generate a possible fiber path, if a walk is possible (seeds can fail if they are located on noise without possible paths).
}
  \item{show.plot}{
Optionally show the location of the tracker in the image stack at each step
}
  \item{start.seed}{
Optionally applies \emph{set.seed} in the tracker to make results reproducible from one run to the next. See set.seed for more details.
}
  \item{allowed.black}{
For noisy datasets, allows this number of voxels with grayscale values below \emph{cutoff} to be included in the possible paths without terminating the algorithm
}
  \item{bound.buffer}{
If a fiber path is within this many voxels distance of the outermost boundary of the muscle, as determined by grayscale values above the \emph{cutoff}, then the algorithm will terminate the particular path. See details.
}
  \item{backstep}{
How many images "behind" the current plane should be considered. Should be kept to low values (1-3). When backstep = 0, only paths ending on images "ahead" of the image plane will be considered. \strong{EXPERIMENTAL}, will create some weird paths. Use only if the muscle fibers definitely arc back through the image stack and perhaps only on image planes close to that point.
}
  \item{verbose}{
If TRUE will list the progress through each fiber}

}
\details{
The function begins by selecting a set of seed points from the selected image. Grayscale values below \emph{threshold} are excluded, and the pairwise euclidean distances among all remaining voxels are calculated. Cluster analysis is conducted using \emph{hclust} and a set of groups equal to \emph{seeds} are produced using \emph{cutree}. Voxels within each group are randomly selected.

From a selected seed voxel, a hemisphere of possible paths is projected, extending \emph{radius} images from the selected starting image. If \emph{backstep} is >0, paths within the starting image plane and behind the plane (1 = 1 image behind, 2 = 2 images behind) are also included in the possible paths. \strong{NOTE} backstep is experimental and does cause more circuitous paths, use only if fiber paths reverse direction through the image stack at some point.

The forward walk from the seed point begins by choosing from the set of possible paths, the one that minimizes the following function.

diagnostic value = scaled grayscale SD * trajectory ^ scaler 

1)The scaled grayscale SD is the standard deviation of grayscale values along each possible path. This value is scaled to 0 to 1. 
2)The trajectory is the straight line distance between the end points on the hemisphere between the previous step and the next possible steps. This value is scaled to a range of 0 to 1, and added to 1 (resulting values range from 1 to 2). This is to penalize steps that make severe changes, as muscle fibers tend to not have very severe bends. 
3)The impact of the trajectory penalization is scaled using \emph{scaler}. If scaler = 0, there is no trajectory penalization.

The path with the minimum diagnostic value is selected, and the process repeated from the end point of that path. This stepwise algorithm continues the forward walk through the image stack until one of several stop conditions is reached:

1) The only available remaining paths would either terminate or cross a black voxel. This prevents the tracker from passing out of the muscle fascicle. To accommodate noisy datasets, the tracker may be permitted to cross a small number of black voxels (\emph{allowed.black}). 
2) The number of black voxels in the possible paths exceeds a specified number (e.g., 95 percent). This is meant to isolate regions of noise towards the end of a muscle fascicle, where adjoining connective tissue may obscure the end of a fiber. 
3) The remaining paths would terminate within a specified distance of the external "boundary" of the muscle. This prevents fibers from continuing to track along the exterior surface of the muscle in noisy image stacks.

Once the path is terminated, the algorithm returns to the seed point and begins a walk in the opposite direction. It proceeds using the same terms as above. The forwards and backwards walks are returned. The process repeats for the next seed point.

}
\value{
A list with a length equal to or less than seeds (failed paths will be dropped). Each element contains $fiber.points, the 3D coordinates providing the fiber path through the image stack, expressed in units of voxels.

Can be combined with separate runs from other images planes using c(). See example
}
\references{
Arbour, J. In Prep. GoodFibes: an R package for the detection of muscle fibers from diceCT scans.
}
\author{
J. Arbour
}
\seealso{
\code{\link{fibers.smoothed}}
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

fibes1<-good.fibes(images = images, zero.image = 200, radius = 9, threshold = 0.7,
cutoff = 0.65, seeds=5, start.seed = 1, show.plot=FALSE)

fibes2<-good.fibes(images = images, zero.image = 300, radius = 9, threshold = 0.7,
cutoff = 0.65, seeds=5, start.seed = 1, show.plot=FALSE)

fibes<-c(fibes1,fibes2)

muscle.plot.multi(fibes, images, df=1)


setwd(olddir)
}

}