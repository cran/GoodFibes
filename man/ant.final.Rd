\name{ant.final}
\alias{ant.final}
\docType{data}
\title{
Ant muscle fibers finalized
}
\description{
Muscle fibers reconstructed from the mandibular muscle of an ant \emph{(Monomorium pharaonis)} (post processing).
}
\format{
A list with 198 entries, each representing a muscle fiber reconstructed from an image stack. Each entry contains $fiber.points, the raw reconstructed fiber paths, and $fiber.smoothed, the smoothed paths. 
}
\details{
These fibers were reconstructed using \emph{good.fibes}, checked for poor fiber paths using \emph{quality.check}, and had fibers fused and merged using \emph{fuse.fibers} and \emph{check.overlap}. Also see "ant.raw" for the initial fiber paths from good.fibes.
}
\source{
Fiber paths were generated from the ant dataset in Katzke et al (2022). Original image files available upon request.
}
\references{
Katzke, J., Puchenkov, P., Stark, H., and Economo, E. 2022. A Roadmap to Reconstructing Muscle Architecture from CT Data. \emph{Integrative Organismal Biology} 4(1): 1-16.
}
\examples{
\donttest{
data(ant.final)
fl<-fiber.lengths(ant.final, res = 0.000673107, df = 1)
}

}
\keyword{datasets}
