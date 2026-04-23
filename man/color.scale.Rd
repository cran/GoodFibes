\name{color.scale}
\alias{color.scale}
\title{
A simple wrapper to create colors for a continuous variable}
\description{
For any continuous variable like fiber length, produces a vector that can be used in plotting functions for the \emph{col} argument. Uses colorRampPalette.
}
\usage{
color.scale(fl, cols, min.fl=NULL, max.fl=NULL)
}
\arguments{
  \item{fl}{
A numeric vector containing a measurement for each fiber in a fiber list (length, curvature or angle).
}
  \item{cols}{
A character vector for determining the color gradient, in order from small to large values. Alternatively "viridis", "rainbow", "inferno" and "turbo" will supply these preset color gradients.
}
  \item{min.fl}{
A imposed lowest value of fl for the color scale. Can be used to make color legends equivalent across different plots.
}
\item{max.fl}{
A imposed highest value of fl for the color scale. Can be used to make color legends equivalent across different plots.
}
}
\value{
A vector with color values for each fiber}

\seealso{
\code{\link{muscle.plot.stl}}
}
\examples{
\donttest{
data(ant.final)
fl<-fiber.lengths(ant.final, res = 0.000673107, df=1)

cols<-color.scale(fl, c("blue", "green"))
muscle.plot.stl(ant.final, res = 0.000673107, cols = cols, mirror.axis = TRUE, df=1)
}
}