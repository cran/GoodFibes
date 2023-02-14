\name{color.scale}
\alias{color.scale}
\title{
A simple wrapper to create colors for a continuous variable}
\description{
For any continuous variable like fiber length, produces a vector that can be used in plotting functions for the \emph{col} argument. Uses colorRampPalette.
}
\usage{
color.scale(fl, color1, color2)
}
\arguments{
  \item{fl}{
A numeric vector containing a measurement for each fiber in a fiber list.
}
  \item{color1}{
A character value for the color for small values of the variable
}
  \item{color2}{
A character value for the color for large values of the variable
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

cols<-color.scale(fl, "blue", "green")
muscle.plot.stl(ant.final, res = 0.000673107, cols = cols, mirror.axis = TRUE, df=1)
}
}