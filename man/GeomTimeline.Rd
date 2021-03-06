% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/earthquakes.R
\docType{data}
\name{GeomTimeline}
\alias{GeomTimeline}
\title{GeomTimeline - A new class that inherits from the GeomPoint and shows Earthquake timeline}
\format{
An object of class \code{GeomTimeline} (inherits from \code{GeomPoint}, \code{Geom}, \code{ggproto}, \code{gg}) of length 5.
}
\usage{
GeomTimeline
}
\description{
Required Aesthetics:
x The DATE when the earthquake occurs
xmin The MINIMUM date for the earthquake timeline
xmax The MAXIMUM date for the earthquake timeline
}
\details{
Default Aesthetics:
y A factor indicating some stratification in which case multiple timelines will be plotted
  for each level of the factor (e.g. country).
colour The border color for point. Default is GREEN
fill The fill color for point. Default is RED.
size The size of the point. Default is 2
shape The shape of the point. Default is 2
alpha The transparency value for fill color. Default is 0.6
draw_key The key value for legend. Default is ggplor2::draw_key_point
draw_panel The function that returns the PointsGrob based on data passed
}
\keyword{datasets}
