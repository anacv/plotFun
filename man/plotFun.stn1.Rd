% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotFun.stn1.R
\name{plotFun.stn1}
\alias{plotFun.stn1}
\title{Plot stations (single plot).}
\usage{
plotFun.stn1(x, lon, lat, xlims, ylims, breaks = breaks,
  palette = palette, cex.marker, cex.main, title.main, axis.lab,
  mark.border = mark.border, text = NULL, cex.text = 1,
  map.fill = FALSE, map.col = "grey89")
}
\arguments{
\item{x}{vector to be plotted.}

\item{lon}{vector of longitudes. It must have the same length as x.}

\item{lat}{vector of latitudes. It must have the same length as x.}

\item{xlims}{2-element vector defining the minimum and maximum longitud for the plotting area.}

\item{ylims}{2-element vector defining the minimum and maximum latitude for the plotting area.}

\item{breaks}{vector of values defining the intervals to be used in the colorbar.}

\item{palette}{character vector with the colors for the plot. They will be interpolated to match the number of intervals defined by breaks.}

\item{cex.marker}{numeric value giving the marker expansion factor. Default: 1.}

\item{cex.main}{numeric value giving the title expansion factor. Default: 1.}

\item{title.main}{character string with the title.}

\item{axis.lab}{display or not lon/lat ticks and labels. Default=FALSE}

\item{mark.border}{logical. If black contour for the cicles is plotted. Default=TRUE.}

\item{text}{display some text in each point. Length should be the same as lat and lon.}

\item{cex.text}{numeric value giving the text expansion factor. Default: 1.}

\item{map.fill}{logical. If map is filled with colour in `map.col'. Default: FALSE}

\item{map.col}{Colour for the map (continent). Default: grey89.}
}
\description{
Plot a figure based on point stations, for a single plot.
}
\author{
Ana Casanueva (16.02.2017)
}
