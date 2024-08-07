% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coord.r
\name{Coord1D}
\alias{Coord1D}
\title{1D coordinate vector R6 Class}
\description{
\code{Coord1D} is an \code{\link[R6:R6Class]{R6::R6Class()}} object representing two-dimensional points
represented by Cartesian Coordinates.
}
\examples{
p <- as_coord1d(x = rnorm(100, 2))
print(p, n = 10L)
pc <- mean(p) # Centroid
# method chained affine transformation matrices are auto-pre-multiplied
p$
  translate(-pc)$
  reflect("origin")$
  print(n = 10L)
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{xw}}{A two-column matrix representing the homogeneous coordinates.
The first column is the "x" coordinates
and the second column is all ones.}

\item{\code{x}}{A numeric vector of x-coordinates.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Coord1D-new}{\code{Coord1D$new()}}
\item \href{#method-Coord1D-print}{\code{Coord1D$print()}}
\item \href{#method-Coord1D-project}{\code{Coord1D$project()}}
\item \href{#method-Coord1D-reflect}{\code{Coord1D$reflect()}}
\item \href{#method-Coord1D-scale}{\code{Coord1D$scale()}}
\item \href{#method-Coord1D-translate}{\code{Coord1D$translate()}}
\item \href{#method-Coord1D-transform}{\code{Coord1D$transform()}}
\item \href{#method-Coord1D-clone}{\code{Coord1D$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Coord1D-new"></a>}}
\if{latex}{\out{\hypertarget{method-Coord1D-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Coord1D$new(xw)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{xw}}{A matrix with three columns representing (homogeneous) coordinates.
The first column represents x coordinates and
the last column is all ones.
Column names should be "x" and "w".}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Coord1D-print"></a>}}
\if{latex}{\out{\hypertarget{method-Coord1D-print}{}}}
\subsection{Method \code{print()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Coord1D$print(n = NULL, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{n}}{Number of coordinates to print. If \code{NULL} print all of them.}

\item{\code{...}}{Passed to \code{\link[=format.default]{format.default()}}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Coord1D-project"></a>}}
\if{latex}{\out{\hypertarget{method-Coord1D-project}{}}}
\subsection{Method \code{project()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Coord1D$project(point = as_point1d("origin"), ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{point}}{A \link{Point1D} object of length one representing the point
you with to reflect across or project to or an object coercible to one by \code{as_point1d(point, ...)}
such as "origin".}

\item{\code{...}}{Passed to \code{\link[=project1d]{project1d()}}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Coord1D-reflect"></a>}}
\if{latex}{\out{\hypertarget{method-Coord1D-reflect}{}}}
\subsection{Method \code{reflect()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Coord1D$reflect(point = as_point1d("origin"), ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{point}}{A \link{Point1D} object of length one representing the point
you with to reflect across or project to or an object coercible to one by \code{as_point1d(point, ...)}
such as "origin".}

\item{\code{...}}{Passed to \code{\link[=reflect1d]{reflect1d()}}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Coord1D-scale"></a>}}
\if{latex}{\out{\hypertarget{method-Coord1D-scale}{}}}
\subsection{Method \code{scale()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Coord1D$scale(x_scale = 1)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{x_scale}}{Scaling factor to apply to x coordinates}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Coord1D-translate"></a>}}
\if{latex}{\out{\hypertarget{method-Coord1D-translate}{}}}
\subsection{Method \code{translate()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Coord1D$translate(x = as_coord1d(0), ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{x}}{A \link{Coord1D} object of length one or an object coercible to one by \code{as_coord1d(x, ...)}.}

\item{\code{...}}{Passed to \code{as_coord1d(x, ...)} if \code{x} is not a \link{Coord1D} object}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Coord1D-transform"></a>}}
\if{latex}{\out{\hypertarget{method-Coord1D-transform}{}}}
\subsection{Method \code{transform()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Coord1D$transform(mat = transform1d())}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{mat}}{A 2x2 matrix representing a post-multiplied affine transformation matrix.
The last \strong{column} must be equal to \code{c(0, 1)}.
If the last \strong{row} is \code{c(0, 1)} you may need to transpose it
to convert it from a pre-multiplied affine transformation matrix to a post-multiplied one.
If a 1x1 matrix we'll quietly add a final column/row equal to \code{c(0, 1)}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Coord1D-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Coord1D-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Coord1D$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
