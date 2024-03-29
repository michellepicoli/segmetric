% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{general_functions}
\alias{general_functions}
\alias{sm_area}
\alias{sm_centroid}
\alias{sm_distance}
\alias{sm_intersection}
\alias{sm_subset_union}
\alias{sm_rbind}
\alias{sm_apply_group}
\alias{sm_summarize_group}
\alias{sm_norm_left}
\alias{sm_norm_right}
\alias{sm_options}
\title{General functions}
\usage{
sm_area(s, order = NULL)

sm_centroid(s, order = NULL)

sm_distance(s1, s2)

sm_intersection(s1, s2, touches = TRUE)

sm_subset_union(s)

sm_rbind(...)

sm_apply_group(x, groups, fn, ...)

sm_summarize_group(x, groups, fn, ...)

sm_norm_left(x, y)

sm_norm_right(x, y)

sm_options(..., digits = NULL)
}
\arguments{
\item{s, s1, s2}{Either a \code{ref_sf}, a \code{seg_sf}, or a \code{subset_sf} object
(inherited from \code{sf}).}

\item{order}{A \code{subset_sf}. This argument arranges the returned values
according to the object passed here.}

\item{touches}{A \code{logical}. Is the border part of the intersection?}

\item{...}{For \code{sm_rbind()}, a set of \code{subset_sf} objects to be
merged.}

\item{x, y}{A \code{numeric} values (e.g. areas, lengths) to compute metrics.}

\item{groups}{A group identifier vector used to aggregate a metric
for each group.}

\item{fn}{A \code{function} to aggregate a metric for a group.}

\item{digits}{An \code{integer} indicating how many digits used to round
metric values.}
}
\value{
\itemize{
\item \code{sm_area()}: Return a \code{numeric} vector with polygons' area.
\item \code{sm_centroid()}: Return a \code{subset_sf} object with polygons' centroid.
\item \code{sm_intersection()}: Return a \code{subset_sf} object with intersection
between polygons.
\item \code{sm_subset_union()}: Return a \code{subset_sf} object with union
between intersecting polygons.
\item \code{sm_rbind()}: Return a \code{subset_sf} object with unique features.
}
}
\description{
These functions manipulate segmetric objects.
\itemize{
\item \code{sm_area()}: Return a vector of areas, one for each polygon.
\item \code{sm_centroid()}: Return the centroids of the given polygons.
\item \code{sm_intersection()}: Return the intersection of the given simple features.
\item \code{sm_subset_union()}: Return the union of the given simple features.
\item \code{sm_rbind()}: Return the merge of unique simple features.
\item \code{sm_summarize_groups()}: Summarizes metric values by a group
(either reference or segment).
\item \code{sm_options()}: Get/Set segmetric options.
}
}
