% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_data.R
\name{oes_peak_area}
\alias{oes_peak_area}
\title{Calculate peak area for set of ocena optics spectras}
\usage{
oes_peak_area(oes_mat, x.min, x.max, sub_min = TRUE)
}
\arguments{
\item{oes_mat}{matrix with data}

\item{x.min}{range min for peak area calculation}

\item{x.max}{range max for peak area calculation}

\item{sub_min}{TRUE subtract const background}
}
\value{
peak-area array
}
\description{
Calculate peak area for set of ocena optics spectras
}
\examples{
        \dontrun{oes_peak_area(oes_mat, xmin, xmax)}
}
\author{
K. Juraic
}
