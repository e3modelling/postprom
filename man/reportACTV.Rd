% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reportACTV.R
\name{reportACTV}
\alias{reportACTV}
\title{Generate an Activity Report from GDX Data}
\usage{
reportACTV(path, regions, years)
}
\arguments{
\item{path}{The file path to the GDX data file.}

\item{regions}{A character vector of region names to filter data.}
}
\value{
A magclass object containing filtered activity data with assigned units.
}
\description{
This function reads activity data from a GDX file,
assigns appropriate unit mappings, and returns a structured `magclass` object.
}
\details{
- Reads `iActv` from the specified GDX file.
- Removes unwanted activity categories (`PG`, `H2P`, `H2INFR`).
- Maps corresponding unit labels to activity variables.
- Returns a magclass object with an added unit dimension.
}
\examples{
\dontrun{
reportACTV(path_gdx, c("MEA", "EU"))
}

}
