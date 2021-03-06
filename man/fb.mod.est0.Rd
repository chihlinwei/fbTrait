% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fb.mod.est0.R
\name{fb.mod.est0}
\alias{fb.mod.est0}
\title{Extract model estimates using url}
\usage{
fb.mod.est0(url)
}
\arguments{
\item{url}{A url address to a fishbase web page.}
}
\value{
A vector of model estimated properties, including:
\describe{
  \item{phylogenetic.diversity}{}
  \item{length.weight.a}{}
  \item{length.weight.a.min}{}
  \item{length.weight.a.max}{}
  \item{length.weight.b}{}
  \item{length.weight.b.min}{}
  \item{length.weight.b.max}{}
  \item{trophic.level}{}
  \item{trophic.level.se}{}
  \item{double.time.min}{}
  \item{double.time.max}{}
  \item{Vulnerability}{}
  \item{price}{}
}
}
\description{
Extract estimates of some fishbase properties based on models using url
}
\details{
This funtion extracts estimates of some properties based on models
}
\examples{
fb.mod.est0("http://www.fishbase.org/summary/SpeciesSummary.php?id=2593")

}
\author{
Chih-Lin Wei <chihlinwei@gmail.com>
}

