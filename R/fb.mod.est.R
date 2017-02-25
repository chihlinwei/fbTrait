#' Extract model estimates using SpecCode
#'
#' Extract estimates of some fishbase properties based on models using SpecCode
#'
#' @param SpecCode A vector of fishbase SpecCode.
#' @details This funtion extracts estimates of some properties based on models using SpecCode
#' @return A vector of model estimated properties, including:
#' \describe{
#'   \item{phylogenetic.diversity}{}
#'   \item{length.weight.a}{}
#'   \item{length.weight.a.min}{}
#'   \item{length.weight.a.max}{}
#'   \item{length.weight.b}{}
#'   \item{length.weight.b.min}{}
#'   \item{length.weight.b.max}{}
#'   \item{trophic.level}{}
#'   \item{trophic.level.se}{}
#'   \item{double.time.min}{}
#'   \item{double.time.max}{}
#'   \item{Vulnerability}{}
#'   \item{price}{}
#' }
#' @author Chih-Lin Wei <chihlinwei@@gmail.com>
#' @export
#' @examples
#' fb.mod.est(c(69, 309, 2420, 3272))
#'

fb.mod.est <-
  function(SpecCode){
    server <- "http://www.fishbase.org/summary/"
    url <- paste(server, SpecCode, sep = "")
    out <- NULL
    for(i in 1:length(SpecCode)){
      trait <- fb.mod.est0(url[i])
      if(length(trait)!=13) print(paste("Check SpecCode", SpecCode[i])) # which i has problem
      out <- rbind(out, trait)
    }
    out <- cbind(SpecCode, out)
    row.names(out) <- NULL
    return(out)
  }
