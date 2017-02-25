#' Extract FAO using SpecCode
#'
#' Extract FAO from fishbase using SpecCode
#'
#' @param SpecCode A vector of fishbase SpecCode.
#' @details This funtion extracts FAO information from fishbase using SpecCode
#' @return A matrix of FAO information, including:
#' \describe{
#'   \item{SpecCode}{}
#'   \item{no.of.fao}{}
#'   \item{fao.area}{}
#' }
#' @author Chih-Lin Wei <chihlinwei@@gmail.com>
#' @export
#' @examples
#' fb.FAO(c(69, 309, 2420, 3272))
#'
fb.FAO <-
  function(SpecCode){
    server <- "http://www.fishbase.org/Country/FaoAreaList.php?ID="
    url <- paste(server, SpecCode, sep = "")
    fao.area <-NULL; no.of.fao <- NULL
    for(i in 1:length(SpecCode)){
      tt <- getURL(url[i], followlocation=TRUE)
      doc <- htmlParse(tt)
      tab <- readHTMLTable(doc)
      if(is.null(tab$dataTable)){
        n <- NA; area <- NA
      } else {
        n <- nrow(tab$dataTable)
        area <- paste(tab$dataTable[,1], collapse = "; ")
      }
      if(length(n)!=1) print(paste("Check SpecCode", SpecCode[i])) # which i has problem
      no.of.fao <- c(no.of.fao, n)
      fao.area <- c(fao.area, area)
    }
    out <- cbind(SpecCode, no.of.fao, fao.area)
    return(out)
  }
