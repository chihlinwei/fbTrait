#' Extract diet using SpecCode
#'
#' Extract diet from fishbase using SpecCode
#'
#' @param SpecCode A vector of fishbase SpecCode.
#' @details This funtion extracts diet information from fishbase using SpecCode
#' @return A matrix of diet, including:
#' \describe{
#'   \item{SpecCode}{}
#'   \item{no.of.diet}{}
#'   \item{main.food}{}
#' }
#' @author Chih-Lin Wei <chihlinwei@@gmail.com>
#' @export
#' @examples
#' fb.diet(c(69, 309, 2420, 3272))
#'
fb.diet <-
  function(SpecCode){
    server <- "http://www.fishbase.org/TrophicEco/DietCompoList.php?ID="
    url <- paste(server, SpecCode, sep = "")
    main.food <-NULL; no.of.diet <- NULL
    for(i in 1:length(SpecCode)){
      tt <- getURL(url[i], followlocation=TRUE)
      doc <- htmlParse(tt)
      tab <- readHTMLTable(doc)
      if(is.null(tab$dataTable)){
        n <- NA; summ2 <- NA
      } else {
        diet <- tab$dataTable[, 1]
        percent <- as.numeric(as.character(tab$dataTable[, 2]))
        total <- sum(percent, na.rm = TRUE)
        summ1 <- sort(round(tapply(percent, diet, sum, na.rm = TRUE)/total*100, 1), decreasing = TRUE)
        summ2 <- paste(paste(summ1, "%", " ", names(summ1), sep = ""), collapse = ", ")
        n <- length(summ1)

      }
      if(length(n)!=1) print(paste("Check SpecCode", SpecCode[i])) # which i has problem
      no.of.diet <- c(no.of.diet, n)
      main.food <- c(main.food, summ2)
    }
    out <- cbind(SpecCode, no.of.diet, main.food)
    return(out)
  }
