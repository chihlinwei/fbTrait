#' Extract fooditem using SpecCode
#'
#' Extract fooditem from fishbase using SpecCode
#'
#' @param SpecCode A vector of fishbase SpecCode.
#' @details This funtion extracts fooditem information from fishbase using SpecCode
#' @return A matrix of fooditem , including:
#' \describe{
#'   \item{SpecCode}{}
#'   \item{no.of.food1}{}
#'   \item{food1}{}
#'   \item{no.of.food2}{}
#'   \item{food2}{}
#'   \item{no.of.food3}{}
#'   \item{food3}{}
#' }
#' @author Chih-Lin Wei <chihlinwei@@gmail.com>
#' @export
#' @examples
#' fb.fooditem(c(69, 309, 2420, 3272))
#'
fb.fooditem <-
  function(SpecCode){
    server <- "http://www.fishbase.org/"
    url <- paste(server, "Summary/", SpecCode, sep = "")
    food1 <- NULL; food2 <- NULL; food3 <- NULL
    no.of.food1 <- NULL; no.of.food2 <- NULL; no.of.food3 <- NULL
    for(i in 1:length(SpecCode)){
      tt <- getURL(url[i], followlocation=TRUE)
      doc <- htmlParse(tt)
      links <- getHTMLLinks(doc)
      good <- grep("FoodItemsList", links, value = TRUE)
      if(length(good)==0){
        n1 <- NA; out1 <- NA; n2 <- NA; out2 <- NA; n3 <- NA; out3 <- NA
      } else {
        good.url <- sub("../", server, good)
        tt2 <- getURL(good.url, followlocation=TRUE)
        doc2 <- htmlParse(tt2)
        tab <- readHTMLTable(doc2)
        if(is.null(tab$dataTable)){
          n1 <- NA; out1 <- NA; n2 <- NA; out2 <- NA; n3 <- NA; out3 <- NA
        } else {
          item1 <- table(tab$dataTable[, 1])
          per1 <- sort(round(item1/sum(item1)*100, 1), decreasing = TRUE)
          out1 <- paste(paste(per1, "%", " ", names(per1), sep = ""), collapse = ", ")
          n1 <- length(item1)

          item2 <- table(tab$dataTable[, 2])
          per2 <- sort(round(item2/sum(item2)*100, 1), decreasing = TRUE)
          out2 <- paste(paste(per2, "%", " ", names(per2), sep = ""), collapse = ", ")
          n2 <- length(item2)

          item3 <- table(tab$dataTable[, 3])
          per3 <- sort(round(item3/sum(item3)*100, 1), decreasing = TRUE)
          out3 <- paste(paste(per3, "%", " ", names(per3), sep = ""), collapse = ", ")
          n3 <- length(item3)
        }
      }
      if(length(n1)!=1) print(paste("Check SpecCode", SpecCode[i])) # which i has problem
      no.of.food1 <- c(no.of.food1, n1)
      food1 <- c(food1, out1)
      no.of.food2 <- c(no.of.food2, n2)
      food2 <- c(food2, out2)
      no.of.food3 <- c(no.of.food3, n3)
      food3 <- c(food3, out3)
    }
    out <- cbind(SpecCode, no.of.food1, food1, no.of.food2, food2, no.of.food3, food3)
    return(out)
  }
