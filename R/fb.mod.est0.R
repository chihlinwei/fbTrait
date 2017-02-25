#' Extract model estimates using url
#'
#' Extract estimates of some fishbase properties based on models using url
#'
#' @param url A url address to a fishbase web page.
#' @details This funtion extracts estimates of some properties based on models
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
#' fb.mod.est0("http://www.fishbase.org/summary/SpeciesSummary.php?id=2593")
#'

fb.mod.est0 <-
  function(url){
    tt <- read_html(url)
    doc <- html_nodes(tt, ".smallSpace>.smallSpace")
    pr <- html_text(doc)

    # Phylogenetic diversity index
    pd <- grep("Phylogenetic diversity index", pr, value = T)[1]
    pd <- sub(".*?PD50(.*?),.*", "\\1", pd)
    pd <- unlist(regmatches(pd, gregexpr('[0-9.]+', pd)))
    if(length(pd)==0) pd <- NA

    # Baysien length-weight
    lw <- grep("length-weight", pr, value = T)[1]

    a <- sub(".*?a=(.*?),.*", "\\1", lw)                      # get string between "a=" and ","
    a <- unlist(regmatches(a, gregexpr('[0-9.]+', a)))        # only keep numbers
    if(length(a)<3 & length(a)>0) a <- c(a, rep(NA, 3-length(a)))
    if(length(a)==0)  a <- rep(NA, 3)

    b <- sub(".*?b=(.*?),.*", "\\1", lw)
    b <- unlist(regmatches(b, gregexpr('[0-9.]+', b)))
    if(length(b)<3 & length(b)>0) b <- c(b, rep(NA, 3-length(b)))
    if(length(b)==0) b <- rep(NA, 3)

    # Trophic Level
    tr <- grep("Trophic Level", pr, value = T)[1]
    tr <- sub(".*?:(.*?);.*", "\\1", tr)
    tr <- unlist(regmatches(tr, gregexpr('[0-9.]+', tr)))
    if(length(tr)<2 & length(tr)>0) tr <- c(tr, NA)
    if(length(tr)==0) tr <- rep(NA, 2)

    # Population doubling time
    re <- grep("Resilience", pr, value = T)[1]
    # wether doubling time unit is year or month
    yr <- grep("year", re, value = T)
    mn <- grep("month", re, value = T)

    if(length(yr)==0 & length(mn)==0) dt <- rep(NA, 2)  # If population doubling time doesn't exsit
    if(length(yr)==1) {
      dt <- sub(".*?time(.*?)year.*", "\\1", re)
      dt <- unlist(regmatches(dt, gregexpr('[0-9.]+', dt)))
      if(length(dt)<2 & length(dt)>0) dt <- c(dt, NA)
      if(length(dt)==0) dt <- rep(NA, 2)
    }

    if(length(mn)==1) {
      dt <- sub(".*?time(.*?)month.*", "\\1", re)
      dt <- unlist(regmatches(dt, gregexpr('[0-9.]+', dt)))
      dt <- as.numeric(dt)/12
      if(length(dt)<2 & length(dt)>0) dt <- c(dt, NA)
      if(length(dt)==0) dt <- rep(NA, 2)
    }

    # Vulnerability
    vu <- grep("Vulnerability", pr, value = T)[1]
    vu <- sub(".*?vulnerability(.*?)).*", "\\1", vu)
    vu <- unlist(regmatches(vu, gregexpr('[0-9.]+', vu)))[1]
    if(length(vu)==0) vu <- NA

    # Price category
    pc <- grep("Price category", pr, value = T)[1]
    pc <- sub(".*?Price(.*?)[.]", "\\1", pc)
    pc <- sub(".*?:(.*?)[.].*", "\\1", pc)
    pc <- unlist(regmatches(pc, gregexpr('[a-z|A-Z]+', pc)))
    pc <- paste(pc, collapse = " ")
    if(length(pc)==0) pc <- NA

    out <- c(pd, a, b, tr, dt, vu ,pc)
    names(out) <- c("phylogenetic.diversity",
                    "length.weight.a", "length.weight.a.min", "length.weight.a.max",
                    "length.weight.b", "length.weight.b.min", "length.weight.b.max",
                    "trophic.level", "trophic.level.se",
                    "double.time.min", "double.time.max",
                    "Vulnerability", "price")
    return(out)
  }
