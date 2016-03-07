#' Get DATRAS data
#'
#' @param record Record type ("HH", "HL", "CA")
#' @param survey Survey name
#' @param year Year
#' @param quarter Quarter
#' @param time Boolean, if TRUE prints out time. The default is FALSE
#'
#' @export

get_datras <- function(record = "CA", survey = "NS-IBTS", year = 2015, quarter = 1,
                       time = FALSE) {

  URL <-
    paste0("http://datras.ices.dk/WebServices/DATRASWebService.asmx/get",
           toupper(record),
           "data")

  time1 <- proc.time()
  
  r <- httr::GET(URL,query=list(survey=survey,year=year,quarter=quarter))

  time2 <- proc.time()

  x <-
    r %>%
    httr::content(as = "parsed") %>%
    xml2::xml_children()
  var.names <- tolower(xml2::xml_name(xml2::xml_children(x[[1]])))
  n.col <- length(var.names)
  x <-
    x %>%
    xml2::xml_text() %>%
    stringr::str_replace_all("\\n", "\t") %>%
    stringr::str_replace_all(" +", "") %>%
    read.table(text = .,
               sep = "\t",
                  na.strings = c("-9.0000","-9.000","-9.00","-9.0","-9"),
                  stringsAsFactors = FALSE)
  x <- x[,c(2:(ncol(x)-1))]
  colnames(x) <- var.names

  time3 <- proc.time()
  if(time) {
    print(time2 - time1)
    print(time3 - time1)
  }

  if(toupper(record) == "HH") {
    x$timeshot <- as.character(x$timeshot)
    i <- nchar(x$timeshot) == 3
    if(any(i)) x$timeshot[i] <- paste0("0",x$timeshot[i])
  }
  
  if(toupper(record) == "HL") {
    x$lngtclass = as.character(x$lngtclass)
  }

  return(x)
}

