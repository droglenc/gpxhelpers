#' @title Summarize tracks that form a walk.
#' 
#' @description Create a table that summarizes tracks in a data frame that form a contiguous \dQuote{walk}.
#' 
#' @param walkdata A data frame that contains tracks that form a contiguous \dQuote{walk}. Usually made with \code{\link{walkMaker}}.
#' 
#' @details NONE YET
#' 
#' @return Returns a \code{kable} table object.
#' 
#' @seealso \code{\link{walkMap}}
#' 
#' @author Derek H. Ogle
#' @keywords manip
#' 
#' @examples
#' ## None yet.
#' 
#' @export 
walkSummary <- function(walkdata) {
  walksum <- iWalkSumPts(walkdata) %>%
    dplyr::select(.data$trknum:.data$Ownership,
                  .data$Distance,.data$DeltaElev,-.data$Primary) %>%
    dplyr::rename(NUM=.data$trknum,Owner=.data$Ownership)
  
  knitr::kable(walksum,digits=c(0,NA,NA,NA,NA,2,1)) %>%
    kableExtra::kable_classic(html_font="Cambria",full_width=FALSE) %>%
    kableExtra::kable_styling(bootstrap_options=c("hover","condensed")) %>%
    kableExtra::footnote(general=paste("Total distance of this walk is",
                                       formatC(max(walkdata$Distance),format="f",digits=2),
                                       "miles."))
}


