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
    dplyr::rename(NUM=.data$trknum,Owner=.data$Ownership,
                  CumDist=.data$end_Dist) %>%
    dplyr::mutate(Description=iMakeDescription(.data$Primary,
                                               .data$From,.data$To),
                  Description=ifelse(.data$Primary==.data$Description,
                                     "--",.data$Description)) %>%
    dplyr::select(.data$NUM,.data$trackID,.data$Primary,
                  .data$Description,.data$Type,.data$Owner,
                  .data$Distance,.data$CumDist,.data$DeltaElev)
  
  knitr::kable(walksum,digits=c(0,NA,NA,NA,NA,NA,2,2,0)) %>%
    kableExtra::kable_classic(html_font="Cambria",full_width=FALSE) %>%
    kableExtra::kable_styling(bootstrap_options=c("hover","condensed"))
}