#' @title Make a carousel of images
#'
#' @description Makes HTML code to be used in a quarto document that will produce a carousel of images. Names of files with images may be given in \code{imgs} (and must be in \code{path}) or if \code{imgs} is \code{NULL} then all files in \code{path} will be used to make the carousel.
#'
#' @param id Name for the carousel (make unique if more than one carousel on a page).
#' @param path A relative path to the folder with the images. If \code{imgs} is \code{NULL} then the folder should only contain image files that will form the carousel.
#' @param imgs A vector of file names with the images to form the carousel. See details.
#' @param caps A vector of captions for each image in the carousel. If \code{NULL} then captions will be the filename sans extension.
#' @param rev A logical for whether the given order of \code{imgs} and \code{caps} should be reversed. Most logically used if \code{imgs} and \code{caps} are not specified.
#' @param width The relative width (as a percentage) of the carousel (can only be 100, 75, or 50).
#' @param center Logical for whether the carousel should be centered on the page or not.
#' @param captions Logical for whether captions should be added.
#' @param controls Logical for whether the Previous and Next control items should be used.
#' @param indicators Logical for whether indicator buttons should be shown for the image list.
#' @param interval The amount of time between images on the carousel.
#' @param fade Logical for whether there is a slide (\code{FALSE}) or fade between images.
#' @param hover Logical for whether the carousel should stop on a mouseover.
#'
#' @details NONE YET
#'
#' @return None, used for side effect of writing HTML code that produces a slide carousel of images.
#'
#' @author Derek H. Ogle
#' @keywords manip
#'
#' @examples
#' ## None yet.
#'
#' @export
makeCarousel <- function(id,path,imgs=NULL,caps=NULL,rev=FALSE,
                         width=c("100","75","50"),center=TRUE,
                         captions=TRUE,controls=TRUE,indicators=FALSE,
                         interval=2500,fade=FALSE,hover=TRUE) {
  ## If imgs is NULL, use all files in path folder, otherwise given in imgs
  if (is.null(imgs)) imgs <- list.files(path=path)
  ## If caps is NULL, use filename as caps, otherwise given in caps
  if (is.null(caps)) caps <- tools::file_path_sans_ext(imgs)
  ## Make sure imgs and caps is same length
  if (length(imgs)!=length(caps)) stop("'imgs' and 'caps' not same length!",call.=FALSE)
  
  ## Reverse given order of imgs/caps if asked for
  if (rev) {
    imgs <- rev(imgs)
    caps <- rev(caps)
  }

  ## Get the width for the carousel container
  width <- as.character(width)
  width <- match.arg(width)

    ## start the carousel container
  tmp <- paste0('<div id="carousel',id,'Captions" class="carousel slide',ifelse(fade,' carousel-fade',''),' w-',width,'" data-bs-ride="carousel">','\n')

  ## create the indicators/buttons (if asked for)
  if (indicators) {
    tmp <- paste0(tmp,'<div class="carousel-indicators">','\n')
    for (i in seq_along(imgs)) {
      tmp <- paste0(tmp,'<button type="button" data-bs-target="#carousel',id,'Captions" data-bs-slide-to="',i-1,'" class="active" aria-current="true" aria-label="Slide ',i,'"></button>','\n')
    }
    tmp <- paste0(tmp,'</div>','\n')
  }

  ## add the images for each slide
  tmp <- paste0(tmp,'<div class="carousel-inner">','\n')
  for (i in seq_along(imgs)) {
    if (i==1) tmp <- paste0(tmp,'<div class="carousel-item active" data-bs-pause="hover" data-bs-interval="',interval,'">','\n')
    else tmp <- paste0(tmp,'<div class="carousel-item" data-bs-pause="hover" data-bs-interval="',interval,'">','\n')
    tmp <- paste0(tmp,'<img src="',path,imgs[i],'" class="d-block w-100" alt="',caps[i],'">','\n')
    ## add captions (if asked for)
    if (captions) {
      tmp <- paste0(tmp,'<div class="carousel-caption d-none d-md-block">','\n')
      tmp <- paste0(tmp,'<h5>',caps[i],'</h5>','\n')
      tmp <- paste0(tmp,'</div>','\n')
    }
    tmp <- paste0(tmp,'</div>','\n')
  }
  tmp <- paste0(tmp,'</div>','\n')

  ## add controls to the slide (if asked for)
  if (controls) {
    tmp <- paste0(tmp,'<button class="carousel-control-prev" type="button" data-bs-target="#carousel',id,'Captions" data-bs-slide="prev">','\n')
    tmp <- paste0(tmp,'<span class="carousel-control-prev-icon" aria-hidden="true"></span>','\n')
    tmp <- paste0(tmp,'<span class="visually-hidden">Previous</span>','\n')
    tmp <- paste0(tmp,'</button>','\n')
    tmp <- paste0(tmp,'<button class="carousel-control-next" type="button" data-bs-target="#carousel',id,'Captions" data-bs-slide="next">','\n')
    tmp <- paste0(tmp,'<span class="carousel-control-next-icon" aria-hidden="true"></span>','\n')
    tmp <- paste0(tmp,'<span class="visually-hidden">Next</span>','\n')
    tmp <- paste0(tmp,'</button>','\n')
  }

  ## finish up
  tmp <- paste0(tmp,'</div>','\n')
  ## center if asked for
  if (center) tmp <- paste0('<center>\n',tmp,'</center>\n')
  ## display the object
  cat(tmp)
}


#' @title Make thumbnails of images
#'
#' @description Makes HTML code to be used in a quarto document that will produce a thumbnail of images that can then be clicked on to view as a lightbox. Names of files with images may be given in \code{imgs} (and must be in \code{path}) or if \code{imgs} is \code{NULL} then all files in \code{path} will be used to make the carousel.
#'
#' @param id Name for the thumbnail gallery.
#' @param path A relative path to the folder with the images. If \code{imgs} is \code{NULL} then the folder should only contain image files that will form the carousel.
#' @param ncol A numeric that indicates the number of columns to be used for the thumbnails. Defaults to 6. Use a bigger number to make smaller thumbnails.
#' @param imgs A vector of file names with the images to form the thumbnails. See details.
#' @param caps A vector of captions for each image in the thumbnails. If \code{NULL} then captions will be the filename sans extension.
#' @param rev A logical for whether the given order of \code{imgs} and \code{caps} should be reversed. Most logically used if \code{imgs} and \code{caps} are not specified.
#'
#' @details NONE YET
#'
#' @return None, used for side effect of writing HTML code that produces thumbnails of images.
#'
#' @author Derek H. Ogle
#' @keywords manip
#'
#' @examples
#' ## None yet.
#'
#' @export
makeThumbs <- function(id,path,ncol=6,imgs=NULL,caps=NULL,rev=FALSE) {
  ## If imgs is NULL, use all files in path folder, otherwise given in imgs
  if (is.null(imgs)) imgs <- list.files(path=path)
  ## If caps is NULL, use filename as caps, otherwise given in caps
  if (is.null(caps)) {
    ## Remove extension and underscore in name
    caps <- tools::file_path_sans_ext(imgs)
    caps <- stringr::str_replace(caps,"_"," ")
  }
  ## Make sure imgs and caps is same length
  if (length(imgs)!=length(caps)) stop("'imgs' and 'caps' not same length!",call.=FALSE)
  
  ## Reverse given order of imgs/caps if asked for
  if (rev) {
    imgs <- rev(imgs)
    caps <- rev(caps)
  }
  
  tmp <- paste0("::: {layout-ncol=",ncol,"}\n\n")
  for (i in seq_along(imgs)) {
    tmp <- paste0(tmp,'![',caps[i],'](',path,imgs[i],'){group="',id,'"}')
    tmp <- paste0(tmp,"\n\n")
  }
  tmp <- paste0(tmp,":::")
  cat(tmp)
}
