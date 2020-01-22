#' @title R functions for unpaywall API
#'
#' @description Functions that simplify using the unpaywall API with R
#'
#' @param dois
#'
#' @param email
#'
#' @return pdf_links
#'
#' @examples dois_OA_colors(c("10.1186/s12864-016-2566-9","10.1103/physreve.88.012814"), "test@email.com")
#'
#' @export dois_OA_colors

#sends list of dois to unpaywall and returns tibble with two columns: dois & OA color
dois_OA_colors <- function(dois, email, color_hierarchy = c("gold", "hybrid", "green", "bronze", "closed"), clusters = 1, sleep = 0) {

  #parallelize pdf conversion
  cl <- parallel::makeCluster(clusters, outfile="")
  doParallel::registerDoParallel(cl)

  oaDOIs <- .to_oaDOI(dois, email)

  article_colors <- vector()

  if(length(dois) > 0) {
    #send dois to oaDOI.org to obtain article colors - foreach loop sends many requests in parallel to speed up progress
    article_colors <- foreach::foreach(i=1:length(oaDOIs),
                              .export = c(".get_article_color", ".get_loc_article_color", ".to_DOI"),
                              .packages = ("tidyverse")) %dopar% {
                                .get_article_color(oaDOIs[i], color_hierarchy, sleep)
                              }
    article_colors <- do.call(rbind, article_colors)
    article_colors_tbl <- tibble::tibble(doi = article_colors[,1],
                                 OA_color = article_colors[,2])
  } else {
    article_colors_tbl <- tibble::tibble(doi = character(),
                                 OA_color = character())
  }

  parallel::stopCluster(cl)

  return(article_colors_tbl)
}


#returns article color (gold, green, bronze, hybrid, closed) for given oaDOI
.get_article_color <- function(oaDOI, color_hierarchy, sleep)
{
  article_color <- ""
  tryCatch({
    oaDOI_result <- readLines(oaDOI, warn = FALSE)
    oaDOI_result <- jsonlite::fromJSON(oaDOI_result)

    is_oa <- oaDOI_result$is_oa
    journal_is_oa <- oaDOI_result$journal_is_oa

    #loop over all OA-locations and calculate the OA color for each
    oa_colors <- vector()
    if(length(oaDOI_result$oa_locations) == 0) {
      oa_colors <- "closed"
    } else {
      for(i in 1:dim(oaDOI_result$oa_locations)[1])
      {
        loc_article_color <- .get_loc_article_color(oaDOI_result$oa_locations[i,], is_oa, journal_is_oa)
        oa_colors <- append(oa_colors, loc_article_color)
      }
    }

    for(color in color_hierarchy) {
      if(color %in% oa_colors) {
        article_color <- color
        break
      }
    }

    # if("gold" %in% oa_colors) {
    #   article_color <- "gold"
    # } else if("green" %in% oa_colors) {
    #   article_color <- "green"
    # } else if("hybrid" %in% oa_colors) {
    #   article_color <- "hybrid"
    # } else if("bronze" %in% oa_colors) {
    #   article_color <- "bronze"
    # } else {
    #   article_color <- "closed"
    # }

  }, error=function(e){
    article_color <- ""
    print("Could not access oaDOI")
  })

  Sys.sleep(sleep)

  return(c(.to_DOI(oaDOI), article_color))
}


.get_loc_article_color <- function(oa_location, is_oa, journal_is_oa)
{
  host_type <- oa_location$host_type
  article_license <- oa_location$license

  if(is.null(article_license) | is.na(article_license)) {
    article_license <- ""
  }
  if(is.null(oa_location)) {
    article_color = "closed"
  } else if(is_oa == FALSE) {
    article_color <- "closed"
  } else if(is_oa == TRUE && host_type == "repository") {
    article_color <- "green"
  } else if(is_oa == TRUE && host_type == "publisher" && journal_is_oa == TRUE) {
    article_color <- "gold"
  } else if(is_oa == TRUE && host_type == "publisher" &&
            journal_is_oa == FALSE && (stringr::str_sub(article_license, 1, 3) == "cc-")) {
    article_color <- "hybrid"
  } else if(is_oa == TRUE) { #} && host_type == "publisher" && journal_is_oa == FALSE) {
    article_color <- "bronze"
  }

  return(article_color)
}



#converts normal DOIs to oaDOIs, adding email address
.to_oaDOI <- function(dois, email)
{
  oaDOIs <- paste0("https://api.unpaywall.org/v2/", dois, "?email=", email)
  return(oaDOIs)
}

.to_DOI <- function(oadois)
{
  dois <- oadois %>%
    str_remove("https://api.unpaywall.org/v2/") %>%
    str_split(fixed("?")) %>%
    map_chr(1)

  return(dois)
}


