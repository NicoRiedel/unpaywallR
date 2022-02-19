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
#'

#sends list of dois to unpaywall and returns tibble with the DOI, OA_color, issn, journal, publisher, and publication date
dois_OA_colors <- function(dois, email, color_hierarchy = c("gold", "hybrid", "green", "bronze", "closed"), clusters = 1, sleep = 0) {
  tbl <- dois_OA_colors_fetch(dois, email, clusters = clusters, sleep = sleep)
  return(dois_OA_pick_color(tbl, color_hierarchy))
}

dois_OA_colors_fetch <- function(dois, email, clusters = 1, sleep = 0) {

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
                                .get_article_color(oaDOIs[i], sleep)
                              }
    article_colors <- do.call(rbind, article_colors)
    article_colors_tbl <- tibble::tibble(doi = article_colors[,1],
                                 OA_colors = article_colors[,2],
                                 issn = article_colors[,3],
                                 journal = article_colors[,4],
                                 publisher = article_colors[,5],
                                 date = article_colors[,6])
  } else {
    article_colors_tbl <- tibble::tibble(doi = character(),
                                 OA_colors = character(),
                                 issn = character(),
                                 journal = character(),
                                 publisher = character(),
                                 date = character())
  }

  parallel::stopCluster(cl)

  return(article_colors_tbl)
}

dois_OA_pick_color <- function(df, color_hierarchy) {

  pick_color <- function(column){
    res <- vector()
    for (r in column) {
      oa_colors <- unlist(strsplit(as.character(r), ";"))
      article_color <- ""

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

      res <- append(res, article_color)
    }

    return(res)
  }

  result <-
  df %>%
    mutate(OA_colors = pick_color(OA_colors)) %>%
    rename(OA_color = OA_colors)

  return(result)
}

#returns all article colors for given oaDOI
.get_article_color <- function(oaDOI, sleep)
{
  oa_colors <- vector()
  issn <- ""
  journal <- ""
  publisher <- ""
  date <- ""

  tryCatch({
    oaDOI_result <- readLines(oaDOI, warn = FALSE)
    oaDOI_result <- jsonlite::fromJSON(oaDOI_result)

    is_oa <- oaDOI_result$is_oa
    journal_is_oa <- oaDOI_result$journal_is_oa

    if(!is.null(oaDOI_result$journal_issns)) {
      issn <- oaDOI_result$journal_issns
    }

    if(!is.null(oaDOI_result$journal_name)) {
      journal <- oaDOI_result$journal_name
    }

    if(!is.null(oaDOI_result$publisher)) {
      publisher <- oaDOI_result$publisher
    }

    if(!is.null(oaDOI_result$published_date)) {
      date <- oaDOI_result$published_date
    }

    #loop over all OA-locations and calculate the OA color for each
    if(length(oaDOI_result$oa_locations) == 0) {
      oa_colors <- append(oa_colors, "closed")
    } else {
      for(i in 1:dim(oaDOI_result$oa_locations)[1])
      {
        loc_article_color <- .get_loc_article_color(oaDOI_result$oa_locations[i,], is_oa, journal_is_oa)
        oa_colors <- append(oa_colors, loc_article_color)
      }
    }

  }, error=function(e){
    print("Could not access oaDOI")
  })

  Sys.sleep(sleep)
  return(c(.to_DOI(oaDOI), paste0(oa_colors, collapse=";"), issn, journal, publisher, date))
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


