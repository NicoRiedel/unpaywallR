#' @title get PDF links from unpaywall
#'
#' @description Functions that gets PDF links from DOI using the unpaywall API with R
#'
#' @param dois
#'
#' @param email
#'
#' @return pdf_links
#'
#' @examples dois_pdf_link(c("10.1186/s12864-016-2566-9","10.1103/physreve.88.012814"), "test@email.com")
#'
#' @export dois_pdf_link

dois_pdf_link <- function(dois, email, repository_pdf = TRUE)
{
  dois <- dois[dois != ""] #remove empty dois

  response <- roadoi::oadoi_fetch(dois, email)
  if(length(response) == 0) {
    pdf_links <- NA
  } else {
    pdf_links <- .field_content(response, "url_for_pdf")

    #if we don't want the repository version, return no link
    if(repository_pdf == FALSE) {
      host_type <- .field_content(response, "host_type")
      pdf_links[host_type == "repository"] <- NA
    }

    pdf_links <- tibble::tibble(doi = response$doi,
                        pdf_links = pdf_links)
  }

  return(pdf_links)
}


.field_content <- function(oadoi_response, field)
{
  content <- oadoi_response %>%
    dplyr::mutate(
      extracted_field = purrr::map(best_oa_location, field) %>%
        purrr::map_if(purrr::is_empty, ~ NA_character_) %>%
        purrr::flatten_chr()
    ) %>%
    .$extracted_field

  return(content)
}










# oaDOI_article_colors <- function(dois, email)
# {
#   dois <- dois[dois != ""] #remove empty dois
#
#   response <- roadoi::oadoi_fetch(dois, email)
#   host_type <- .field_content(response, "host_type")
#   oa_article <- response$is_oa
#   oa_journal <- response$journal_is_oa
#
#   article_colors <- rep("closed", dim(response)[1])
#   article_colors[oa_article == TRUE & host_type == "repository"] <- "green"
#   article_colors[oa_article == TRUE & host_type == "publisher" & oa_journal == TRUE] <- "gold"
#   article_colors[oa_article == TRUE & host_type == "publisher" & oa_journal == FALSE] <- "hybrid/bronze"
#
#   article_colors <- cbind(response$doi, article_colors)
#   colnames(article_colors) <- c("doi", "article_colors")
#
#   return(article_colors)
# }
#
#
#
#
#
# #converts normal DOIs to oaDOIs, adding email address
# to_oaDOI <- function(dois, email)
# {
#   oaDOIs <- paste0("https://api.oadoi.org/v2/", dois, "?email=", email)
#   return(oaDOIs)
# }
#
#
# #returns article color (gold, green, bronze, hybrid, closed) for given oaDOI
# get_article_color <- function(oaDOI)
# {
#   article_color <- ""
#   tryCatch({
#     oaDOI_result <- readLines(oaDOI, warn = FALSE)
#     oaDOI_result <- jsonlite::fromJSON(oaDOI_result)
#
#     host_type <- oaDOI_result$best_oa_location$host_type
#     if(is.null(oaDOI_result$best_oa_location)) {
#       article_color = "closed"
#     } else if(oaDOI_result$is_oa == FALSE) {
#       article_color <- "closed"
#     } else if(oaDOI_result$is_oa == TRUE && host_type == "repository") {
#       article_color <- "green"
#     } else if(oaDOI_result$is_oa == TRUE && host_type == "publisher" && oaDOI_result$journal_is_oa == TRUE) {
#       article_color <- "gold"
#     } else if(oaDOI_result$is_oa == TRUE && host_type == "publisher" && oaDOI_result$journal_is_oa == FALSE) {
#       article_color <- "hybrid/bronze"
#     }
#
#   }, error=function(e){
#     article_color <- ""
#     print("Could not access oaDOI")
#   })
#
#   return(article_color)
# }
#
