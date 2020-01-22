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

