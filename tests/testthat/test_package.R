context("PDF_links")

doi_1 <- "10.1103/physreve.88.012814"
doi_2 <- "10.1186/s12864-016-2566-9"
email <- "test@email.com"

test_that("PDF_links",
          {
            expect_true(dois_pdf_link(doi_1, email)[1,2] ==
                          "https://link.aps.org/accepted/10.1103/PhysRevE.88.012814")
          })

context("OA colors")

test_that("OA colors",
          {
            expect_true(dois_OA_colors(c(doi_2), email)[1,2] == "gold")
            expect_true(dois_OA_colors(c(doi_1), email)[1,2] == "green")
          })

test_that("OA hierarchy",
          {
            expect_true(dois_OA_colors(c(doi_1), email,
                                       color_hierarchy = c("gold", "hybrid", "green", "bronze", "closed"))[1,2] ==
                          "green")
            expect_true(dois_OA_colors(c(doi_1), email,
                                       color_hierarchy = c("gold", "bronze", "hybrid", "green", "closed"))[1,2] ==
                          "bronze")
            expect_true(dois_OA_colors(c(doi_2), email,
                                       color_hierarchy = c("gold", "hybrid", "green", "bronze", "closed"))[1,2] ==
                          "gold")
            expect_true(dois_OA_colors(c(doi_2), email,
                                       color_hierarchy = c("green", "gold", "hybrid", "bronze", "closed"))[1,2] ==
                          "green")
          })

