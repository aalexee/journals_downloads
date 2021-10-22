
# packages ----------------------------------------------------------------

library(shiny)
library(fs)
library(tidyverse)
library(rvest)


# options -----------------------------------------------------------------


options(timeout = max(300, getOption("timeout")))


# ui ----------------------------------------------------------------------

ui <- fluidPage(

    theme = bslib::bs_theme(bootswatch = "sandstone")
    , fluidRow(
        column(
            width = 4, offset = 4
            , h1("AER Citations Downloader")
            , p(
                "The app downloads citations for"
                , strong("all")
                , "the articles in a selected issue of the"
                , a(
                    "American Economic Review"
                    , href = "https://www.aeaweb.org/journals/aer"
                )
                , " (except the 'Front Matter')."
            )
            , p(
                "Paste the URL of an issue you want into the"
                , code("Issue URL")
                , "field,"
                , "choose the desired"
                , code("Citation format")
                , ", click the"
                , code("Prepare citations")
                , "button, wait until the download process is finished, and finally click the"
                , code("Download citations")
                , "button to download a zip file with citations on your computer."
            )
            , hr()
            , h5("Issue URL")
            , textInput("url", label = NULL, value = "https://www.aeaweb.org/issues/617")
            , h5("Citation format")
            , selectInput("cit_format", label = NULL
                          , choices = list(
                              "BibTeX" = "bib"
                              , "EndNote" = "enw"
                              , "Refer/BiblX" = "txt"
                              , "RIS" = "ris"
                              , "Tab-Delimited" = "tab"
                          ))
            , actionButton(
                "download_bibs"
                , "Prepare citations"
                # , icon = icon("download")
                # , class = "btn-primary"
            )
            , downloadButton(
                "download_bibs_zip"
                , "Download citations"
                , class = "btn-primary"
            )
            , hr()
            , p(
                "Programmed by "
                , a(href = "https://aalexee.com", "Alex Alekseev")
                , "."
            )
        )
    )
)


# server ------------------------------------------------------------------


server <- function(input, output, session) {

    input_directory <- tempdir()

    observeEvent(input$download_bibs, {

        unlink(str_c(input_directory, .Platform$file.sep, "*.", input$cit_format))

        pg <- read_html(input$url)

        hrefs <- pg %>%
            html_nodes(., "a") %>%
            html_attr("href")

        articles <- hrefs %>%
            str_subset(., "articles\\?") %>%
            str_subset(., "\\.i", negate = T) %>%
            str_extract(., "aer.*")

        issue_id <-  hrefs %>%
            str_subset(., "articles\\?") %>%
            str_sub(., str_locate(., "id")[, 1] + 3, str_locate(., "aer")[, 1] - 2)
        issue_id <- issue_id[1]

        withProgress(
            expr = {

                for (j in 1:length(articles)) {

                    i <- articles[j]

                    name_download <- str_c(str_remove_all(i, "\\."), ".", input$cit_format)
                    path <- str_c(
                        input_directory
                        , .Platform$file.sep
                        , name_download
                    )
                    url_cit <- str_c(
                        "https://www.aeaweb.org/articles/citation-export?args%5Bformat%5D="
                        , input$cit_format
                        , "&args%5Bdoi%5D="
                        , issue_id
                        , "%2F"
                        , i
                        , "&args%5Btype%5D="
                        , input$cit_format
                    )

                    download.file(
                        url = url_cit
                        , destfile = path
                    )

                    incProgress(
                        1/length(articles)
                        , detail = str_c(j, " of ", length(articles))
                    )
                }
            }
            , message = "Downloaded"
        )


    })

    output$download_bibs_zip <- downloadHandler(
        filename = function() {
            "citations.zip"
        },
        content = function(zip_file_name) {
            files <- list.files(
                path = input_directory
                , pattern = input$cit_format
                , full.names = F
            )
            zip::zip(
                zipfile = zip_file_name
                , files = files
                , root = input_directory
            )
        }
        , contentType = "application/zip"
    )

}


# run ---------------------------------------------------------------------


shinyApp(ui = ui, server = server)
