# Initially, want an interface that allows the user to search for a dataset, select it, and that writes code
# First, let's just have two buttons, which write different code


#' We'll wrap our Shiny Gadget in an addin.
#' @export
#' @importFrom magrittr %>%
file_browse_addin <- function() {

  # Our ui will be a simple gadget page, which
  # simply displays the time in a 'UI' output.
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Select rows. 'Done' inserts code."),
    miniUI::miniContentPanel(
      div(style="display: inline-block;vertical-align:top; width: 64%;",
            shiny::textInput("search", "Search (regex)", "")),
      div(style="display: inline-block;vertical-align:middle; width: 30%;",
          shiny::checkboxInput("preview", "Preview files?", TRUE))
    ),
    DT::dataTableOutput("files_table", height= "400px")
  )

  df <- s3tools::accessible_files_df()

  df <- df[,c("filename", "size_readable", "path")]

  server <- function(input, output, session) {

    df_filtered <- shiny::reactive({
      regex <- input$search

      df2 <- tryCatch({
        df %>%
          dplyr::filter(stringr::str_detect(path, regex))
        },
        error = function(e) {
          message("Your serach is not a valid regular expression, searching on exact string")
          df %>%
            dplyr::filter(stringr::str_detect(path, stringr::coll(regex)))
        }
      )

      df2
    })

    output$files_table <-  DT::renderDataTable(df_filtered(), selection = list(mode = 'single', target = 'row'), options = list(dom = 'tp'))
    # Listen for 'done' events. When we're finished, we'll
    # insert the current time, and then stop the gadget.
    observeEvent(input$done, {
      id <- input$files_table_rows_selected
      if (!(is.null(id))) {
        sel <- as.list(df[id,])
        output_str <- stringr::str_interp("s3tools::s3_path_to_full_df(\"${sel$path}\")")
        rstudioapi::insertText(output_str)
      }
      shiny::stopApp()

    })

    observeEvent(input$files_table_rows_selected, {
      if (input$preview) {
        id <- input$files_table_rows_selected
        sel <- as.list(df[id,])
        tryCatch({df <- s3tools::s3_path_to_preview_df(sel$path)
                 View(df)},
                 error = function(e) {message("You selected a file that could not be previewed using read_csv")}
        )

      }
    })

  }

  # We'll use a pane viwer, and set the minimum height at
  viewer <- shiny::paneViewer(500)
  shiny::runGadget(ui, server, viewer = viewer)
  # shiny::shinyApp(ui, server)

}
