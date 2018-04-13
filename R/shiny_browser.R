#' s3 file explorer
#'
#' Selected files will be inseted as s3tools commands into the source pane
#'
#' Browser currently supports selection of multiple files.
#' @export
#' @import shiny
#' @import miniUI
#' @importFrom magrittr %>%
#' @importFrom magrittr %$%
file_explorer_s3 <- function() {


  shiny::addResourcePath('sF', system.file('www', package='s3browser'))

  # buckets <- s3tools::accessible_buckets()
  # df <- s3tools::accessible_files_df()
  # df <- df[,c("filename", "size_readable", "path")]

  # Our ui will be a simple gadget page, which
  # simply displays the time in a 'UI' output.
  ui <- miniPage(
    gadgetTitleBar("S3 Explorer"),
    miniTabstripPanel(
      miniTabPanel("Browser", icon = icon("files-o"),
                   miniContentPanel(fillRow(
                     shiny::includeMarkdown(system.file('markdown', 'widget_guide.md', package = 's3browser')),
                     shinyFilesButton('file', 'File select', 'Please select a file', TRUE, buttonType = 'primary'))
                   )
      )))

  server <- function(input, output, session) {
    volumes <- c('S3'='S3')
    shinyFileChoose(input, 'file', roots=volumes, session=session, restrictions=NULL,
                    defaultRoot = 'S3', defaultPath = '')

    output$filepaths <- renderPrint({parseFilePaths(volumes, input$file)})
    output$directorypath <- renderPrint({parseDirPath(volumes, input$directory)})
    output$savefile <- renderPrint({parseSavePath(volumes, input$save)})

    observeEvent(input$file,{
      tryCatch(
        parseFilePaths(volumes,input$file) %$% datapath %>%
          purrr:::map(s3browser:::shiy_js_to_s3tools_command) %>%
          purrr::map(rstudioapi:::sendToConsole, execute=FALSE)

      )
      shiny::stopApp()
    })



     # Listen for 'done' events. When we're finished, we'll
    # insert the current time, and then stop the gadget.
    observeEvent(input$done, {
      id <- input$files_table_rows_selected
      if (!(is.null(id))) {
        sel <- as.list(df_filtered()[id,])
        output_str <- stringr::str_interp("s3tools::s3_path_to_full_df(\"${sel$path}\")")
        rstudioapi::sendToConsole(output_str, execute = FALSE)
      }
      shiny::stopApp()

    })




  }

  # We'll use a pane viwer, and set the minimum height at
  viewer <- shiny::paneViewer(500)
  #shiny::runGadget(ui, server, viewer = viewer, stopOnCancel = TRUE)


  # Run the addin
  app <- shinyApp(ui = ui, server = server)
  #viewer <- dialogViewer("Find files", width = 1200, height = 900)
  shiny::runGadget(app, viewer = viewer, stopOnCancel = TRUE)

}


