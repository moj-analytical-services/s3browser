# Initially, want an interface that allows the user to search for a dataset, select it, and that writes code
# First, let's just have two buttons, which write different code


#' We'll wrap our Shiny Gadget in an addin.
#' @export
#' @import shiny
file_explorer_s3 <- function() {


  shiny::addResourcePath('sF', system.file('www', package='s3browser'))

  # Our ui will be a simple gadget page, which
  # simply displays the time in a 'UI' output.
  ui <- shiny::shinyUI(miniUI::miniPage(shiny::bootstrapPage(
    shinyFilesButton('file', 'File select', 'Please select a file', TRUE, buttonType = 'primary'),
    verbatimTextOutput('filepaths')
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
        rstudioapi::insertText(shiy_js_to_s3tools_command(parseFilePaths(volumes,input$file)))
        )
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


