# Initially, want an interface that allows the user to search for a dataset, select it, and that writes code
# First, let's just have two buttons, which write different code


# We'll wrap our Shiny Gadget in an addin.
file_browse_addin <- function() {

  # Our ui will be a simple gadget page, which
  # simply displays the time in a 'UI' output.
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Select a row and hit 'done' to insert R code to read the data"),
    miniUI::miniContentPanel(
      DT::dataTableOutput("files_table", height= "400px")
    )
  )

  df <- s3tools::accessible_files_df()

  df <- df[,c("filename", "size_readable", "path")]

  server <- function(input, output, session) {

    output$files_table <-  DT::renderDataTable(df, selection = list(mode = 'single', target = 'row'))
    # Listen for 'done' events. When we're finished, we'll
    # insert the current time, and then stop the gadget.
    observeEvent(input$done, {

      id <- input$files_table_rows_selected
      sel <- as.list(df[id,])

      output_str <- stringr::str_interp("s3tools::s3_path_to_full_df(\"${sel$path}\")")

      rstudioapi::insertText(output_str)
      stopApp()

    })

  }

  # We'll use a pane viwer, and set the minimum height at
  viewer <- shiny::paneViewer(500)
  shiny::runGadget(ui, server, viewer = viewer)

}

file_browse_addin()
