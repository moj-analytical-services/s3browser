#' Convert the output of a selection to platform specific path(s)
#'
#' This function takes the value of a shinyFiles button input variable and
#' converts it to be easier to work with on the server side. In the case of file
#' selections  and saving the input variable is converted to a data frame (using
#' \code{parseFilePaths()} or \code{parseSavePath() respectively}) of the same
#' format as that provided by \code{\link[shiny]{fileInput}}. The only caveat
#' here is that the MIME type cannot be inferred in file selections so this will
#' always be an empty string and new files doesn't have a size so this is left
#' out with file saving. In the case of folder selection the input variable is
#' converted to a string (using \code{parseDirPath()}) giving the absolute path
#' to the selected folder.
#'
#' The use of \code{parseFilePaths} makes it easy to substitute fileInput and
#' shinyFiles in your code as code that relies on the values of a file selection
#' doesn't have to change.
#'
#' @param roots The path to the root as specified in the \code{shinyFileChoose()}
#' call in \code{shinyServer()}
#'
#' @param selection The corresponding input variable to be parsed
#'
#' @return A data frame mathcing the format of \code{\link[shiny]{fileInput}}
#'
#' @examples
#' \dontrun{
#' ui <- shinyUI(bootstrapPage(
#'     shinyFilesButton('files', 'File select', 'Please select a file', FALSE),
#'     verbatimTextOutput('rawInputValue'),
#'     verbatimTextOutput('filepaths')
#' ))
#' server <- shinyServer(function(input, output) {
#'     roots = c(wd='.')
#'     shinyFileChoose(input, 'files', roots=roots, filetypes=c('', 'txt'))
#'     output$rawInputValue <- renderPrint({str(input$files)})
#'     output$filepaths <- renderPrint({parseFilePaths(roots, input$files)})
#' })
#'
#' runApp(list(
#'     ui=ui,
#'     server=server
#' ))
#' }
#'
#' @rdname shinyFiles-parsers
#' @name shinyFiles-parsers
#'
#' @family shinyFiles
#'
#'
#'
parseFilePaths <- function(roots, selection) {
  roots <- if(class(roots) == 'function') roots() else roots

  if (is.null(selection) || is.na(selection)) return(data.frame(name=character(0), size=numeric(0),
                                                                type=character(0), datapath=character(0),
                                                                stringsAsFactors = FALSE))
  files <- sapply(selection$files, function(x) {file.path(roots[selection$root], do.call('file.path', x))})
  files <- gsub(pattern='//*', '/', files, perl=TRUE)

  data.frame(name=basename(files), size=file.info(files)$size, type='', datapath=files, stringsAsFactors = FALSE)
}


#' @rdname shinyFiles-parsers
#'
#'
#'
parseDirPath <- function(roots, selection) {
  currentRoots <- if(class(roots) == 'function') roots() else roots

  if (is.null(names(currentRoots))) stop('Roots must be a named vector or a function returning one')

  root <- currentRoots[selection$root]

  location <- do.call('file.path', as.list(selection$path))
  gsub(pattern='//*', '/', file.path(root, location), perl=TRUE)
}

#' @rdname shinyFiles-parsers
#'
#'
#'
parseSavePath <- function(roots, selection) {
  if(is.null(selection)) return(data.frame(name=character(), type=character(),
                                           datapath=character(), stringsAsFactors = FALSE))

  currentRoots <- if(class(roots) == 'function') roots() else roots

  if (is.null(names(currentRoots))) stop('Roots must be a named vector or a function returning one')

  root <- currentRoots[selection$root]

  location <- do.call('file.path', as.list(selection$path))
  savefile <- file.path(root, location, selection$name)
  savefile <- gsub(pattern='//*', '/', savefile, perl=TRUE)
  type <- selection$type
  if (is.null(type)) {
    type <- ""
  }
  data.frame(name=selection$name, type=type, datapath=savefile, stringsAsFactors = FALSE)
}




#' Title
#'
#' @param df_in
#'
#' @return formatted s3 path command
#'
#'
shiy_js_to_s3tools_command <- function(df_in){
  df_in <- stringr::str_sub(df_in, 4)
  output_str <- stringr::str_interp("s3tools::s3_path_to_full_df(\"${df_in}\")\n")
  return(output_str)
}

