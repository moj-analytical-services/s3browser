#' Create a connection to the server side filesystem
#'
#' These function sets up the required connection to the client in order for the
#' user to navigate the filesystem. For this to work a matching button should be
#' present in the html, either by using one of the button generating functions
#' or adding it manually. See \code{\link{shinyFiles-buttons}} for more details.
#'
#' Restrictions on the access rights of the client can be given in several ways.
#' The root parameter specifies the starting position for the filesystem as
#' presented to the client. This means that the client can only navigate in
#' subdirectories of the root. Paths passed of to the \code{restrictions}
#' parameter will not show up in the client view, and it is impossible to
#' navigate into these subdirectories. The \code{filetypes} parameter takes a
#' vector of file extensions to filter the output on, so that the client is
#' only presented with these filetypes. The \code{hidden} parameter toggles
#' whether hidden files should be visible or not. Whenever a file or folder
#' choice is made the resulting files/folder will be accessible in the input
#' variable with the id given in the parameters. This value should probable be
#' run through a call to one of the parser (\code{\link{shinyFiles-parsers}}) in
#' order to get well formatted paths to work with.
#'
#' @param input The input object of the \code{shinyServer()} call (usaully
#' \code{input})
#'
#' @param id The same ID as used in the matching call to
#' \code{shinyFilesButton} or as the id attribute of the button, in case of a
#' manually defined html. This id will also define the id of the file choice in
#' the input variable
#'
#' @param updateFreq The time in milliseconds between file system lookups. This
#' determines the responsiveness to changes in the filesystem (e.g. addition of
#' files or drives)
#'
#' @param session The session object of the shinyServer call (usually
#' \code{session}).
#'
#' @param defaultRoot The default root to use. For instance if
#'                    \code{roots = c('wd' = '.', 'home', '/home')} then \code{defaultRoot}
#'                    can be either \code{'wd'} or \code{'home'}.
#'
#' @param defaultPath The default relative path specified given the \code{defaultRoot}.
#'
#' @param ... Arguments to be passed on to \code{\link{fileGetter}} or
#' \code{\link{dirGetter}}
#'
#' @return A reactive observer that takes care of the server side logic of the
#' filesystem connection.
#'
#' @note The syntax for this version has changed with version 0.4.0. Prior to
#' that version the output of \code{shinyFileChoose()} should be assigned to the
#' output object. This is no longer the case and doing so will result in an
#' error. In newer versions the function returns an observer which can be
#' ignored for the most part, or assigned to a variable if there needs to be
#' interactions with it later on.
#'
#' @examples
#' \dontrun{
#' # File selections
#' ui <- shinyUI(bootstrapPage(
#'     shinyFilesButton('files', 'File select', 'Please select a file', FALSE)
#' ))
#' server <- shinyServer(function(input, output) {
#'     shinyFileChoose(input, 'files', roots=c(wd='.'), filetypes=c('', 'txt'),
#'                     defaultPath='', defaultRoot='wd')
#' })
#'
#' runApp(list(
#'     ui=ui,
#'     server=server
#' ))
#' }
#'
#' @rdname shinyFiles-observers
#' @name shinyFiles-observers
#'
#' @family shinyFiles
#'
#' @importFrom shiny observe invalidateLater
#'
#'
#'
#'
#'
shinyFileChoose <- function(input, id, updateFreq=200000, session = getSession(),
                            defaultRoot=NULL, defaultPath='', ...) {
  fileGet <- do.call(s3_dir_shiny_files_list, list(...))
  currentDir <- list()
  clientId = session$ns(id)

  return(observe({
    dir <- input[[paste0(id, '-modal')]]
    if(is.null(dir) || is.na(dir)) {
      dir <- list(dir=defaultPath, root=defaultRoot)
    } else {
      dir <- list(dir=dir$path, root=dir$root)
    }
    dir$dir <- do.call(file.path, as.list(dir$dir))
    newDir <- do.call('fileGet', dir)
    if(!identical(currentDir, newDir)) {
      currentDir <<- newDir
      session$sendCustomMessage('shinyFiles', list(id=clientId, dir=newDir))
      session$sendCustomMessage('s3browser', list(id=clientId, dir=newDir))
    }
    invalidateLater(updateFreq, session)
  }))
}

#' Create a button to summon a shinyFiles dialog
#'
#' This function adds the required html markup for the client to access the file
#' system. The end result will be the appearance of a button on the webpage that
#' summons one of the shinyFiles dialog boxes. The last position in the file
#' system is automatically remembered between instances, but not shared between
#' several shinyFiles buttons. For a button to have any functionality it must
#' have a matching observer on the server side. shinyFilesButton() is matched
#' with shinyFileChoose() and shinyDirButton with shinyDirChoose(). The id
#' argument of two matching calls must be the same. See
#' \code{\link{shinyFiles-observers}} on how to handle client input on the
#' server side.
#'
#' @details
#' \strong{Selecting files}
#'
#' When a user selects one or several files the corresponding input variable is
#' set to a list containing a character vector for each file. The character
#' vectors gives the traversal route from the root to the selected file(s). The
#' reason it does not give a path as a string is that the client has no
#' knowledge of the file system on the server and can therefore not ensure
#' proper formatting. The \code{\link{parseFilePaths}} function can be used on
#' the server to format the input variable into a format similar to that
#' returned by \code{\link[shiny]{fileInput}}.
#'
#' \strong{Selecting folders}
#'
#' When a folder is selected it will also be available in its respective input
#' variable as a list giving the traversal route to the selected folder. To
#' properly format it, feed it into \code{\link{parseDirPath}} and a string with
#' the full folder path will be returned.
#'
#' \strong{Creating files (saving)}
#'
#' When a new filename is created it will become available in the respective
#' input variable and can be formatted with \code{\link{parseSavePath}} into a
#' data.frame reminiscent that returned by fileInput. There is no size column
#' and the type is only present if the filetype argument is used in
#' \code{shinySaveButton}. In that case it will be the name of the chosen type
#' (not the extension).
#'
#' \strong{Manual markup}
#'
#' For users wanting to design their html markup manually it is very easy to add
#' a shinyFiles button. The only markup required is:
#'
#' \emph{shinyFilesButton}
#'
#' \code{<button id="inputId" type="button" class="shinyFiles btn btn-default" data-title="title" data-selecttype="single"|"multiple">label</button>}
#'
#' \emph{shinyDirButton}
#'
#' \code{<button id="inputId" type="button" class="shinyDirectories btn-default" data-title="title">label</button>}
#'
#' \emph{shinySaveButton}
#'
#' \code{<button id="inputId" type="button" class="shinySave btn-default" data-title="title" data-filetype="[{name: 'type1', ext: ['txt']}, {name: 'type2', ext: ['exe', 'bat']}]">label</button>}
#'
#' where the id tag matches the inputId parameter, the data-title tag matches
#' the title parameter, the data-selecttype is either "single" or "multiple"
#' (the non-logical form of the multiple parameter) and the internal textnode
#' mathces the label parameter. The data-filetype tag is a bit more involved as
#' it is a json formatted array of objects with the properties 'name' and 'ext'.
#' 'name' gives the name of the filetype as a string and 'ext' the allowed
#' extensions as an array of strings. The non-exported
#' \code{\link{formatFiletype}} function can help convert from a named R list
#' into the string representation. In the example above "btn-default" is used as
#' button styling, but this can be changed to any other Bootstrap style.
#'
#' Apart from this the html document should link to a script with the
#' following path 'sF/shinyFiles.js' and a stylesheet with the following path
#' 'sF/styles.css'.
#'
#' The markup is bootstrap compliant so if the bootstrap css is used in the page
#' the look will fit right in. There is nothing that hinders the developer from
#' ignoring bootstrap altogether and designing the visuals themselves. The only
#' caveat being that the glyphs used in the menu buttons are bundled with
#' bootstrap. Use the css ::after pseudoclasses to add alternative content to
#' these buttons. Additional filetype specific icons can be added with css using
#' the following style:
#'
#' \preformatted{
#' .sF-file .sF-file-icon .yourFileExtension{
#'     content: url(path/to/16x16/pixel/png);
#' }
#' .sF-fileList.sF-icons .sF-file .sF-file-icon .yourFileExtension{
#'     content: url(path/to/32x32/pixel/png);
#' }
#' }
#'
#' If no large version is specified the small version gets upscaled.
#'
#' \strong{Client side events}
#'
#' If the shiny app uses custom Javascript it is possible to react to selections
#' directly from the javascript. Once a selection has been made, the button will
#' fire of the event 'selection' and pass the selection data along with the
#' event. To listen for this event you simple add:
#'
#' \preformatted{
#' $(button).on('selection', function(event, path) {
#'     // Do something with the paths here
#' })
#' }
#'
#' in the same way a 'cancel' event is fired when a user dismisses a selection
#' box. In that case, no path is passed on.
#'
#' Outside events the current selection is available as an abject binded to the
#' button and can be accessed at any time:
#'
#' \preformatted{
#' // For a shinyFilesButton
#' $(button).data('files')
#'
#' // For a shinyDirButton
#' $(button).data('directory')
#'
#' // For a shinySaveButton
#' $(button).data('file')
#' }
#'
#' @param id The id matching the \code{\link{shinyFileChoose}}
#'
#' @param label The text that should appear on the button
#'
#' @param title The heading of the dialog box that appears when the button is
#' pressed
#'
#' @param multiple A logical indicating whether or not it should be possible to
#' select multiple files
#'
#' @param buttonType The Bootstrap button markup used to colour the button.
#' Defaults to 'default' for a neutral appearance but can be changed for another
#' look. The value will be pasted with 'btn-' and added as class.
#'
#' @param class Additional classes added to the button
#'
#' @param icon An optional \href{http://shiny.rstudio.com/reference/shiny/latest/icon.html}{icon} to appear on the button.
#'
#' @param filetype A named list of file extensions. The name of each element
#' gives the name of the filetype and the content of the element the possible
#' extensions e.g. \code{list(picture=c('jpg', 'jpeg'))}. The first extension
#' will be used as default if it is not supplied by the user.
#'
#' @return This function is called for its side effects
#'
#' @rdname shinyFiles-buttons
#' @name shinyFiles-buttons
#'
#' @family shinyFiles
#'
#' @references The file icons used in the file system navigator is taken from
#' FatCows Farm-Fresh Web Icons (\url{http://www.fatcow.com/free-icons})
#'
#' @importFrom htmltools tagList singleton tags
#'
#'
#'
shinyFilesButton <- function(id, label, title, multiple, buttonType='default', class=NULL, icon=NULL) {
  shiny::addResourcePath('sF', system.file('www', package='s3browser'))

  tagList(
    singleton(tags$head(
      tags$script(src='sF/shinyFiles.js'),
      tags$link(
        rel='stylesheet',
        type='text/css',
        href='sF/styles.css'
      ),
      tags$link(
        rel='stylesheet',
        type='text/css',
        href='sF/fileIcons.css'
      )
    )),
    tags$button(
      id=id,
      type='button',
      class=paste(c('shinyFiles btn', paste0('btn-', buttonType), class), collapse=' '),
      'data-title'=title,
      'data-selecttype'=ifelse(multiple, 'multiple', 'single'),
      list(icon, label)
    )
  )
}
