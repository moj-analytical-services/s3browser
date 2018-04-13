#' s3time to shinyfiles time convertor
#'
#' @param string containing s3 time
#'
#' @return shinyfile compatible time strings
#'
#'
#' @examples s3time_to_shinyfiles(s3times)
s3time_to_shinyfiles <- function(time){
  time <- stringr::str_sub(time,end = -6)
  time <- stringr::str_replace_all(time, 'T', '-')
  time <- stringr::str_replace_all(time, ':', '-')
  return(time)
}



#' Title
#'
#' @param bucket_contents contents of a bucket from s3tools
#'
#' @return df containing everything required for shiny files

#' @importFrom magrittr %>%
#' @examples bucket_contents_to_shiny_files_df(bucket_contents)
bucket_contents_to_shiny_files_df <- function (bucket_contents) {

  df <- bucket_contents %>%
    purrr::map(unclass) %>%
    purrr::map(dplyr::as_data_frame) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(path = paste(Bucket, Key, sep = "/")) %>%
    dplyr::mutate(size_readable = gdata::humanReadable(Size)) %>%
    dplyr::mutate(filename = basename(path)) %>%
    dplyr::mutate(mtime = s3time_to_shinyfiles(LastModified))%>%
    dplyr::mutate(ctime = mtime, atime = mtime) %>%
    dplyr::select(filename, Size, mtime, ctime, atime, path, size_readable, dplyr::everything())

  names(df) <- tolower(names(df))
  return(df)
}



aws_get_bucket_memoised <- memoise::memoise(aws.s3::get_bucket)

#' Title
#'
#' @param current_path
#'
#' @return df with required shinyfiles fields

#' @importFrom magrittr %>%
#' @examples s3_dir_shiny_files('bucket/dir/')
s3_dir_shiny_files <- function(current_path=''){

  message('fetching file tree...')

  if(current_path == '') {
      file_list <- dplyr::tibble(bucket = s3tools::accessible_buckets())
      file_list["path"] <- paste0(file_list$bucket, "/")
      file_list["key"] <- file_list["bucket"]
      file_list["size"] <- 10
      file_list["mtime"] <- as.POSIXct("2000-01-01-00-00-00")
      file_list["ctime"] <- as.POSIXct("2000-01-01-00-00-00")
      file_list["atime"] <- as.POSIXct("2000-01-01-00-00-00")
      file_list["size_readable"] <- 10
      file_list["lastmodified"] <- as.POSIXct("2000-01-01-00-00-00")
      file_list["etag"] <- "etag"
      file_list["owner"] <- "owner"
      file_list["storageclass"] <- "STANDARD"
    } else {

    if (substr(current_path,1,1) == "/") {
      current_path <- substr(current_path,2, nchar(current_path))
    }

    bucket <- sub("/.+","",current_path)

    bucket_contents <- aws_get_bucket_memoised(bucket)

    if (length(bucket_contents) > 0) {
      print("length more than 0")
      file_list <- bucket_contents_to_shiny_files_df(bucket_contents) %>%
        dplyr::mutate(owner = as.character(owner))
    } else {
      print("length equal to 0")
      file_list <- dplyr::tibble(bucket = c(paste0(current_path, "/")))
      file_list["path"] <- paste0(file_list$bucket, "/")
      file_list["key"] <- file_list["bucket"]
      file_list["size"] <- 10
      file_list["mtime"] <- as.POSIXct("2000-01-01-00-00-00")
      file_list["ctime"] <- as.POSIXct("2000-01-01-00-00-00")
      file_list["atime"] <- as.POSIXct("2000-01-01-00-00-00")
      file_list["size_readable"] <- 10
      file_list["lastmodified"] <- as.POSIXct("2000-01-01-00-00-00")
      file_list["etag"] <- "etag"
      file_list["owner"] <- "owner"
      file_list["storageclass"] <- "STANDARD"
    }
  }




  #if path doesn't end with / then add it
  current_path <- ifelse(stringr::str_sub(current_path, -1)!='/',
                         stringr::str_c(current_path,'/'), current_path)

  current_path <- ifelse(current_path=='/', '', current_path)

  #If it starts with a / then get rid of it (this breaks shinyFilesBrowser)
  current_path <-ifelse(stringr::str_sub(current_path,1,1)=='/',
                        stringr::str_sub(current_path, 2), current_path)

  #Subset to input folder and format for shinyfiles

  file_list <- file_list %>%
    dplyr::filter(stringr::str_detect(path, current_path)) %>%
    dplyr::mutate(path =
                    stringr::str_sub(path,
                                     start = stringr::str_length(current_path)+1)) %>%
    dplyr::mutate(tree = stringr::str_split(path, '/')) %>%
    dplyr::mutate(tree = tree %>% purrr::map(1) %>% unlist(.)) %>%
    dplyr::distinct(tree, .keep_all = TRUE) %>%
    dplyr::mutate(filename = tree) %>%
    dplyr::mutate(extension = tools::file_ext(filename)) %>%
    dplyr::mutate(isdir = ifelse(extension=='', TRUE, FALSE))%>%
    dplyr::filter(filename!="") %>%
    dplyr::filter(filename!=".DS_Store")%>%
    dplyr::select(filename, extension, isdir, size, mtime, ctime, atime, path, size_readable, dplyr::everything())

  return(file_list)
}
s3_dir_shiny_files <- memoise::memoise(s3_dir_shiny_files)

#' Title
#'
#' @param roots roots from shinyfiles server addins
#' @param restrictions not used
#' @param filetypes not used
#' @param hidden not used
#'
#' @return a list compatable with shinyfiles

#'
#' @examples do.call(s3_dir_shiny_files_list, list(...))
s3_dir_shiny_files_list <- function(roots, restrictions, filetypes, hidden=FALSE) {
  if (missing(filetypes)) filetypes <- NULL
  if (missing(restrictions)) restrictions <- NULL

  function(dir, root) {

    currentRoots <- if(class(roots) == 'function') roots() else roots

    if (is.null(names(currentRoots))) stop('Roots must be a named vector or a function returning one')
    if (is.null(root)) root <- names(currentRoots)[1]


    fileInfo <- s3_dir_shiny_files(dir)

    rownames(fileInfo) <- NULL
    breadcrumps <- strsplit(dir, .Platform$file.sep)[[1]]
    list(
      files=fileInfo[, c('filename', 'extension', 'isdir', 'size', 'mtime', 'ctime', 'atime')],
      writable=TRUE,
      exist=TRUE,
      breadcrumps=I(c('', breadcrumps[breadcrumps != ''])),
      roots=I(names(currentRoots)),
      root=root,
      fulldir=dir
    )
  }
}




