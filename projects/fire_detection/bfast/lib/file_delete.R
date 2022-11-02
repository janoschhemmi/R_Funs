#' Delete file
#'
#' @title Delete file
#' @description Delete file after checking if it exists.
#' @param file_name character. Name of file to delete.
#' @return Nothing
#' @author Dirk Pflugmacher
#' @export
#' @md
#'
#
file_delete <- function(file_name) {
  if (file.exists(file_name)) file.remove(file_name)
}