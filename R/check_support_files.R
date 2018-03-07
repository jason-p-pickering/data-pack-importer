#' @export
#' @importFrom compare compare
#' @title check_support_files(support_files_path)
#'
#' @description Checks DataPack support files to ensure they are compatible
#' with this version of the code.
#' @param support_files_path  The absolute file path to support files directory.
#' @return Returns a list of files which are not correct.
#'
check_support_files <- function(support_files_path) {
  want_md5s <- datapackimporter::support_files_md5
  support_files <- paste0(support_files_path, names(want_md5s))
  foo <- as.list(tools::md5sum(support_files))
  names(foo) <- basename(names(foo))
  not_good <- compare(want_md5s, foo)
  if (!not_good$result) {
    print(not_good$detailedResult)
    stop("Please ensure your support files are present and up to date!")
  } else {
    print("Support files look good.")
  }
}
