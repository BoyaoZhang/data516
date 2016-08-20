#' @title Read data
#'
#' @description Read data for all formats.
#'
#' @param file [\code{character(1)}]\cr
#'   Give the file name with extenstions.
#'
#' @importFrom tools file_ext
#' @importFrom readr read_csv
#' @importFrom utils read.csv
#' @export

readData = function(file) {
  ext = tools::file_ext(file)

  if (ext == "csv")
    # large file
    if (file.size(file) > 10240000) # 10Mb
      output = readr::read_csv(file)
    else
      output = read.csv(file)

  return(output)
}
