#' Concatenate CSVs
#'
#' Row-bind several CSV files into one dataframe.
#'
#' @param folder Folder in which the CSV files live
#' @param pattern Pattern which included files are to match
#' @return A data frame
#'
#' @importFrom utils read.csv setTxtProgressBar txtProgressBar
#' @export
cat_csv <- function(folder,
                    pattern) {
  tmpfile <- tempfile()
  files <- list.files(folder, pattern=pattern, full.names=TRUE)
  if (length(files) == 1) {
    warning("Only one matching file - nothing to concatenate!")
    return(read.csv(files[[1]]))
  }
  cat("Concatenating...\n")
  pb = txtProgressBar(min = 0, max = length(files), initial = 0, style=3)
  system(paste("cp", files[[1]], tmpfile))
  setTxtProgressBar(pb,1)
  for (i in 2:length(files)) {
    system(paste("sed '1d'", files[[i]], ">>", tmpfile))
    setTxtProgressBar(pb,i)
  }
  cat("\nReading temporary file into data frame...")
  out <- read.csv(tmpfile)
  cat(" done!\nData frame created: ")
  cat(paste0(nrow(out), " rows, ", ncol(out), " columns.\n"))
  unlink(tmpfile)
  out
}


#' Concatenate CSVs inside a .tar.gz
#'
#' Row-bind several CSV files that live inside a tar.gz archive. The
#' \code{csv_folder} argument specifies an optional directory within the
#' archive that contains the CSV files.
#'
#' @param gzfile Archive
#' @param pattern Pattern which the CSV files are to match
#' @param csv_folder Folder within the archive in which the CSVs are found
#' @return Data frame
#'
#' @importFrom utils read.csv setTxtProgressBar txtProgressBar
#' @export
cat_csv.gz <- function(gzfile,
                       pattern,
                       csv_folder = ".") {
  td <- tempdir()
  system(paste0("tar xzf ", gzfile, " -C ", td))
  innerfolder <- unlist(strsplit(gzfile, "/"))
  innerfolder <- innerfolder[length(innerfolder)]
  innerfolder <- unlist(strsplit(innerfolder, "\\."))[1]
  cat_csv(folder=paste0(td, "/", innerfolder, "/", csv_folder), pattern=pattern)
}

