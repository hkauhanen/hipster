#' Data Frame to TeX Tabular
#'
#' Prints a data frame as a (La)TeX tabular.
#'
#' @param df Data frame
#' @param file File to write the table into
#' @param col.names Should column names be printed?
#' @param row.names Should row names be printed?
#' @param header Optional tabular header. If not specified, a default header of the form \code{\\tabular{r...r}} is used with as many columns as there are columns in the data frame.
#' @param before Optional vector of lines to write after header and (possible) column names but before data. Mostly useful to add custom column names, or an \code{\\hline}.
#' @param after Optional vector of lines to write before \code{\\end{tabular}}
#'
#' @export
df2tex <- function(df,
                   file,
                   col.names = TRUE,
                   row.names = TRUE,
                   header = "",
                   before = NULL,
                   after = NULL) {
  con <- file(file, "w")
  rnames <- row.names(df)
  cnames <- names(df)

  if (header != "") {
    writeLines(header, con)
  } else {
    toWrite <- "\\begin{tabular}{"
    if (row.names) {
      toWrite <- paste0(toWrite, "r")
    }
    for (i in 1:ncol(df)) {
      toWrite <- paste0(toWrite, "r")
    }
    toWrite <- paste0(toWrite, "}")
    writeLines(toWrite, con)
  }

  if (col.names & row.names) {
    writeLines(paste0(paste(paste0(" & ", cnames), collapse=""), " \\\\"), con)
  }

  if (col.names & !row.names) {
    if (ncol(df) > 1) {
      writeLines(paste0(cnames[1], paste(paste0(" & ", cnames[-1]), collapse=""), " \\\\"), con)
    } else {
      writeLines(paste0(cnames[1], " \\\\"), con)
    }
  }

  for (line in before) {
    writeLines(line, con)
  }

  for (i in 1:nrow(df)) {
    toWrite <- ""

    if (row.names) {
      toWrite <- paste0(toWrite, rnames[i], " & ")
    }

    toWrite <- paste0(toWrite, df[i,1])

    if (ncol(df) > 1) {
      for (j in 2:ncol(df)) {
        toWrite <- paste0(toWrite, " & ", df[i,j])
      }
    }

    toWrite <- paste0(toWrite, " \\\\")

    writeLines(toWrite, con)
  }

  for (line in after) {
    writeLines(line, con)
  }

  writeLines("\\end{tabular}", con)

  close(con)
}

