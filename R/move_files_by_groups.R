#' Move Files to Different Folders Based on Patterns
#'
#' This function moves files into different folders based on specified patterns.
#'
#' @param patterns_list List of patterns to match files
#' @param folder_names Vector of folder names to move files to
#' @param type Character string specifying type: "files", "dirs", or "all"
#'
#' @return None
#' @export
#'
#' @examples
#' \dontrun{
#' # Move PDF files to PDF_Files folder and CSV files to CSV_Files folder
#' patterns <- list(c("pdf$"), c("csv$"))
#' folders <- c("PDF_Files", "CSV_Files")
#' move_files_by_groups(patterns, folders)
#' }
move_files_by_groups <- function(patterns_list, folder_names, type = "files") {
  if(length(patterns_list) != length(folder_names)) {
    stop("Length of patterns_list and folder_names must be equal")
  }

  if(type == "files") {
    all_items <- list.files(recursive = FALSE, include.dirs = FALSE)
  } else if(type == "dirs") {
    all_items <- list.dirs(recursive = FALSE, full.names = FALSE)
    all_items <- all_items[all_items != ""]
  } else {
    all_items <- list.files(recursive = FALSE, include.dirs = TRUE)
  }

  for(i in 1:length(patterns_list)) {
    dir.create(folder_names[i], showWarnings = FALSE)
    pattern <- paste(patterns_list[[i]], collapse="|")
    matched_items <- all_items[grepl(pattern, all_items)]

    if(length(matched_items) > 0) {
      file.copy(matched_items, file.path(folder_names[i], matched_items))
      file.remove(matched_items)
      cat("\nMoved", length(matched_items), "files to", folder_names[i], "folder:")
      cat("\n", paste(matched_items, collapse="\n"), "\n")
    } else {
      cat("\nNo files found matching", paste(patterns_list[[i]], collapse=" or "), "\n")
    }
  }
}
