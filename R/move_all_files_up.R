#' Move All Files from Subdirectories to Current Directory
#'
#' This function moves all files from specified subdirectories to the current directory
#' and then removes the empty subdirectories.
#'
#' @param target_folders Optional character vector of specific folder names to process
#'
#' @return None
#' @export
#'
#' @examples
#' \dontrun{
#' # Move all files from all subdirectories
#' move_all_files_up()
#'
#' # Move files only from specific folders
#' move_all_files_up(c("folder1", "folder2"))
#' }
move_all_files_up <- function(target_folders = NULL) {
  # Get all folders in current directory
  folders <- list.dirs(path = ".", recursive = FALSE)

  # Process only specified folders if target_folders is provided
  if (!is.null(target_folders)) {
    folders <- folders[basename(folders) %in% target_folders]
    if (length(folders) == 0) {
      message("No specified folders found")
      return(invisible())
    }
  }

  message("Processing the following folders:")
  message(paste(basename(folders), collapse = "\n"))

  # Process each folder
  for(folder in folders) {
    files <- list.files(folder, full.names = TRUE)

    if(length(files) > 0) {
      file_names <- basename(files)
      file.copy(files, file_names)
      file.remove(files)

      message(sprintf("\nMoved %d files from %s to current directory:",
                      length(files), basename(folder)))
      message(paste(file_names, collapse = "\n"))
    } else {
      message(sprintf("\nFolder %s is empty", basename(folder)))
    }

    # Remove the processed folder
    unlink(folder, recursive = TRUE)
  }

  message("\nAll files moved successfully")
  invisible()
}
