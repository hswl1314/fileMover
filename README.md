# Install and load package
```r
devtools::install_github("hswl1314/fileMover")
library(fileMover)
```

# Example 1: Move files by patterns
```r
patterns_list <- list(
"txt",#keyword
"csv",#keyword
c("jpg", "png")#keyword
)
folder_names <- c("text", "data", "images")#TargetFolders
move_files_by_groups(patterns_list, folder_names)
```

# Example 2: Move all files from subdirectories to current directory
```r
#全部移动
move_all_files_up(target_folders=NULL)
move_all_files_up()
#指定移动
target_folders <- c("Control", "QC")
move_all_files_up(target_folders)
```
