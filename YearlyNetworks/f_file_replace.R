library(gsubfn)
file_find_replace <- function(source_path, target_path, toreplace) {
  file_contents <- readLines(source_path)
  updated_contents <- gsubfn(paste(names(toreplace), collapse="|"), toreplace, x= file_contents)
  # write_lines(updated_contents, file = target_path, sep = "\n")
  cat(updated_contents, file = target_path, sep = "\n")
  
}

