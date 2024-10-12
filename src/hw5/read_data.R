library("tidyverse")

read_method_1 <- function(file1, file2) {
  d1 <- read.table(file1, sep=";", header=TRUE)
  d2 <- read.table(file2, sep=";", header=TRUE)

  d3 <- merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
  return (d3)
}

read_method_2 <- function(file1, file2) {
  d1 <- read_delim(file1, delim=";", show_col_types = FALSE)
  d2 <- read_delim(file2, delim=";", show_col_types = FALSE)

  join_on_columns <- names(d1)[!names(d1) %in% c("G1", "G2", "G3", "paid", "absences")]
  d3 <- inner_join(d1, d2, join_on_columns, suffix = c("_math", "_lang"))
  return (list(d1=d1, d2=d2, combined=d3))
}
