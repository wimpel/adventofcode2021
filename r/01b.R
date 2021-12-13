library(data.table)
DT <- fread("./data/01a_input.txt", header = FALSE, col.names = "depth")
sum(diff(frollsum(DT$depth, n = 3)) > 0, na.rm = TRUE)
# [1] 1627

