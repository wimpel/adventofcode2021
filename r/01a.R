library(data.table)
DT <- fread("./data/01a_input.txt", header = FALSE, col.names = "depth")
sum(diff(DT$depth) > 0)
# [1] 1583