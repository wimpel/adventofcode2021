library(data.table)
DT <- fread("./data/02a_input.txt", sep = " ", header = FALSE, col.names = c("direction", "value"))
# sum value by direction
ans <- DT[, .(total = sum(value, na.rm = TRUE)), by = .(direction)]
#    direction total
# 1:   forward  1850
# 2:      down  2026
# 3:        up  1099

hor   <- ans[direction == "forward", total]
depth <- ans[direction == "down", total] - ans[direction == "up", total]

hor * depth
# [1] 1714950