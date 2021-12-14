library(data.table)
# str <- "16,1,2,0,4,2,7,1,2,14\n"
# DT <- fread(str, header = FALSE)
DT <- fread("./data/07a_input.txt", header = FALSE)
positions <- as.numeric(as.vector(DT[1,]))

DT <- CJ(pos = positions, goal = seq(min(positions), max(positions), 1))
DT[, fuel := abs(goal - pos)]
DT[, .(fuel = sum(fuel)), by = goal][fuel == min(fuel)]         
#    goal   fuel
# 1:  349 356992