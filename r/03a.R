library(data.table)
DT <- fread("./data/03a_input.txt", header = FALSE, col.names = "code", colClasses = "character")
# split binary character
DT[, paste0("val", sprintf("%02d", 1:12)) := lapply(tstrsplit(DT$code, ""), as.numeric)]
# calculate mean for each val, round 
ans <- DT[, .(mean = unlist(lapply(.SD, mean))), .SDcols = patterns("^^val")]
ans[, gamma   := round(mean, digits = 0)]
ans[, epsilon := round((1-mean), digits = 0)]
#convert binary to numeric
gamma   <- strtoi(paste0(ans$gamma, collapse = ""), base = 2)
epsilon <- strtoi(paste0(ans$epsilon, collapse = ""), base = 2)

gamma * epsilon
# [1] 2640986