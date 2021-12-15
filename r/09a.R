DT <- fread("2199943210
3987894921
9856789892
8767896789
9899965678", header = FALSE)

DT<- fread("./data/09a_input.txt", header = FALSE)
m <- as.matrix(as.data.table(lapply(tstrsplit(DT$V1, ""), as.numeric)))
m <- unname(m)

cols <- ncol(m)
rows <- nrow(m)
m <- cbind(rep(999,rows+2), rbind(rep(999,cols), m, rep(999,cols)), rep(999,rows+2))

values <- as.numeric()
for (x in 2:(ncol(m) - 1)) {
  for (y in 2:(nrow(m) - 1)) {
    val <- m[y,x]
    if (val < m[y-1,x] & val < m[y+1,x] & val < m[y,x-1] & val < m[y,x+1]) values <- c(values, val)
  }
}
sum(values + 1)
# 564
