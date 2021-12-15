library(data.table)
library(igraph)

DT<- fread("./data/09a_input.txt", header = FALSE)
# DT <- fread("2199943210
# 3987894921
# 9856789892
# 8767896789
# 9899965678", header = FALSE)
m <- as.matrix(as.data.table(lapply(tstrsplit(DT$V1, ""), as.numeric)))
m <- unname(m)
#add rows/cols of value 9 as boundary
rows <- nrow(m)
cols <- ncol(m)
m <- cbind(rep(9,rows+2), rbind(rep(9,cols), m, rep(9,cols)), rep(9,rows+2))
# Indices of nonzero matrix elements
idx <- which(m != 9, arr.ind = TRUE)
# Adjacency matrix for matrix entries
# Two entries are adjacent if their column or row number differs by one
# Also, due to idx, an implicit condition is also that the two entries are the same
adj <- 1 * (as.matrix(dist(idx, method = "manhattan")) == 1)
# Creating loops as to take into account singleton islands
diag(adj) <- 1
# A corresponding graphs
g <- graph_from_adjacency_matrix(adj, mode = "undirected", )
# Connected components of this graph
islands <- clusters(g)
# final answer
prod(tail(sort(islands$csize), 3))
# 1038240

