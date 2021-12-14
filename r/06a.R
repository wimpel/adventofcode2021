state0 <- c(3,4,3,1,2)

DT <- fread("./data/06a_input.txt", header = FALSE)
state0 <- as.numeric(as.vector(DT[1,]))
state = state0
for (i in 1:80) {
  temp <- state - 1
  zero.pos <- temp < 0
  #replace 0 by 6
  temp[zero.pos] <- 6
  #add 8
  state <<- c(temp, rep(8, sum(zero.pos)))
  #print(state)
}
length(state)
# 345387
