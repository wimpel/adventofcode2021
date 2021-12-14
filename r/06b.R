state0 <- c(3,4,3,1,2)

DT <- fread("./data/06a_input.txt", header = FALSE)
state0 <- as.numeric(as.vector(DT[1,]))
state = state0

DT0 <- data.table(day = 0,
           state = state0 )

DT <- data.table(day = 0,state = 0:8, N = 0)[DT0[, .N, by = .(state)], N := i.N, on = .(state)]
#    day state N
# 1:   0     0 0
# 2:   0     1 1
# 3:   0     2 1
# 4:   0     3 2
# 5:   0     4 1
# 6:   0     5 0
# 7:   0     6 0
# 8:   0     7 0
# 9:   0     8 0
for (i in 1:256) {
  DT[, day := day + 1][]
  state0 <- DT[state == 0]$N
  DT[, N := shift(N, type = "lead", fill = state0)][]
  DT[state == 6, N := N + state0][]
}

format(sum(DT$N), scientific = FALSE)
#1574445493136
