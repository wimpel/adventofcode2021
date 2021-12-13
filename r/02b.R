library(data.table)
DT <- fread("./data/02a_input.txt", sep = " ", header = FALSE, col.names = c("direction", "value"))
# calculate aim
DT[, value2 := 0]
DT[direction == "down", value2 := value]
DT[direction == "up",   value2 := -1 * value]
DT[, aim := cumsum(value2)]
#depth
depth <- tail(DT[direction == "forward", .(cumsum(value * aim))]$V1, 1)
#[1] 692961
#forward
horizontal <- tail(cumsum(DT[direction == "forward"]$value), 1)
#[1] 1850
horizontal * depth
#[1] 1281977850
