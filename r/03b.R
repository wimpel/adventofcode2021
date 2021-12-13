library(data.table)
DT <- fread("./data/03a_input.txt", header = FALSE, col.names = "code", colClasses = "character")
# split binary character
DT[, paste0("val", sprintf("%02d", 1:12)) := lapply(tstrsplit(DT$code, ""), as.numeric)]
# initialise filtered dataset
oxy <- copy(DT)
#
for (i in 1:12) {
  val <- eval(parse(text = sprintf( "mean(oxy[, val%02d])", i)))
  val.use <- ifelse(val == 0.5, 1, ifelse(val < 0.5, 0, 1))
  oxy <<- eval(parse(text = sprintf( "oxy[val%02d == %d,]", i, val.use)))
  print(nrow(oxy))
}

co <- copy(DT)
for (i in 1:9) {
  val <- eval(parse(text = sprintf( "mean(co[, val%02d])", i)))
  val.use <- ifelse(val == 0.5, 0, ifelse(val < 0.5, 1, 0))
  co <<- eval(parse(text = sprintf( "co[val%02d == %d,]", i, val.use)))
  print(nrow(co))
}

oxy <- oxy$code
co2 <- co$code

strtoi(oxy, base = 2) * strtoi(co2, base = 2)
# 6822109
