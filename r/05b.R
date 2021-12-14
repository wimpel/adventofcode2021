DT <- fread("./data/05a_input.txt", sep = "", header = FALSE)

DT[, c("x1", "y1", "x2", "y2") := tstrsplit(gsub(" -> ", ",", V1), ",")]
DT[, line := .I][]
#get coordinates
ans <- DT[, .(x = seq.int(x1, x2),
              y = seq.int(y1, y2)), 
          by = .(line)]
#sum by coordinate, get values > 1
sum( ans[, .N, by = .(x, y)]$N > 1 )
# 21577