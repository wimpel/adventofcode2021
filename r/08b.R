library(data.table)
sol <- data.table(digit = 0:9,
                 segment = c("a,b,c,e,f,g","c,f","a,c,d,e,g","a,c,d,f,g","b,c,d,f",
                             "a,b,d,f,g","a,b,d,e,f,g","a,c,f","a,b,c,d,e,f,g","a,b,c,d,f,g"))
sol[, segment := gsub(",", "", segment)]
sol[, segments.v := sapply(strsplit(sol$segment, ""), as.vector)]
sol[, nseg := sapply(strsplit(sol$segment, ""), length)]
sol[, digits_nseg := uniqueN(digit), by = nseg]
#    digit segment      segments.v nseg digits_nseg
# 1:     0  abcefg     a,b,c,e,f,g    6           3
# 2:     1      cf             c,f    2           1
# 3:     2   acdeg       a,c,d,e,g    5           3
# 4:     3   acdfg       a,c,d,f,g    5           3
# 5:     4    bcdf         b,c,d,f    4           1
# 6:     5   abdfg       a,b,d,f,g    5           3
# 7:     6  abdefg     a,b,d,e,f,g    6           3
# 8:     7     acf           a,c,f    3           1
# 9:     8 abcdefg a,b,c,d,e,f,...    7           1
#10:     9  abcdfg     a,b,c,d,f,g    6           3

#puzzle input
DT <- fread("./data/08a_input.txt", sep = "|", header = FALSE, col.names = c("pattern", "output"))
#DT <- fread("./data/test.txt", sep = "|", header = FALSE, col.names = c("pattern", "output"))
cols <- paste0("digit", 1:4)
DT[, (cols) := tstrsplit(DT$output, " ")][]
DT[, paste0(cols, "_len") := lapply(.SD, nchar), .SDcols = cols]
DT[, paste0(cols, ".v") := lapply(.SD, strsplit, ""), .SDcols = cols][]

DT[, final_solution := lapply(seq.int(nrow(DT)), function(i){
temp <- data.table( pattern = tstrsplit(DT$pattern[i], " "))
temp[, length := nchar(pattern)]
temp <- sol[temp, on = .(nseg = length), allow.cartesian = TRUE]
temp <- temp[, .(code = paste0(pattern, collapse = ","), segments = segments.v[1]), 
         by = .(digit, segment)]
cols <- paste0("code", 1:3) 
temp[, (cols) := tstrsplit(code, ",")]
temp[, (cols) := lapply(.SD, function(x) sapply(strsplit(x, ""), as.vector)), .SDcols = cols]
#find 1
temp <- melt(temp, measure.vars = patterns("^code[1-3]"), na.rm = TRUE, value.name = "code2")
temp <- temp[!is.na(code2), ]
temp[, segments := sapply(segments, as.vector)]
temp[sapply(segments, length) == 2, solution := 1]
temp[sapply(segments, length) == 3, solution := 7]
temp[sapply(segments, length) == 4, solution := 4]
temp[sapply(segments, length) == 7, solution := 8]
temp[is.na(solution) & sapply(segments, length) == 5 & sapply(code2, function(x) sum(x %in% temp[solution == 7,]$code2[[1]]) == 3), solution := 3]
temp[is.na(solution) & sapply(segments, length) == 6 & sapply(code2, function(x) sum(x %in% temp[solution == 4,]$code2[[1]]) == 4), solution := 9]
temp[is.na(solution) & sapply(segments, length) == 5 & sapply(code2, function(x) sum(x %in% temp[solution == 9,]$code2[[1]]) == 5), solution := 5]
temp[is.na(solution) & sapply(segments, length) == 5, solution := 2]
temp[is.na(solution) & sapply(segments, length) == 6 & sapply(code2, function(x) sum(x %in% temp[solution == 5,]$code2[[1]]) == 5), solution := 6]
temp[is.na(solution), solution := 0]

rosetta <- temp[!duplicated(code2), .(code = code2, solution)]
rosetta
sol1 <- rosetta[sapply(code, length) == length(unlist(DT$digit1.v[i])) &
          sapply(code, function(x) sum(x %in% unlist(DT$digit1.v[i]))) == length(unlist(DT$digit1.v[i])), ]$solution
sol2 <- rosetta[sapply(code, length) == length(unlist(DT$digit2.v[i])) &
                  sapply(code, function(x) sum(x %in% unlist(DT$digit2.v[i]))) == length(unlist(DT$digit2.v[i])), ]$solution
sol3 <- rosetta[sapply(code, length) == length(unlist(DT$digit3.v[i])) &
                  sapply(code, function(x) sum(x %in% unlist(DT$digit3.v[i]))) == length(unlist(DT$digit3.v[i])), ]$solution
sol4 <- rosetta[sapply(code, length) == length(unlist(DT$digit4.v[i])) &
                  sapply(code, function(x) sum(x %in% unlist(DT$digit4.v[i]))) == length(unlist(DT$digit4.v[i])), ]$solution

as.numeric(paste0(sol1, sol2, sol3, sol4))
})]

sum(unlist(DT$final_solution))

    