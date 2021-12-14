library(data.table)
DT <- data.table(digit = 0:9,
                 segment = c("a,b,c,e,f,g","c,f","a,c,d,e,g","a,c,d,f,g","b,c,d,f",
                             "a,b,d,f,g","a,b,d,e,f,g","a,c,f","a,b,c,d,e,f,g","a,b,c,d,f,g"))
DT[, nseg := sapply(strsplit(DT$segment, ","), length)]
DT[, digits_nseg := uniqueN(digit), by = nseg]
unique.lengths <- DT[digits_nseg == 1, ]$nseg


DT2 <- fread("./data/08a_input.txt", sep = "|", header = FALSE, col.names = c("pattern", "output"))
cols <- paste0("digit", 1:4)
DT2[, (cols) := tstrsplit(DT2$output, " ")][]
DT2[, paste0(cols, "_len") := lapply(.SD, nchar), .SDcols = cols]

sum( as.numeric(rbind(unlist(DT2[, .SD, .SDcols = paste0(cols, "_len")]))) %in% unique.lengths )
# 512
