library(data.table)
drawn <- as.numeric(paste0(fread("./data/04a_input.txt", 
                                 nrows = 1, header = FALSE, sep = ",")))
boards <- fread("./data/04a_input.txt", 
                skip = 1, header = FALSE, sep = "")
#split boards on empty rows
u <- rowSums(boards == "")
boards <- split(subset(boards,!u),cumsum(u)[!u])

L <- lapply(boards, function(x) {
  DT <- as.data.table(tstrsplit(x$V1, " +", perl = TRUE))
  cols <- names(DT)
  DT[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
})
#bind alle rijen en kolommen aan elkaar
rijen <- paste0("V", 1:5)
kolommen <- paste0("V", 6:10)
DT <- rbindlist(L, idcol = "board")[, (kolommen) := rbindlist(lapply(L, transpose))]
DT[, board := as.numeric(board) + 1]
#draw all numbers
L <- lapply(1:length(drawn), function(i) {
  waarden <- drawn[1:i]
  temp <- copy(DT)
  temp[, rijbingo := "nee"]
  temp[, colbingo := "nee"]
  temp[ temp[, Reduce( `&`, lapply( .SD, `%in%`, waarden ) ), .SDcols = rijen ], rijbingo := "ja"]
  temp[ temp[, Reduce( `&`, lapply( .SD, `%in%`, waarden ) ), .SDcols = kolommen ], colbingo := "ja"]
  temp[, getallen := list(waarden)] #getrokken getallen
  temp[, ronde := i]
})
#bind together
DT <- rbindlist(L)

DT2 <- DT[, .(bingowaarde = sum(rijbingo == "ja") + sum(colbingo == "ja")), by = .(board, ronde)]
ans <- DT2[, .(bingo = ifelse( max(bingowaarde) == 0, "nee", "ja")), by = .(board, ronde)]
ans <- ans[bingo == "ja", .(bingo.ronde = min(ronde)), by = .(board)][bingo.ronde == max(bingo.ronde), ]

ans
#    board bingo.ronde
# 1:    66          88

#what does the winning board look like?
winning.board <- DT[board == ans$board & ronde == ans$bingo.ronde, ]
# numbers on the winning board
winning.board.numbers <- sapply(strsplit(paste0(
  apply(as.matrix(winning.board[,2:6]), 1, paste, collapse = ","), 
  collapse = ","), ","), as.numeric, simplify = TRUE)[,1]
#numbers draws for the winning board
drawn.numbers <- winning.board$getallen[[1]]
#solution
sum(winning.board.numbers[!winning.board.numbers %in% drawn.numbers]) * 
  tail(drawn.numbers, 1)
