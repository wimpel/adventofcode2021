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
#draw all numbers, and decide if on which draw, which bord has a full row or column
BINGO <- FALSE
i <- 1
while (BINGO == FALSE) {
  waarden <- drawn[1:i]
  temp <- copy(DT)
  temp[, rijbingo := "nee"]
  temp[, colbingo := "nee"]
  temp[ temp[, Reduce( `&`, lapply( .SD, `%in%`, waarden ) ), .SDcols = rijen ], rijbingo := "ja"]
  temp[ temp[, Reduce( `&`, lapply( .SD, `%in%`, waarden ) ), .SDcols = kolommen ], colbingo := "ja"]
  temp[, getallen := list(waarden)] #getrokken getallen
  temp[, ronde := i]
  if (sum(temp$rijbingo == "ja") == 0 | sum(temp$colbingo == "ja") > 0) BINGO <- TRUE
  i <- i + 1
  }
#what does the winning board look like?
winning.board <- temp[board == temp[rijbingo == "ja"|colbingo == "ja", ]$board, ]
# numbers on the winning board
winning.board.numbers <- sapply(strsplit(paste0(
  apply(as.matrix(winning.board[,2:6]), 1, paste, collapse = ","), 
  collapse = ","), ","), as.numeric, simplify = TRUE)[,1]
#numbers draws for the winning board
drawn.numbers <- winning.board$getallen[[1]]
#solution
sum(winning.board.numbers[!winning.board.numbers %in% drawn.numbers]) * 
  tail(drawn.numbers, 1)
      