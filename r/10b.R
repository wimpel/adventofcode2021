library(data.table)
DT <- fread("[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]", header = FALSE, sep = "")


DT <- fread("./data/10a_input.txt", header = FALSE, sep = "")
DT[, s := lapply(sapply(DT$V1, strsplit, ""), as.vector)][]

s1 <- character()

L <- lapply(DT$s, function(s){
  s <- unlist(s)
  error.value <- character()
  for (i in 1:length(s)) {
    #if s1 is empty, fill with value
    if (length(s1) == 0 ) {
      #browser()
      s1[1] <- s[i]
      next
    } else {
      #browser()
      val <- s[i]
      valp <- s1[length(s1)]
      #if new value matches last value
      if ((val == ")" & valp == "(") | 
          (val == "}" & valp == "{") | 
          (val == "]" & valp == "[") |
          (val == ">" & valp == "<")) {
        #browser()
        if(length(s1) == 1) {
          #browser()
          s1 <- character()
        } else {
          #browser()
          s1 <- s1[1:(length(s1) - 1)]
        }
      } else {
        #browser()
        #check if is closure character >> error found
        if (val %in% c(">", "}", "]", ")")) {
          #browser()
          error.value <- val
          break
        } else {
          #browser()
          s1 <- c(s1, val)
        }
      }
    }
    #  print(s1)
  }
  #browser()
  if (length(error.value) == 0) s1 else NA
})

L2 <- Filter(Negate(anyNA), L)

DT2 <- data.table(str = sapply(L2, paste0, collapse = ""))
DT2[, s := lapply(sapply(str, strsplit, ""), as.vector)][]

L3 <- sapply(DT2$s, function(s) {
  s <- unlist(s)
  score <- 0
  for (i in length(s):1) {
    val <- s[i]
    if (val == "(") score <- score * 5 + 1
    if (val == "[") score <- score * 5 + 2
    if (val == "{") score <- score * 5 + 3
    if (val == "<") score <- score * 5 + 4
  }
  return(score)
})

median(L3)

