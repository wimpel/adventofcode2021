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
  for (i in 2:length(s)) {
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
  if (length(error.value) == 0) list(NA,NA) else list(val,i)
})
L

lookup <- data.table(chr = c(")", "]", "}", ">"),
                     val = c(3,57,1197,25137))
  
final <- data.table( char = unlist(lapply(L, `[[`, 1)),
            pos = unlist(lapply(L, `[[`, 2)))[lookup, val := i.val, on = .(char = chr)][]

sum(final$val, na.rm = TRUE)

