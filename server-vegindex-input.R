# Check if vegindex input contains any unallowed characters Allow only 
# expressions that consist of basic arithmetic operators (+-/*), numerals (0-9),
# regular parantheses "()" and the letter R to denote a reflectance value. This
# is neccessary for now, as hsdar's built-in vegindex() func will run any code
# passed to it as a string. Allowing for the execution of malicious code.
parse_vegindex_input <- function(vegindex_input) {
  allowed_chars <- c("R", 0:9, " ", "(" ,")", "/", "+", "-", "*")
  iterable_input <- vegindex_input %>%  
    strsplit(split = "") %>% unlist()
  checked_chars <- vapply(iterable_input, 
                          FUN = function(x) x %in% allowed_chars,
                          FUN.VALUE = logical(1), 
                          USE.NAMES = FALSE) 
  all(checked_chars)
}
