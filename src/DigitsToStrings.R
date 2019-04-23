# Utility funcions to turn a positive integer into a list of strings
# based on a given associatio between digits and chracters
# USAGE:
#    > los <- to_list_of_strings(222)
#    > los
#    > [1] "aaa" "aab" "aac" "aba" "abb" "abc" "aca" "acb" "acc" "baa" "bab" "bac" "bba" "bbb" "bbc" "bca" "bcb" "bcc" "caa" "cab" "cac"
#    >[22] "cba" "cbb" "cbc" "cca" "ccb" "ccc"
# The number of strings is 3^3 = 27, the number of combinations
# of characters when there are three possible characters per digit.

library(stringr)

# Useful maps:
characters_to_digit <- c(1,"a" = 2, "b" = 2,"c" = 2, "d" = 3, "e" = 3, "f" = 3,
                         "g" = 4, "h" = 4, "i" = 4, "j" = 5, "k" = 5, "l" = 5,
                         "m" = 6, "n" = 6, "o" = 6, "p" = 7, "q" = 7, "r" = 7, "s" = 7,
                         "t" = 8, "u" = 8, "v" = 8, "w" = 9, "x" = 9, "y" = 9, "z" = 9, "+" = 0)


digit_to_characters <- c("1" = "", "2" = "abc", "3" = "def", "4" = "ghi",
                         "5" = "jkl", "6" = "mno", "7" = "pqrs",
                         "8" = "tuv", "9" = "wxyz", "0" = "+")



# returs a vector of characters given a string of characters representing the series of digits
to_vector_of_digits_from_string <- function(aStringOfDigits) {
  vector_of_digits_as_chars <- strsplit( aStringOfDigits, split = "")
  unlist(vector_of_digits_as_chars)
}


# returns a vector of character strings corresponding to each of the digits given
# no assumptions made about the character string returned, e.g. "0" being mapped to "+"
to_vector_of_target_characters <- function(aVOfDigits) {
  # given ("0" "1" "9") as input it gives the output: 
  #     0     1        9
  #   "+"   ""    "wxyz"
  # Note: it uses the variable "digit_to_characters" defined at file scope
  sapply(aVOfDigits, function(x) digit_to_characters[x], USE.NAMES = F)
}


# Closure builder function.
# The closure keeps tabs on the enclosing environment when it is created
# that serves to maintain state between calls to the closure.
# A closure is a function with data (in contrast with the OO paradigm of a class as
# a container of data with functions).
level_visitor <- function(a_level_string) {
  position <- 1
  level_string <- a_level_string
  level_size <- nchar( a_level_string )
  maxed <- FALSE
  function(a_level, a_carry_string) {
    # the a_carry_string is being constructed as levels are visited
    if (level_size >= position) {
      # check next character
      nc <- substr( level_string, position, position )
      new_carry_string <- paste0( a_carry_string, nc )
      position <<- position + 1
    } else {
      new_carry_string <- a_carry_string
    }
    maxed <<- (level_size <= position)
    new_carry_string
  }
}

visit_level <- function(closure) {
  pos_level1 <- environment(closure)$position 
  level_str <- environment(closure)$level_string
  environment(closure)$position <- pos_level1 + 1
  if ( (pos_level1 + 1) == str_length(level_str) ) {
    environment(closure)$maxed <- TRUE
  }
  str_sub(level_str, pos_level1, pos_level1)
}

#=========================
# Predicates:
#=========================

is_maxed <- function(f) {
  maxed <- get( "maxed", environment(f) )
  maxed == TRUE
}

#=========================
# Helpers:
#=========================

are_all_maxed <- function(closures) {
  all_maxed_list <- Filter( x = closures, f = is_maxed )
  (length(all_maxed_list) == length(closures))
}

all_above_are_maxed <- function(closures) {
  funcs_above <- closures[2:length(closures)]
  all_above_maxed <- Filter(x = funcs_above, f = is_maxed)
  (length(all_above_maxed) == length(funcs_above))  
}

reset <- function(closures) {
  lapply( X = closures, FUN = function(x) { 
    maxed <- environment(x)$maxed
    if (maxed) { 
      environment(x)$maxed <- FALSE
      environment(x)$position <- 1
    } 
  })
}

#=========================
# Recursion
#=========================

recurse_on_levels_at_this_position <- function(closures, char_level1) {
  clsr_above <- closures[2:length(closures)]
  char_list <- lapply(X = clsr_above, FUN = function(clsr)  visit_level(clsr)) 
  paste0(char_level1, char_list)
}

recurse_on_position_at_level_1 <- function(closures, string_list=list("")) {
  # this will determine if we have exhausted all combinations
  all_maxed <- are_all_maxed( closures )
  all_above_are_maxed <- all_above_are_maxed( closures )

  if ( all_maxed ) {
    return(string_list) # successful case of the recursion
  }
  else {
    reset(closures[2:length(closures)])
    
    if (all_above_are_maxed) {
      pos_level1 <- environment( closures[[1]] )$position
      environment( closures[[1]] )$position <- pos_level1 + 1
    }
    str_level1 <- environment(closures[[1]])$level_string
    pos_level1 <- environment( closures[[1]] )$position
    char_level1 <- str_sub(str_level1, pos_level1, pos_level1 )
    if (pos_level1 >= str_length(str_level1)) {
      environment(closures[[1]])$maxed <- TRUE
    }  
    new_string <- recurse_on_levels_at_this_position(closures, char_level1)
    if ( (is.list(string_list) & (string_list[[1]] == "")) |
         (is.character(string_list) & string_list[1] == "")) {
      string_list <- new_string
    } else {
      string_list <- c( string_list, new_string )
    }
    # recursive case
    recurse_on_position_at_level_1( closures, string_list )
  }
}


to_vector_of_strings <- function(aStrigOfDigits) {
  vector_of_digits <- to_vector_of_digits_from_string(aStrigOfDigits)
  vec_of_level_strings <- to_vector_of_target_characters( vector_of_digits )
  level_closures <- lapply(X = vec_of_level_strings, FUN = function(level_string ) level_visitor( level_string ))
  recurse_on_position_at_level_1(level_closures)
}




# tests
vos1 <- to_vector_of_strings("92")
vos2 <- to_vector_of_strings("12")
vos3 <- to_vector_of_strings("234")
print(vos1)
print(vos2)
print(vos3)

vec_of_strings <- to_vector_of_strings("92")
print(vec_of_strings)
level_funs_test <- lapply(vec_of_strings, FUN = function(level_string) level_visitor(level_string))
level_funs_test[[1]](1,"")
p2 <- environment( level_funs_test[[2]] )$position
m2 <- environment( level_funs_test[[2]] )$maxed
print(m2)
environment( level_funs_test[[2]] )$maxed <- TRUE
m2_p <- environment( level_funs_test[[2]] )$maxed
print("After changing maxed in function 2")
print(m2_p)


l1 <- lapply(X = level_funs_test, FUN = is_maxed)

msg <- paste0("(", as.character(l1[[1]]), ",", as.character(l1[[2]]), ")")
print(paste("The maxed state in the vector of functions: ", msg ))

am1 <- are_all_maxed( level_funs_test )
print(paste0("All maxed: ", am1))

am2 <- all_above_are_maxed( level_funs_test )
print(paste0("All above maxed: ", am2))

reset(level_funs_test)
am2 <- all_above_are_maxed( level_funs_test )
print(paste0("All maxed after reset: ", am1))
