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
closure_creator <- function(a_level_string) {
  position_at_level_string <- 1
  level_string <- a_level_string
  level_size <- str_count(a_level_string)
  function(a_carry_string) {
    # the a_carry_string is being constructed as levels are visited
    if (position_at_level_string <= level_size) {
      nc <- substr( level_string, position_at_level_string, position_at_level_string )
      paste0( a_carry_string, nc )
    } else {
      a_carry_string
    }
  }
}


#=========================
# Predicates:
#=========================

is_maxed <- function(closure) {
  pos <- get( "position_at_level_string", environment(closure) )
  level_size <- get( "level_size", environment(closure))
  pos >= level_size
}

#=========================
# Helpers:
#=========================

level_is_maxed <- function(closure) {
  pos <- environment(closure)$position_at_level_string
  level_size <- environment(closure)$level_size
  pos >= level_size
}

# the closure must have been built using the function 'closure_creator' 
get_new_cur_string <- function(closure, carry_string) {
  closure(carry_string)
}

get_next_char <- function(closure) {
  str_level <- environment( closure )$level_string
  pos_level <- environment( closure )$position_at_level_string
  str_sub(str_level, pos_level, pos_level )
}

add_to_string_list <- function(string_list, cur_string) {
  if (cur_string != "") { 
    c(string_list, cur_string)
  } else {
    string_list
  }
}

decrease_pos_in_level <- function(closure) {
  cur_pos <- environment(closure)$position_at_level_string
  environment(closure)$position_at_level_string <- cur_pos - 1
}

increase_pos_in_level <- function(closure) {
  cur_pos <- environment(closure)$position_at_level_string
  environment(closure)$position_at_level_string <- cur_pos + 1
}

set_pos_in_level_to_one <- function(closure) {
  environment(closure)$position_at_level_string <- 1
}

trim_first_empty_from_string_list <- function(string_list) {
  if (head(string_list, 1) == "" & (length(string_list) > 1)) {
    unlist(tail(string_list, -1))
  } else {
    unlist(string_list)
  }
}

#=========================
# Recursion
#=========================

##########################################
## Function:               'recurse_on_position_at_level'
## Arguments:
##   1. closures:          list of closures that store in their environment the state of each level.
##                         The list of closures is created applying a list of strings to the function 'closure_creator'
##                         Note: a level has a level string, a level_string size, and
##                               a position within the level string. The level strings are like "abc". "xwyz", or ""
##   2. string_list:       used to store the strings of combinations of characters each from a different level (and all unique) 
##   3. level:             integer to keep track of the current position in the closures stack. 
##                         Ranges between 1 and length(closures)
##   4. cur_string:        keeps the string with the combination of characters from each level before it gets added to the
##                         string_list
##   5. advance_position:  boolean flag to indicate whether the position in the level string should be moved up
##
## Logic:
##
##    The recursive calls travel the closure stack up from the first level or down towards it.
##    The state at each step of the travel can be determined based on the following four constraints (labeled A -> D):
##
##    last level reached? ==> A 
##    advance_poistion = TRUE?  ==> B 
##    last character in level string reached? ==> C
##    first level reached? ==> D
##
##    =============================================================================================================================
##    A  |  B  |  C  |  D  | Action
##   ==============================================================================================================================
## 1  Y     Y     Y    Y   | Simple case: re-entry point after Y-Y-N-Y case and also the bottom clause for the one-level stack!
##   ------------------------------------------------------------------------------------------------------------------------------
## 2  Y     Y     Y    N   | Illegal case (should never be called because the last level never gets called with advance_position=T)
##   -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
## 3  Y     Y     N    Y   | Simple case: re-entry after Y-N-(Y|N)-Y case
##   ------------------------------------------------------------------------------------------------------------------------------
## 4  Y     Y     N    N   | Illegal case (should never be called because the last level never gets called with advance_position=T)
##   -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
## 5  Y     N   (Y|N)  Y   | Simple case: entry point to only one level stack. The character at the current position is red into 
##                         | 'cur_string' and added to the 'string_list' because there is only one level the strings have only
##                         | one character. The return value is set to the recursive call made with the current level
##   ------------------------------------------------------------------------------------------------------------------------------
## 6  Y     N     Y    N   | then the character at the current position is added to the 'cur_string', the 'cur_string' is added 
##                         | to the 'string_list', the 'position_at_level_string' is set to one, and then a recursive call 
##                         | is made with the level decreased by one, the 'cur_string' trimmed of its last two characters,
##                         | and the 'advance_position' set to TRUE
##   ------------------------------------------------------------------------------------------------------------------------------
## 7  Y     N     N    N   | then the character at the current position is added to the 'cur_string', the 'cur_string' is added 
##                         | to the 'string_list', the 'position_at_level_string' is increased by one, and then a recursive call 
##                         | is made with the level decreased by one, the 'cur_string' trimmed of its last two characters,
##                         | and the 'advance_position' set to FALSE (as it would have been called from a lower level)
##   ==============================================================================================================================
## 8  N     Y     Y    Y   | Bottom clause reached. Return 'string_list'
##   ------------------------------------------------------------------------------------------------------------------------------
## 9  N     Y     Y    N   | then set the 'position_at_level_string' to one and call itself with the 'cur_string' trimmed by one, 
##                         | the level decreased by one and the 'advance_position' set to TRUE (we are moving down the levels until
##                         | finding one than has a character available or we reach the first level)
##   -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
## 10 N     Y     N  (Y|N) | then increase 'position_at_level_string' by one, add the character at the current position to 
##                         | the 'cur_string' (which should be empty at this point), and call itself with the level up by one, 
##                         | the 'cur_string', and the 'advance_position' set to FALSE (every level above should have been reset 
##                         | to position 1 by this point)
##   -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
## 11 N     N  (Y|N) (Y|N) | then leave 'position_at_level_string' as is, add the character at the current position to 
##                         | the 'cur_string', call itself with level up by one, the 'cur_string', and the 'advance_position' 
##                         | set to FALSE
##   ==============================================================================================================================
recurse_on_position_at_level <- function(closures, string_list, level, cur_string, advance_position=F) {
  # this will determine if we have exhausted all combinations
  if (length(closures) == 0L) { return("") } # the trivial case...nothing to parse
  
  # compute this level's state
  last_level_reached <- (level == length(closures))
  if (advance_position) {
    increase_pos_in_level(closures[[level]])
  }
  last_char_in_level_string_reached = is_maxed(closures[[level]])
  first_level_reached <- (level == 1L)
  # CASES 2 & 4
  if (last_level_reached & advance_position & !first_level_reached) {
    stop("Illegal case: last level should never be called with 'advance_position'=T unless is a one-level closure stack")
  } else 
  # CASE 5
  if (last_level_reached & !advance_position & first_level_reached) { # length of the closure stack must be 1
    cur_string <- get_next_char(closures[[level]])
    string_list <- add_to_string_list(string_list, cur_string)
    final_string_list <- recurse_on_position_at_level(closures, string_list, level = 1, cur_string = "", advance_position = T)
    return(trim_first_empty_from_string_list(final_string_list))
  } else
  # CASE 1
  if (last_level_reached & advance_position & last_char_in_level_string_reached & first_level_reached) {
    cur_string <- get_next_char(closures[[level]])
    string_list <- add_to_string_list(string_list, cur_string)
    return(string_list) # the bottom clause for the case of one level in the closure stack
  } else
  # CASE 3
  if (last_level_reached & advance_position & !last_char_in_level_string_reached & first_level_reached) {
    cur_string <- get_next_char(closures[[level]])
    string_list <- add_to_string_list(string_list, cur_string)
    recurse_on_position_at_level(closures, string_list, level = level, cur_string = "", advance_position = T)
  } else
  # CASE 8 Bottom clause of the common case (closures stack size > 1)
  if (!last_level_reached & advance_position & last_char_in_level_string_reached & first_level_reached) {
    return(trim_first_empty_from_string_list(final_string_list))
  } else
  # CASE 6: A=Y     B=N     C=Y    D=N 
  if (last_level_reached & !advance_position & last_char_in_level_string_reached & !first_level_reached) {
    cur_string <- get_next_char(closures[[level]])
    string_list <- add_to_string_list(string_list, cur_string)
    set_pos_in_level_to_one(closures[[level]])
    cur_string <- str_sub(cur_string, 1, -3) # last two characters removed
    recurse_on_position_at_level(closures, string_list, level = level - 1, cur_string = cur_string, advance_position = T)
  } else
  
  # CASE 7: A=Y     B=N     C=N    D=N  
  if (last_level_reached & !advance_position & !last_char_in_level_string_reached & !first_level_reached) {
    
  } else
  
  # CASE 9  
  if (!last_level_reached & advance_position & last_char_in_level_string_reached & !first_level_reached) {
    
  } else
  
  # CASE 10:  A=N     B=Y     C=N  D=(Y|N)
  if (!last_level_reached & advance_position & !last_char_in_level_string_reached ) { 
    
  } else
  
  # CASE 11: A=N     B=N  C=(Y|N) D=(Y|N)
  if (!last_level_reached & !advance_position) {
    
  }
  
  ### cur_string <-  get_new_cur_string(closures[[level]], cur_string)
}


to_vector_of_strings <- function(aStrigOfDigits) {
  vector_of_digits <- to_vector_of_digits_from_string(aStrigOfDigits)
  vec_of_level_strings <- to_vector_of_target_characters( vector_of_digits )
  closures <- lapply(X = vec_of_level_strings, FUN = function(level_string ) closure_creator( level_string ))
  recurse_on_position_at_level(closures, list(""), 1, "")
}




# tests
#vos1 <- to_vector_of_strings("92")
#vos2 <- to_vector_of_strings("12")
#vos3 <- to_vector_of_strings("234")
#print(vos1)
#print(vos2)
#print(vos3)

#s_to_parse <- "43957"
s_to_parse <- "9"
print(paste0("Entering ", s_to_parse))
vos4 <- to_vector_of_strings(s_to_parse)
print(vos4)

