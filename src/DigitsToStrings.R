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
  level_size <- str_count( a_level_string )
  function(a_carry_string) {
    # the a_carry_string is being constructed as levels are visited
    nc <- substr( level_string, position_at_level_string, position_at_level_string )
    paste0( a_carry_string, nc )
  }
}


#=========================
# Predicates:
#=========================

is_maxed <- function(closure) {
  pos <- get( "position", environment(closure) )
  level_size <- get( "level_size", environment(closure))
  pos >= level_size
}

#=========================
# Helpers:
#=========================

level_is_maxed <- function(closures, level) {
  pos <- environment(closures[[level]])$position
  level_size <- environment(closures[[level]])$level_size
  pos >= level_size
}

are_all_maxed <- function(closures) {
  all_maxed_list <- Filter( x = closures, f = is_maxed )
  (length(all_maxed_list) == length(closures))
}

are_all_above_maxed <- function(closures, level) {
  level_above <- level + 1
  if ( level_above > length( closures ) ) {
    TRUE
  } else {
    closures_above <- closures[level_above:length(closures)]
    all_above_maxed <- Filter(x = closures_above, f = is_maxed)
    (length(all_above_maxed) == length(closures_above))      
  }
}


reset_closures_if_maxed <- function(closures) {
  lapply( X = closures, FUN = function(x) { 
    maxed <- is_maxed(x)
    if (maxed) { 
      environment(x)$position <- 1
    } 
  })
}

reset_all_above <- function(closures, level_above) {
  closures_above <- closures[level_above:length(closures)]
  reset_closures_if_maxed(closures_above)
}


reset_all_above_if_maxed <- function(closures, level) {
  level_above <- level + 1
  if ( level_above <= length( closures ) ) {
    reset_all_above( closures, level_above )
  }
}


  
get_next_char <- function(closures, level) {
  str_level <- environment( closures[[level]] )$level_string
  pos_level <- environment( closures[[level]] )$position
  str_sub(str_level, pos_level, pos_level )
}

decrease_pos_in_level <- function(closures, level) {
  cur_pos <- environment(closures[[level]])$position
  environment(closures[[level]])$position <- cur_pos - 1
}

increase_pos_in_level <- function(closures, level) {
  cur_pos <- environment(closures[[level]])$position
  environment(closures[[level]])$position <- cur_pos + 1
}

#safely calculate the next level value up or down from the current
increase_level <- function(closures, level) {
  if ( (level >= 1) & (level < length(closures )) )  {
    level + 1
  } else if (level < 1) {
    1
  } else {
    length(closures)
  }
}

decrease_level <- function(closures, level) {
  if ( (level > 1) & (level <= length(closures)) ) {
    level - 1
  } else if ( level > length(closures)) {
    length(closures)
  } else {
    1
  }
}


#=========================
# Recursion
#=========================

##########################################
## Functionality:
## 1. closures:    list of closures that store in their environment the state of each level
##                 Note: a level is made up of a level string, a level_string size, and
##                    a position within the level string. The level strings are like "abc". "xwyz", or ""
## 2. string_list: used to store the strings of combinations of characters from each level (all unique) 
## 3. level:       keeps track of the current lposition within the closures. Ranges between 1 and length(closures)
## 4. cur_string:  keeps the string with the combination of characters from each level before it gets added to the
##                 string_list upon completion of a combination.
##
## Logic:
##
##    last level reached? ==> A 
##    advance_poistion = TRUE?  ==> B 
##    last character in level string reached? ==> C
##    first level reached? ==> D
##
##    A  |  B  |  C  |  D  | Action
##   ==============================================================================================================================
##    Y     Y     Y  (Y|N) | Illegal case (should never be called) 
##   -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
##    Y     Y     N  (Y|N) | Illegal case (should never be called)
##   -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
##    Y     N     Y    Y   | Illegal case (should never be called)
##   ------------------------------------------------------------------------------------------------------------------------------
##    Y     N     Y    N   | then the character at the current position is added to the 'cur_string', the 'cur_string' is added 
##                         | to the 'string_list', the 'position_at_level_string' is set to one, and then a recursive call 
##                         | is made with the level decreased by one, the 'cur_string' trimmed of its last two characters,
##                         | and the 'advance_position' set to TRUE
##   ------------------------------------------------------------------------------------------------------------------------------
##    Y     N     N    Y   | Illegal case (should never be called)
##   ------------------------------------------------------------------------------------------------------------------------------
##    Y     N     N    N   | then the character at the current position is added to the 'cur_string', the 'cur_string' is added 
##                         | to the 'string_list', the 'position_at_level_string' is increased by one, and then a recursive call 
##                         | is made with the level decreased by one, the 'cur_string' trimmed of its last two characters,
##                         | and the 'advance_position' set to FALSE (as it would have been called from a lower level)
##   ==============================================================================================================================
##    N     Y     Y    Y   | Bottom clause reached. Return 'string_list'
##   ------------------------------------------------------------------------------------------------------------------------------
##    N     Y     Y    N   | then set the 'position_at_level_string' to one and call itself with the 'cur_string' trimmed by one, 
##                         | the level decreased by one and the 'advance_position' set to TRUE (we are moving down the levels until
##                         | finding one than has a character available or we reach the first level)
##   -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
##    N     Y     N  (Y|N) | then increase 'position_at_level_string' by one, add the character at the current position to 
##                         | the 'cur_string' (which should be empty at this point), and call itself with the level up by one, 
##                         | the 'cur_string', and the 'advance_position' set to FALSE (every level above should have been reset 
##                         | to position 1 by this point)
##   -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
##    N     N  (Y|N) (Y|N) | then leave 'position_at_level_string' as is, add the character at the current position to 
##                         | the 'cur_string', call itself with level up by one, the 'cur_string', and the 'advance_position' 
##                         | set to FALSE
##   ==============================================================================================================================
##########################################
recurse_on_position_at_level <- function(closures, string_list, level, cur_string) {
  # this will determine if we have exhausted all combinations
  all_maxed <- are_all_maxed( closures )
  
  if ( all_maxed ) {
    len <- length(string_list)
    if ( (is.list(string_list) & (len > 1)) & (string_list[[1]] == "")) {
      string_list <- string_list[2:len]
    }
    return(string_list) # successful case of the recursion
  }
  else {
    cur_level_maxed <- level_is_maxed(closures, level)

    if (cur_level_maxed) {
      if ( str_count(cur_string) > 1 ) {
        string_list <- c( string_list, cur_string )
        cur_string <- str_sub(cur_string, 1, -2)
      } else {
        cur_string <- ""
      }
      if (level > 1) {
        recurse_on_position_at_level( closures, string_list, level - 1, cur_string )
      }
    } else {
      cur_level_is_maxed <- level_is_maxed( closures, level )
      
      if (cur_level_is_maxed) {
        if ( str_count(cur_string) > 1 ) {
          cur_string <- str_sub(cur_string, 1, -2)
        } else {
          cur_string <- ""
        }
        
        increase_pos_in_level(closures, level - 1 )
        # unwind levels
        recurse_on_position_at_level( closures, string_list, level - 1, cur_string )    
      } else {
        char_level <- get_next_char( closures, level )
        str_level <- environment( closures[[level]] )$level_string
        pos_level <- environment( closures[[level]] )$position
        
        if (pos_level >= str_length(str_level)) {
          environment(closures[[level]])$maxed <- TRUE
        }
        cur_string <- paste0( c(cur_string,char_level), collapse = "")
        if (level == length(closures)) {
          increase_pos_in_level(closures, level)
          level_minus_one <- decrease_level(closures, level)
          cur_string_rolled_back <- str_sub(cur_string,1, -2)
          recurse_on_position_at_level( closures, string_list, level_minus_one, cur_string_rolled_back )
        } else {
          # continue visiting upper levels while building 'cur_string'
          recurse_on_position_at_level( closures, string_list, level + 1, cur_string )
        }
      }
    }
  }
}


to_vector_of_strings <- function(aStrigOfDigits) {
  vector_of_digits <- to_vector_of_digits_from_string(aStrigOfDigits)
  vec_of_level_strings <- to_vector_of_target_characters( vector_of_digits )
  level_closures <- lapply(X = vec_of_level_strings, FUN = function(level_string ) closure_creator( level_string ))
  recurse_on_position_at_level(level_closures, list(""), 1, "")
}




# tests
#vos1 <- to_vector_of_strings("92")
#vos2 <- to_vector_of_strings("12")
#vos3 <- to_vector_of_strings("234")
#print(vos1)
#print(vos2)
#print(vos3)

s_to_parse <- "43957"
print(paste0("Entering ", s_to_parse))
vos4 <- to_vector_of_strings(s_to_parse)
print(vos4)

