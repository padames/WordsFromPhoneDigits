# WordsFromPhoneDigits
Transform a string representing a phone number into all of the possible words according to a digit to character mapping.

The mapping is as follows:

```
digit_to_characters <- c("1" = "", "2" = "abc", "3" = "def", "4" = "ghi",
                         "5" = "jkl", "6" = "mno", "7" = "pqrs",
                         "8" = "tuv", "9" = "wxyz", "0" = "")


```

The naive implementation uses a recursive function called `recurse_digits_to_strings` that covers all the possible re-entrant cases on four arguments. The data structure used to construct the output is a list of strings. The input data structure used to build the output is a list of closures created with a function called `closure_creator` and the mapping from digits to characters:

```
  to_vector_of_strings <- function(aStrigOfDigits) {
    vector_of_digits <- to_vector_of_digits_from_string(aStrigOfDigits)
    vec_of_level_strings <- to_vector_of_target_characters( vector_of_digits )
    closures <- lapply(X = vec_of_level_strings, FUN = function(level_string ) closure_creator( level_string ))
    recurse_digits_to_strings(closures, list(""), 1, "")
  }
```
 :anguished:
 
The steps to go from the input of the main function, called `aStrigOfDigits` to the list of closures `closures` are:

1. Turn the string `aStrigOfDigits` into a vector of characters representing the digits:
  ```
    > vector_of_digits <- to_vector_of_digits_from_string("123")
    > vector_of_digits
    [1] "1" "2" "3"
  ```
2. Turn the vector of charcaters representing the phone digits into their corresponding character strings:
  ```
    > vec_of_level_strings <- to_vector_of_target_characters( vector_of_digits )
    > vec_of_level_strings
      1     2     3 
    "" "abc" "def"
  ```
3. Create the closure list for this specific conversion:
  
  ```
    closures <- lapply(X = vec_of_level_strings, FUN = function(level_string ) closure_creator( level_string ))  
  ```
4. Now all is left to do is call the recursive function with the seed values:
  
  ```
      recurse_digits_to_strings(closures, list(""), 1, "")
  ```

The logic in `recurse_digits_to_strings` is that one travels the list of closures, seen as a stack from bottom to top, maintaining a state represented by the arguments to the function. The objective is to create all combinations of characters with the characters in the `vec_of_level_strings` created in step 2 above.

For example given "123" as input, the algorithm would compute:
  ```
  > to_vector_of_strings("123")
[1] "ad" "ae" "af" "bd" "be" "bf" "cd" "ce" "cf"
```

Not impressed? :unamused:

Me neither, until one realizes that this naive implementation hits a memory limit pretty quickly, input strings of more than 5 digits cannot be processed, clearly a show stopper with phone numbers in North America having 10 digits. The limit comes from R consuming the memory allocated to store the function calls. This is typical of implementations and/or compilers that don't reuse the function stack frame to avoid stack overflow errors in recursive calls. The optimization is based on storing only one function frame and reusing it every time is called, this asumes the function is stateless, re-entrant, and exhastive in its domain (a sound recursive implementation should meet these requirements). With this optimization the compiler only keeps track of the inputs at every state so there are no huge thunks of code waiting to be executed upon hitting the bottom clause of the recursion where everything starts to get unwound.
 
Then I found this [RStudio Community thread](https://community.rstudio.com/t/tidiest-way-to-do-recursion-safely-in-r/1408) on how to do recursion properly in R. Incredulous, I tested it by computing the factorial of 70 using a naive recursive implementation and their trampolin function pattern to make it tail-end recursive, this test would confirm that the R interpreter optimized the recursive pattern avoiding stack overflow errors:

```
1] "Factorial of 70 = 1.19785716699699e+100"
```

The result appeared in a blur so I implemnted my function with the trampolin pattern and voilá, a 10-digit number like `"4039282922"` was transformed into a vector of 34,992 words occupying 2.24 MB in memory in an average time slightly under 12 seconds for my laptop and R version:

  ```
  > vos7 <- to_vector_of_strings_3("4039282922")
  > object.size(vos7)
  2239528 bytes
  > length(vos7)
  [1] 34992
  > library(microbenchmark)
  > microbenchmark(to_vector_of_strings_3("4039282922"),times = 10L)
  Unit: seconds
                                 expr      min       lq     mean   median       uq      max neval
  to_vector_of_strings_3("4039282922") 11.59465 11.69502 12.11465 11.91282 12.31519 13.70905    10
  > c(version$platform, version$version.string)
  [1] "x86_64-pc-linux-gnu"          "R version 3.4.4 (2018-03-15)"
  ```



