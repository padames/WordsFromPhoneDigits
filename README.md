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
Me neither, until one realizes that this naive implementation hits a limit pretty quickly by consuming the memory allowed by R to store the function stack every time you call it. The maximum number of digits that can handle is 5 digits and phone numbers in North America have 10 digits.
 
