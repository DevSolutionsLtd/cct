# clean-funs.R
## Helper functions for data cleaning and transformation

## Makes ordered factors as given 
set_ord_fac <- function(vec, lvls) {
  factor(vec, lvls, ordered = TRUE)
}




## Sets Y/N such that Y comes first
set_yn <- function(vec = character())
  factor(vec, c("Yes", "No"))





## Fixes spelling of 'easy' (changes case)
fix_ease <- function(var)
  str_replace(var, "(.\\s)(Easy$)", "\\1easy")





## Properly sets variables reflecting 'ease'
set_ease <- function(vec) {
  eazy <- c("Easy", "Not so easy", "Difficult", "Very difficult")
  vec %>%
    fix_ease() %>%
    set_ord_fac(eazy)
}