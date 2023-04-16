---
title: 'Derive new columns based on given formula'
date: '2023-04-16'
author: Qing Zou
categories: ['R', 'dplyr', 'glue', 'Derive']
description: 'Efficiently Derive new columns based on formula'
execute:
  message: false
  warning: false
editor_options:
  chunk_output_type: console
---


```{r rcell0}
library(dplyr)
library(glue)
```

## Real Case

It is the most common in the data manipulation that deriving a column based on a given formula, such as `Toxicity Grade variables`, and `PCSA variables`.

```{r rcell15}
#| echo: true
#| eval: true
df <- data.frame(SUBJID = paste0("S", 1:10),
                 GROUP = factor(sample(c("A", "B", "C"), 10, replace = T), levels = c("A", "B", "C")),
                 AVAL = sample.int(20, 10)
)
```

If we need to create new variables based on AVAL variable and condition or criteria, we have to do the following derivation.

```{r}
#| echo: true
#| eval: true
df <- df %>% mutate(level_1 = ifelse(AVAL>3 & GROUP == "A", "level 1", NA_character_),
                    level_2 = ifelse(AVAL<3, "level 2", NA_character_))
```
```{r rcell11612 }
#| echo: false
#| eval: true
#| message: FALSE
knitr::kable(df, "html")%>%
  kableExtra::kable_styling(font_size = 16)
```

we used the `ifelse` or `case_when` to implemente.In real case, the massive new variables and conditions lead to a not easy life!
here, we can develop functions for functional programmer purpose to handle this kind of task.

## Method 1

we store the column names, formula, and value in a list `derived_rule`.
```{r rcell116}
#| echo: true
#| eval: true

derived_rule <- list('level_1_new' = list('formula' = 'AVAL>3 & GROUP == "A"', value = "level 1"),
                'level_2_new' = list('formula' = 'AVAL<3', value = "level 2"))
```

we can combine the `glue` to build a function based on the given formula, and then make use of the `do.call` acheive our goal.

##### 1. convert formula to a function

```{r}
#| echo: true
#| eval: true
convert_to_function <- function(formula, value = NULL) {
  args <- paste(all.vars(rlang::parse_expr(formula)), collapse = ", ")
  args <- paste0(args, ", value = NULL")
  glue::glue(
    "function({args}) {{
        if(is.null(value)){{
            {formula}
        }}else{{
          ifelse({formula}, value, NA)
        }}
      }}"
  )
}
```
the involved columns in the formula will be used as the arguments in the new function.

##### 2.Build list with functions and arguments and label value

```{r}
#| echo: true
#| eval: true
derived_column_functions <- function(formula, value = NULL) {

  fun <- convert_to_function(formula)

  fun <- eval(rlang::parse_expr(fun))
  function_args <- rlang::fn_fmls_names(fun)
  function_args <- function_args[function_args!="value"]
  return(list(fun = fun,
              args = function_args,
              value = value))
}
```
For funtional programming, we use list to store the information of a new column, including the formula function, the needed arguments(columns), and the derived value.

##### 3.derive columns from formula

```{r}
#| echo: true
#| eval: true
derive_columns <- function(original_df, derive_infos) {
  df_names <- names(original_df)
  derived_names <- names(derive_infos)

  all_args <- unlist(purrr::map(derive_infos, ~.x$args))

  if (!all(all_args %in% c(df_names, derived_names))) {
    stop("derived columns require columns should exist in original data.")
  }

  columns <- recursive_column_calculation(original_df, derive_infos)

  return(columns)
}

# derive new columns
recursive_column_calculation <- function(original_df, derive_infos) {

    cols_name <- names(derive_infos)
    for (column_name in cols_name) {
        fun <- derive_infos[[column_name]]
        if (all(fun$args %in% names(original_df))) {
            original_df[[column_name]] <- do.call(fun$fun,
                                                    append(lapply(fun$args, \(x) original_df[[x]]), list(value = fun$value))) %>%
                as.vector()
            derive_infos[[column_name]] <- NULL
            if (length(derive_infos) == 0) {
                break()
            } else {
                original_df <- recursive_column_calculation(original_df, derive_infos)
            }
        }
    }
    return(original_df)
}

```
everthing is ready for deriving the new columns. we will find the argument `derive_infos` of `derive_columns` is a list that from the result of `derived_column_functions`.
that means however many columns there need to be derived, we can use the `purrr::map` to do a loop efficiently.


Based on the `deriven_rul` and original data, we can easily add new columns as below:

```{r}
#| echo: true
#| eval: true
derived_functions <- purrr::map(
  derived_rule,
  ~derived_column_functions(.x$formula, value = .x$value)
)
df1 <- derive_columns(df, derived_functions)
```
```{r}
#| echo: false
#| eval: true
#| message: FALSE
knitr::kable(df1, "html")%>%
  kableExtra::kable_styling(font_size = 16)
```

## Method 2

another way of handling the formula is evaluate the fomula string directly instead of converting it to function firstly.


```{r}
#| echo: false
#| eval: true
#| message: FALSE
cond <- c(
    quote(AVAL>3 & GROUP == "A")
)
df2 <- df %>% mutate(level_1 = ifelse(eval(do.call("substitute", cond)), 'level 1', NA_character_))
```
```{r}
#| echo: false
#| eval: true
#| message: FALSE
knitr::kable(df2, "html")%>%
  kableExtra::kable_styling(font_size = 16)
```

but I think it is neither robust enough on the perspective of data processing, not the functional programming in R.
it is also easily lead to the risk of data leak in the process,  some records may can't be identified by this way.

