
#' a pair of functions sprintf_format and format_values used for rcell
#'
#' @param n Unique number of events for the current category
#' @param N Total number of events under the split column
#' @param decimal Number of decimal to display
#' @param type to decide if the denominator n/N(%) in the result
#' @keywords sprintf_format
#'
#' @return
sprintf_format_helper <- function(n, N, decimal = 1, type = "n(%)") {
    showN <- NULL
    if (type == "n/N(%)"){
        showN <- "/%.0f"
    }

    if (n == 0) {
        fmt <- sprintf_format(paste0("%.0f", showN))
    } else if (n == N) {
        # if pct == 100%, then show (100) instead of (100.0)
        fmt <- sprintf_format(paste0("%.0f", showN, " ", "(%.0f)"))
    } else {
        fmt <- sprintf_format(paste0("%.0f", showN, " (%.", decimal, "f)"))
    }

    return(fmt)
}


#' a pair of functions sprintf_format and format_values used for rcell
#'
#' @param n Unique number of events for the current category
#' @param N Total number of events under the split column
#' @param type to decide if the denominator n/N(%) in the result
#'
#' @keywords format_values
#' @example
#' rcell(x = format_values_helper(1, 10),
#'       format = sprintf_format_helper(1, 10))
#'
#' rcell(x = format_values_helper(1, 10, type = "n/N(%)"),
#'       format = sprintf_format_helper(1, 10, type = "n/N(%)"))
#'
#' rcell(x = format_values_helper(10, 10),
#'       format = sprintf_format_helper(10, 10))
#'
#' @return
format_values_helper <- function(n, N, type = "n(%)") {
    showN <- NULL
    if (type == "n/N(%)"){
        showN <- N
    }
    if (n == 0) {
        value <- c(0, showN)
    } else {
        value <- c(n, showN, n / N * 100)
    }

    return(value)
}


df_order_header <- function(df, header_var, sort_df = FALSE, add_all = FALSE) {
    if (is.null(df)) {
        stop("missing df")
    }

    if (is.null(header_var)) {
        stop("missing header_var")
    }

    # settings
    var_name <- header_var[1]
    var_name_n <- header_var[length(header_var)]
    var_label <- attr(df[[var_name]], "label")

    # factorize header variable
    levels <- unique(as.character(df[order(df[[var_name_n]]), ][[var_name]]))
    levels <- levels[levels != "All"]
    levels_ <- c(levels, ifelse(add_all, "All", list(NULL)))
    df[[var_name]] <- factor(df[[var_name]], levels = unique(unlist(levels_)))
    attr(df[[var_name]], "label") <- var_label

    if (sort_df) {
        df <- df[order(df[[var_name]]), ]
    }

    return(df)
}



summarise_vars_categorical_afun <- function(df, .N_col, .var, .N_row,
                                            format = 'n(%)',
                                            decimal = 1,
                                            n_unique_keys = NULL,
                                            N_unique_keys = NULL,
                                            show_N_item = FALSE,
                                            add_Number = FALSE,
                                            missing_rm = TRUE,
                                            missing_label = NULL,
                                            indent_mod = 0L,
                                            ...) {


    if(missing_rm){
        df <- df %>% filter(!(!!as.name(.var) %in% "Missing_value_flag"))
    }

    df_n <- df
    if(!is.null(n_unique_keys)){
        df_n <- df %>% distinct(across(c(n_unique_keys, .var)), .keep_all = TRUE)
    }
    x <- df_n[[.var]]
    if(!is.null(N_unique_keys)){
        cus_N_col <- df %>% distinct(across(N_unique_keys)) %>% nrow()
    }else{
        cus_N_col <- .N_col
    }

    lst_body <- lapply(
        as.list(table(x)),
        function(xi) {
            rcell(format_values_helper(xi, cus_N_col, type = format),
                  format = sprintf_format_helper(xi, cus_N_col, 1, type = format),
                  indent_mod = indent_mod
            )
        }
    )
    if(add_Number){
        lst_body <- append(setNames(list(rcell(cus_N_col,
                                               format = "xx",
                                               indent_mod = indent_mod)), 'Number'), lst_body)
    }

    if(is.null(missing_label)){
        if("Missing_value_flag" %in% names(lst_body)){
            lst_body <- purrr::list_modify(lst_body, "Missing_value_flag" = NULL)
        }
    }else{
        lst_body <- lst_body %>%
            setNames(gsub("Missing_value_flag", missing_label, names(.)))

    }

    in_rows(
        .list = c(
            lst_body,
            # add empty row, in .list obj
            " " = rcell("", format = "xx")
        )
    )
}


#### summarise_vars_continuous_afun
summarise_vars_continuous_afun <- function(df,
                                           .var,
                                           decimal = 1,
                                           ...) {

    stat_options <- c("Number", "Mean (SD)", "Median", "Q1 ; Q3", "Min ; Max")
    x <- df[[.var]]
    if (is.numeric(x)) {
        if (sum(!is.na(x)) == 0) {
            lst_body <- setNames(rep(rcell(" "), length(stat_options)), stat_options)

        } else {

            lst_body <- list("Number" = rcell(sum(!is.na(x)), format = "xx"),
                             "Mean (SD)" = rcell(paste(
                                 round(mean(x, na.rm = TRUE), digits = decimal),
                                 " (",
                                 round(sd(x, na.rm = TRUE), digits = decimal),
                                 ")",
                                 sep = ""
                             )),
                             "Median" = rcell(median(x, na.rm = TRUE),
                                              format = sprintf_format(paste0(
                                                  "%.", decimal, "f"
                                              ))),
                             "Q1 ; Q3" = rcell(paste(
                                 round(
                                     quantile(x, probs = 0.25, type = 2),
                                     digits = decimal
                                 ),
                                 " ; ",
                                 round(
                                     quantile(x, probs = 0.75, type = 2),
                                     digits = decimal
                                 ),
                                 sep = ""
                             )),
                             "Min ; Max" = rcell(paste(
                                 round(
                                     min(x, na.rm = TRUE),
                                     digits = (decimal - 1)
                                 ),
                                 " ; ",
                                 round(
                                     max(x, na.rm = TRUE),
                                     digits = (decimal - 1)
                                 ),
                                 sep = ""
                             )))

        }
        in_rows(
            .list = c(
                lst_body,
                # add empty row, in .list obj
                " " = rcell("", format = "xx")
            )
        )
    }
}

summarize_vars <- function(lyt,
                           vars,
                           var_labels,
                           format = 'n(%)',
                           decimal = 1,
                           n_unique_keys = NULL,
                           N_unique_keys = NULL,
                           show_N_item = FALSE,
                           add_Number = FALSE,
                           missing_rm = TRUE,
                           missing_label = NULL,
                           show_labels='visible',
                           indent_mod = 0L, ...){
    afun_wrap <- function(df, .N_col, .var, .N_row, ...){
        x <- df[[.var]]
        if (is.numeric(x)) {
            args <- list(df = df,
                         .var = .var,
                         decimal = decimal)
            do.call("summarise_vars_continuous_afun", args)
        }else{
            args <- list(df = df,
                         .var = .var,
                         .N_col = .N_col,
                         .N_row = .N_row,
                         format = format,
                         decimal = decimal,
                         n_unique_keys = n_unique_keys,
                         N_unique_keys = N_unique_keys,
                         show_N_item = show_N_item,
                         add_Number = add_Number,
                         missing_rm = missing_rm,
                         missing_label = missing_label,
                         indent_mod = indent_mod
            )
            do.call("summarise_vars_categorical_afun", args)
        }
    }
    rtables::analyze(lyt, vars, var_labels,
                     afun = afun_wrap,
                     extra_args = list(format = format,
                                       decimal = decimal,
                                       n_unique_keys = n_unique_keys,
                                       N_unique_keys = N_unique_keys,
                                       show_N_item = show_N_item,
                                       add_Number = add_Number,
                                       missing_rm = missing_rm,
                                       missing_label = missing_label),
                     show_labels = show_labels,
                     indent_mod = indent_mod)
}


table_to_dataframe <- function(tbl) {

    # check the tabl classs
    #check the

    matrix_tbl <- tbl %>% rtables::matrix_form()

    tbl_strings <- as.data.frame(matrix_tbl$strings)


    ## get the hader index from tbl attributes
    nrow_header <- attributes(matrix_tbl)$nrow_header

    ## get the body of tbl
    tbl_body <- tbl_strings[-c(1:nrow_header), ]

    ## get the header of tbl
    tbl_header <- tbl_strings[c(1:nrow_header), ]

    attr(tbl_body, "tbl_header") <- tbl_header



    return(tbl_body)
}
