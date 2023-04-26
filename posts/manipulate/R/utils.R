#' Wrap the check for file exists
#'
#' @param x the object to be checked
#' @param name the name of x
#
check_dictionary_path <- function(x, name) {
    check_result_char <-
        checkmate::check_character(x,
                                   min.chars = 1L,
                                   any.missing = FALSE,
                                   len = 1L,
                                   null.ok = FALSE
        )

    check_result_path <- checkmate::check_file_exists(x)

    if (is.character(check_result_char)) {
        stop(
            stringr::str_glue("Argument '{name}' is invalid. {check_result_char}"),
            call. = FALSE
        )
    } else if (is.character(check_result_path)) {
        stop(
            stringr::str_glue("Argument '{name}' is invalid. {check_result_path}"),
            call. = FALSE
        )
    } else {
        invisible(x)
    }
}


#' add "format" attributes to the variables of passed datasets
#'
#' @param df
#' @param dictionary
#' @param .vars the variables used to be assisgned the attributes
set_attr <- function(df, dictionary, .vars){

    sub_dict <- purrr::keep_at(dictionary, colnames(df))

    for (var in names(sub_dict)) {
        var_N <- ifelse(paste0(var, 'N') %in% colnames(df), paste0(var, 'N'), 'NA')
        attr(df[[var]], "format") <- list("Value" =  names(sub_dict[[var]]),
                                          "Decode" = purrr::map_vec(sub_dict[[var]], 1),
                                          "Numeric variable" = var_N,
                                          "Ordinal" = names(sub_dict[[var_N]]))
    }

    return(df)
}

#' add a Model name attribute to data.frame
add_model_name <- function(df, name){
    attr(df, 'Model') <-toupper(name)
    return(df)
}


#######TLF----------------

parse_mtdata <- function(metadata){

    # check the structure of metadata

    #------------------------------#
    #------------------------------#


    #check the results

    return(list(variables = names(metadata$columns),
                string_variable = names(purrr::keep(metadata$columns, ~.x$type == "string")),
                domains = purrr::map_vec(metadata$columns, ~purrr::pluck(.x, 'domain'))  |>  unique(),
                header_variable = purrr::pluck(metadata, "header")$variable,
                header_order = purrr::pluck(metadata, "header")$order,
                population_variable = purrr::pluck(metadata, "population")$variable)
    )
}
