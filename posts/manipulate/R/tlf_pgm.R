
demo_t_generator <- function(dataset, columns, columns_label, string_variable, header_variable, header_order, population_variable){


    names(header_variable) <- NULL

    adsl <- dataset %>%
        select(USUBJID, all_of(population_variable), all_of(header_variable), all_of(header_order), all_of(columns)) %>%
        filter(!!as.name(population_variable) == "Y")
    # %>%
    #   mutate(across(string_variable, ~ifelse(.=="", "Missing_value_impute", as.character(.))))

    adsl_fact <- adsl %>%
        mutate(across(all_of(string_variable),
                      ~factor(attributes(cur_data()[[cur_column()]])$format$Decode[.x],
                              levels = attributes(cur_data()[[cur_column()]])$format$Decode)))

    if(isTRUE(is.na(header_order))){
        adsl_fact <- adsl_fact %>%
            mutate(across(all_of(header_variable),
                          ~factor(attributes(cur_data()[[cur_column()]])$format$Decode[.x],
                                  levels = attributes(cur_data()[[cur_column()]])$format$Decode)))
    }else{
        adsl_fact <- df_order_header(adsl_fact, c(header_variable, header_order))
    }



    col_counts <- table(adsl[[header_variable]]) %>% as.vector()
    col_counts <- c(col_counts, sum(col_counts))

    calculated_t <- basic_table() %>%
        split_cols_by(header_variable) %>%
        add_overall_col("All") %>%
        summarize_vars(
            vars = columns,
            var_labels = columns_label,
            dispfmt = c("npct"),
            decimal = 1,
            denom = c("col"),
            unique_row = TRUE
        ) %>%
        rtables::build_table(adsl_fact, col_counts = col_counts)

    return(calculated_t)
}


