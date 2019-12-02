# LOG PRINT FUNCTION ----
# Log messages   
logmsg <- function(argument,
                   output=eval(outfile), 
                   cursor = "=> ", 
                   gap=1, 
                   print = T){
  if (trim(cursor) == "") {
    cat(argument, file = output, append = TRUE, sep = "\n")
  } else {
    argument <- gsub("[\r\n]", "", argument)
    argument <- trim(gsub("\\s+", " ", str_trim(argument)))
    word_df <- as.data.frame(unlist(strsplit(argument, " ")), nm = "Words")
    num_word <- dim(word_df)[1]
    wrap_len <- 110
    
    line = 1
    len = 3
    
    cat(cursor, file = output, append = TRUE, sep = "")
    
    for (word in (1:num_word)) {
      val = paste0(trim(toString(word_df[word, 1])), " ")
      if (len + nchar(val) - 1 > line*wrap_len) {
        cat(paste0("\n", "   "), file = output, append = TRUE, sep = "")
        len = line*wrap_len + 3
        line = line + 1
      } 
      
      if (word == num_word) {
        cat(val, file = output, append = TRUE, sep = "\n")  
      } else {
        cat(val, file = output, append = TRUE, sep = "")
      }
      len = len + nchar(val)
    }
  }
  
  if (gap == 1) {
    cat("\n", file = output, append = TRUE, sep = "")
  }
  if (print == T){
    print(paste(cursor, argument))
  }
}

logbreak <- function(..., output = outfile, suffix = "") {
  
  analysis_number <- list(...) %>%
    map(function(y) {
      if (is.numeric(y)) {
        y <- as.character(y)
        
        if (nchar(y) < 3) {
          y <- paste0(c(suffix, rep(0, 3 - nchar(y)), y), collapse = "")
        }
        
        y
      } else {
        y
      }
    }) %>%
    unlist() %>%
    paste0(collapse = " | ")
  
  logmsg(glue::glue("___________________________________________________Analysis {analysis_number}______________________________________________"),
         cursor = "", 
         gap = 1,
         output = output)
}

# GROUP BY ----
# groups and counts by all variables in a dataframe
grouper <- function(df) {
  df %>% 
    group_by_all() %>% 
    count() %>% 
    as.data.frame()
}

# REPLACES ALL VALUES BY THEIR ARCHETYPES
simplifier <- function(df, threshold = 6) {
  df_new <- df
  for (var in colnames(df)) {
    if (length(unique(df_new[, var])) > threshold) {
      df_new[, var] <- ifelse(is.na(df[, var]), "NA", 
                              ifelse(trim(df[, var]) == "", "blank",
                                     ifelse(trim(df[, var]) == 0, "zero",
                                            ifelse(trim(df[, var]) == 1, "one",
                                                   ifelse(trim(df[, var]) < 1 & trim(df[, var]) > 0, "zero-one",
                                                          ifelse(trim(df[, var]) < 0, "negative",
                                                                 "VALUE"))))))
    }
  }
  return(df_new)
}

column_tracker <- function(df){
  print(paste0(ncol(select(df, matches("^x._"))), " (raw) + ",
               ncol(select(df, matches("^z._"))), " (calculated) = ", 
               ncol(df), " (total)"))
}

source_time <- function(file_loc, file_name, print.eval, echo, time_capsule){
  start_time <- Sys.time()
  source(file.path(file_loc, file_name), print.eval = print.eval, echo = echo)
  temp <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  time_capsule[[file_name]] <<- paste0("Time to run: ", round(temp, 0), " mins")
  print(paste0("Time to run ", file_name, ": ", round(temp, 0), " mins"))
}

combine_columns <- function(data, new_column_name, pattern, type = "starts_with") {
  # Capturing the selected column name and selecting names
  quo_column_name <- enquo(new_column_name)
  selecting_function <- switch(
    type,
    "starts_with" = starts_with,
    "ends_with" = ends_with,
    "matches" = matches,
    "contains" = contains,
    "one_of" = one_of,
    starts_with
  )
  
  data %>%
    mutate_at(vars(selecting_function(pattern)), trimws) %>%
    mutate_at(vars(selecting_function(pattern)), ~ ifelse(is.na(.), "", .)) %>%
    unite(!!quo_column_name, 
          selecting_function(pattern), 
          sep = "|", 
          remove = F) %>%
    mutate(!!quo_column_name := gsub("\\|+", "|", !!quo_column_name),
           !!quo_column_name := trimws(!!quo_column_name),
           !!quo_column_name := gsub("\\|$", "", !!quo_column_name),
           !!quo_column_name := str_remove(!!quo_column_name, "^\\| ")
    )
}

# CLEAN AND MODIFY TEXT ----
clean_text <- function(vector) {
  map_chr(vector, function(x) {
    x %>%
      tolower() %>%
      str_replace_all("[:punct:]", " ") %>%
      str_replace_all("\\s+", " ") %>%
      trimws()
  })
}

simple_cap <- function(vector_of_data) {
  map_chr(vector_of_data, function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep = "", collapse = " ")
  })
}

# QC RESPONSES OF VARIOUS QUESTIONS ----
comparator <- function(df, var1, values1, description1, var2, values2, description2){
  df0 <- df %>% 
    select(var1, var2) %>% 
    mutate(id = row_number())
  
  for (num in (1:2)) {
    
    if (get(paste0("values", num)) == "non_blank") {
      df1 <- filter(df0, trim(df0[, get(paste0("var", num))]) != "", !is.na(df0[, get(paste0("var", num))]))
    } else if (get(paste0("values", num)) == "non_zero") {
      df1 <- filter(df0, trim(df0[, get(paste0("var", num))]) != 0, !is.na(df0[, get(paste0("var", num))]))
    } else {
      df1 <- filter(df0, df0[, get(paste0("var", num))] %in% get(paste0("values", num)))
    }
    
    assign(paste0("arr", num), as.array(df1$id))
    
  }
  
  logmsg(ifelse(all(arr2 %in% arr1),
                paste0("QC Pass: Only those ", description1, " answered ", description2),
                paste0("X X X QC Fail: ", length(setdiff(arr2, arr1)), 
                       " people OTHER THAN ", description1, " answered ", description2)), gap = 0)
  
  logmsg(ifelse(all(arr1 %in% arr2),
                paste0("QC Pass: Everyone ", description1, " answered ", description2),
                paste0("X X X QC Fail: ", length(setdiff(arr1, arr2)), 
                       " people ", description1, " did not answer ", description2)))
  
}


# FIND A COLUMN BASED ON STRING ----
finder <- function(data, ...) {
  
  temp <- ensyms(...)
  
  list_of_matches <- map_chr(temp, function(x) quo_name(x))
  
  map(list_of_matches, function(x) {
    data %>%
      select(matches(x)) %>%
      colnames()
  }) %>%
    set_names(list_of_matches)
  
}

# LOG TABLE FUNCTION ----
logtable <- function(df, title="", output=eval(outfile)){
  
  #view(df)
  if (nrow(df) == 0) {
    cat(paste0("   | ", trimws(gsub("_", " ", toupper(title)))), file = output, 
        append = TRUE, sep = "\n")
    cat(paste0("   ", "Table has zero rows. Can not be written"), file = output, 
        append = TRUE, sep = "\n")
    cat("", file = output, append = TRUE, sep = "\n")
  } else {
    
    if (nrow(df) == 1) { df[nrow(df) + 1, ] <- "" } 
    
    df <- cbind("   " = "   ", df)
    
    tot <- 0
    cnames <- colnames(df)
    separator <- colnames(df)
    n <- as.matrix(nchar(cnames))
    
    d <- apply(df, 2, format)
    n <- apply(cbind(n, nchar(d[1,])), 1, max)
    
    fmts <- paste0("%",n, "s")
    for (i in 1:length(cnames)) {
      cnames[i] <- sprintf(fmts[i], cnames[i])
      separator[i] <- paste(replicate(n[i], "-"), collapse = "")
      d[,i] <- sprintf(fmts[i], trimws(d[,i]))
      tot <- tot + n[i] + 1
    }
    d <- rbind(cnames, separator, d)
    
    cat(paste0("   ", paste(replicate(ceiling(tot) - 3, "#"), collapse = "")), file = output, append = TRUE, sep = "\n")
    cat(paste0("   | ", trimws(gsub("_", " ", toupper(title)))), file = output, append = TRUE, sep = "\n")
    cat(paste0("   ", paste(replicate(ceiling(tot) - 3, "="), collapse = "")), file = output, append = TRUE, sep = "\n")
    write.table(d, output, quote = F, append = T, row.names = F, col.names = F, na = "<NA>", sep = "|")
    cat(paste0("   ", paste(replicate(ceiling(tot) - 3, "="), collapse = "")), file = output, append = TRUE, sep = "\n")
    cat(paste0("   ", paste(replicate(ceiling(tot) - 3, "#"), collapse = "")), file = output, append = TRUE, sep = "\n")
    cat("", file = output, append = TRUE, sep = "\n")
    cat("", file = output, append = TRUE, sep = "\n")
  }
}

# TREE CLASSIFICATION ---- 
sub_segmentor <- function(df, 
                          y, 
                          topic, 
                          with_weight, 
                          ignore_weight_responses = c(), 
                          weight_col){
  
  # ** setting up
  dir = paste0(v_loc["outputs"], "/02. Tree Charts/", topic, "/")
  if (!dir.exists(dir)) dir.create(dir)
  
  outfile <- paste0(dir, "summary_", ifelse(with_weight == T, "with_weight", "without_weight"), ".txt")
  unlink(outfile)
  
  # remove NA
  removed_df <- df %>%  
    filter_all(any_vars(is.na(.)))
  logmsg(paste0(nrow(removed_df), " rows removed during classification due to NAs"))
  
  df <- df %>% 
    drop_na()
  
  # Capturing name of y variable
  var_name <- y
  
  min_bucket_size <- 100
  
  logbreak(topic, output = outfile)
  logmsg(paste0("segments for ", quo_name(var_name), ifelse(with_weight == T, "_with_weight", "_without_weight")), output = eval(outfile))
  
  train_control <- trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 3
  )
  
  # ** create data for tree  
  
  df <- df %>%
    filter(!is.na(!!var_name), trimws(!!var_name) != "") %>% 
    mutate_if(is_character, as_factor) 
  
  if (with_weight == T) {
    temp <- tabyl(df, !!var_name) %>%
      filter(!(!!var_name %in% ignore_weight_responses))
    
    weighted_class <- temp %>%
      filter(n != max(n)) %>%
      pull(!!var_name)
    
    if (length(weighted_class) > 0) {
      df <- map_dfr(weighted_class, function(x) {
        temp_n <- temp %>%
          filter(!!var_name %in% x) %>%
          pull(n)
        
        df %>%
          filter(!!var_name %in% x) %>%
          sample_n(size = max(temp$n) - temp_n, replace = TRUE)
      }) %>%
        rbind(df)
    }
  }
  
  print(grouper(select(df, !!var_name)))
  
  # ** fit a tree
  colnames_for_data <- df %>%
    select(-weight_col, - !!var_name) %>%
    colnames() %>%
    paste0(collapse = "+")
  
  tree_formulae <- as.formula(paste(quo_name(var_name), "~", colnames_for_data)) 
  rm(colnames_for_data)
    
  tree_fit <- train(tree_formulae,
                    data = df,
                    weights = get(weight_col),
                    method = "rpart",
                    parms = list(split = "information"),
                    trControl = train_control,
                    control = rpart.control(minbucket = min_bucket_size),
                    tuneLength = 10
  )
  
  # ** output the chart / message of failure
  if (nrow(tree_fit$finalModel$frame) == 1) {
    logmsg("The model predicts only a single class and cannot be graphed")
  } else {
    png(paste0(dir, quo_name(var_name), ifelse(with_weight == T, "_with_weight", "_without_weight"), ".png"),
        height = 3000,
        width = 6000,
        res = 200)
    
    rpart.plot::rpart.plot(tree_fit$finalModel,
                           type = 2,
                           tweak = 1.1,
                           space = 0.1,
                           gap = 0.01)
    dev.off()
    graphics.off()
  }
}

segmentor <- function(df, y_string, topic, ignore_weight_responses = c(), weight_col){
  y_string <- ensym(y_string)
  sub_segmentor(df, y_string, topic, F, weight_col = weight_col)
  sub_segmentor(df, y_string, topic, T, ignore_weight_responses, weight_col = weight_col)
}

# CONVERT DECIMAL TO PERCENTAGE AND REVERSE ----
dec_to_perc <- function(vector_of_data, digits = 2, force = TRUE) {
  temp_vector <- round(vector_of_data*100, digits) %>%
    paste0("%")
  
  
  temp_vector <- map_chr(temp_vector, function(x) {
    no_after_decimal <- str_split(x, "\\.") %>%
      unlist() %>%
      magrittr::extract(2) %>%
      nchar()
    
    if (is.na(no_after_decimal) & digits >= 1) {
      # Is a whole number
      no_after_decimal <- 2
      
      x <- str_replace(x, "%", ".0%")
    }
    
    if (force & digits > 0) {
      if (no_after_decimal < digits + 1) {
        x <- x %>%
          str_remove("%") %>%
          paste0(paste0(c(rep("0", digits - no_after_decimal + 1), "%"), collapse = ""))
      }
    }
    
    x
  })
  
  temp_vector %>%
    str_remove_all("NA.00%")
    
}

perc_to_dec <- function(vector_of_percentages) {
  vector_of_percentages %>%
    str_remove_all("\\*") %>%
    str_remove_all("\\$") %>%
    str_remove_all("<") %>%
    str_remove_all(">") %>%
    str_remove_all("\\[") %>%
    str_remove_all("\\]") %>%
    str_remove_all("%") %>%
    str_remove_all(" ") %>%
    as.numeric() %>%
    magrittr::divide_by(100)
}

sum_over_percentages <- function(vector_of_percentages) {
  vector_of_percentages %>%
    perc_to_dec() %>%
    sum(na.rm = TRUE) %>%
    dec_to_perc()
}

# SPLIT AND MATCH ----
split_and_match <- function(vector_of_splits, pattern_of_matches, sep = "\\|") {
  temp <- vector_of_splits %>%
    str_split(pattern = sep) %>%
    map(clean_text) %>%
    map(paste0, collapse = "|")
  
  matches <- clean_text(pattern_of_matches)
    
    map_lgl(temp, function(x) {
      str_detect(x, matches) %>%
        any()
    })
}

# NORMALIZE NUMERIC COLUMN ----
normaliser <- function(numeric_vector) {
  
  if (sd(numeric_vector, na.rm = TRUE) != 0) {
    (numeric_vector - mean(numeric_vector, na.rm = TRUE)) / sd(numeric_vector, na.rm = TRUE)
  } else {
    rep(0, length(numeric_vector))
  }
  
}

# SUMMARISE AND PIVOT UP ----
# Creating a hidden environment to hide sub_summariser
hidden_env <- new.env()

# Wrapper function around sub summariser
summariser_base <- function(data,                                                 # Must have. Dataset to summarise
                            var,                                                  # Must have. Variable to summarise
                            group_var = NULL,                                     # Grouping variable (if any)
                            type = c("default", "flipped"),                       # Should the grouping responses be in rows, or columns 
                            vartype = c("ci", "se"),                              # Provide SE or CI 
                            stat = c("mean", "median", "total"),                  # Provide proportions (default), median or total
                            significance_level = v_global[["significance"]],      # level of significance to test for
                            spread_threshold = v_global[["spread"]],              # Spread threshold to test for
                            compare_against = "overall",                          # Reference class to test significance
                            survey_design = list( 
                              # List of named arguments for the parameter. Will be passed as is to as_survey function from srvyr
                              id = NULL,
                              strata = NULL,
                              weight = NULL
                            )) {
  # Initialising summariser and conducting basic data sanity checks
  summariser_local <- list()
  # browser()
  
  if (missing(data))   stop("Please provide a valid data frame to summarise")
  
  if (nrow(data) <= 1) stop("The data frame provided has <= 1 row. It cannot be summarised")
  
  if (missing(var))    stop("No variable to summarise has been provided")
  
  # Catching variable name and saving as a symbol
  quo_var_name <- ensym(var)
  
  # If provided, check if the variable exists in the data frame
  if (!as.character(quo_var_name)  %in% colnames(data)) stop("The variable provided does not exist in the dataframe")
  
  # Catching group variable if provided
  if (!missing(group_var)) {
    group_var <- ensym(group_var)
  }
  
  # If provided, check if the variable exists in the data frame
  if (!missing(group_var)) {
    if (!as.character(group_var)  %in% colnames(data)) stop("The group provided does not exist in the dataframe")    
  }
  
  # If confidence level is not between 0 and 1, convert it to 0.95 as a default and drop a message
  if (!between(significance_level, 0, 1)) {
    message("The significance level provided is not between 0 and 1. Will revert to 95% interval as default")
    
    significance_level <- 0.95
  }
  
  # Match arguments for type, vartype, and stat
  summariser_local[["type"]] <- match.arg(type)
  summariser_local[["vartype"]] <- match.arg(vartype)
  summariser_local[["stat"]] <- match.arg(stat)
  
  summariser_local[["compare_against"]] <- trimws(tolower(compare_against))
  
  # TODO: If id, weights or strata are provided, check if they exist in the data frame
  
  # Call sub_summariser_base with the variables provided
  ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  summariser_local[["call_result"]] <- .sub_summariser_base(data = data, 
                                                            var = quo_var_name, 
                                                            group_var = group_var,
                                                            type = summariser_local[["type"]],
                                                            vartype = c("se", "ci"),
                                                            stat = summariser_local[["stat"]],
                                                            significance_level = significance_level,
                                                            survey_design = survey_design
  ) %>%
    clean_names() %>%
    mutate_if(is.numeric, ~ round(., 3))
  
  ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Checking if the variable is numeric (see if there is a column called value)
  summariser_local[["is numeric"]] <- (c("value", "metric") %in% colnames(summariser_local[["call_result"]])) %>%
    any()
  
  # 1. Adding a totals row
  if ((!summariser_local[["is numeric"]]) & summariser_local[["type"]] != "flipped") {
    
    temp_numeric <- summariser_local[["call_result"]] %>%
      summarise_if(is.numeric, ~ sum(., na.rm = TRUE))
    
    temp_character <- summariser_local[["call_result"]] %>%
      select(-contains(quo_name(quo_var_name)), -ends_with("_se"), -ends_with("_upp"), -ends_with("_low")) %>%
      summarise_if(is.character, sum_over_percentages)
    
    summariser_local[["call_result"]] <- bind_cols(temp_numeric, temp_character) %>%
      mutate(!!quo_var_name := "Total") %>%
      bind_rows(summariser_local[["call_result"]], .)
  } else if ((!summariser_local[["is numeric"]]) & summariser_local[["type"]] == "flipped") {
    
    temp_numeric <- summariser_local[["call_result"]] %>%
      select(quo_name(group_var), starts_with("n_")) %>%
      mutate(total = rowSums(
        select(., -!!group_var), na.rm = TRUE
      )) %>%
      select(!!group_var, total)
    
    summariser_local[["call_result"]] <- left_join(temp_numeric,
                                                   summariser_local[["call_result"]],
                                                   by = quo_name(group_var))
  }
  
  ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Creating stars for significance of a variable; depends on numeric vs non-numeric, flipped or not and if grouped or not
  
  # browser()
  
  # First check; if it is not grouped and shows compare against as overall
  if (missing(group_var) & summariser_local[["compare_against"]] == "overall") {
    # Cannot be done. Move ahead
  } else {
    # if variable type is numeric, go ahead
    if (summariser_local[["is numeric"]]) {
      summariser_local[["call_result"]] <- .significance_test_overall(summariser_local[["call_result"]],
                                                                      var = group_var,
                                                                      total_column = "total",
                                                                      mean_column = "value",
                                                                      se_column = "value_se",
                                                                      overall_in = "row",
                                                                      compare_against = summariser_local[["compare_against"]],
                                                                      stat = summariser_local[["stat"]]) %>%
        mutate(`*` = ifelse(tolower(trimws(!!group_var)) == compare_against, "***", "")) %>%
        select(!!group_var, `*`, everything())
    } else {
      if (missing(group_var)) {
        summariser_local[["call_result"]] <- .significance_test_overall(summariser_local[["call_result"]],
                                                                        var = quo_var_name,
                                                                        total_column = "total",
                                                                        mean_column = "percent_overall",
                                                                        se_column = "percent_overall_se",
                                                                        overall_in = "row",
                                                                        compare_against = summariser_local[["compare_against"]],
                                                                        stat = summariser_local[["stat"]]) %>%
          mutate(`*` = ifelse(tolower(trimws(!!quo_var_name)) == compare_against, "***", "")) %>%
          select(!!quo_var_name, `*`, everything())
        
      } else {
        list_of_responses <- summariser_local[["call_result"]] %>%
          select(ends_with("_se"), -contains("overall")) %>%
          colnames() %>%
          str_remove_all("percent_") %>%
          str_remove_all("estimated_") %>%
          str_remove_all("_se$") %>%
          str_remove_all("^se$")
        
        for (x in list_of_responses) {
          summariser_local[["call_result"]] <- .significance_test_overall(summariser_local[["call_result"]],
                                                                          var = ifelse(summariser_local[["type"]] == "flipped", group_var, quo_var_name),
                                                                          total_column = paste0("n", ifelse(x == "", "", "_"),  x),
                                                                          mean_column = paste0(ifelse(summariser_local[['stat']] == "total", "estimated", "percent"), ifelse(x == "", "", "_"), x),
                                                                          se_column = paste0(ifelse(summariser_local[['stat']] == "total", "estimated_", "percent_"), x, ifelse(x == "", "se", "_se")),
                                                                          overall_in = ifelse(summariser_local[["type"]] == "flipped", "row", "column"),
                                                                          compare_against = ifelse(summariser_local[["type"]] == "flipped", 
                                                                                                   summariser_local[["compare_against"]], 
                                                                                                   str_replace(summariser_local[["compare_against"]], " ", "_")),
                                                                          stat = summariser_local[["stat"]])
        }
        
        if (summariser_local[["type"]] == "flipped") {
          summariser_local[["call_result"]] <- summariser_local[["call_result"]] %>%
            mutate(`*` = ifelse(tolower(trimws(!!group_var)) == compare_against, "***", "")) %>%
            select(!!group_var, `*`, everything())
        } else {
          
          binding_row <- ifelse(str_detect(colnames(summariser_local[["call_result"]]), 
                                           str_replace(summariser_local[["compare_against"]], " ", "_")), 
                                "***", 
                                "")
          names(binding_row) <- colnames(summariser_local[["call_result"]])
          
          summariser_local[["call_result"]] <- summariser_local[["call_result"]] %>%
            rbind(binding_row)
        }
      }
    }
  }
  
  # Removing SE or CI based on the called variable
  if (summariser_local[["vartype"]] == "ci") {
    summariser_local[["call_result"]] <- summariser_local[["call_result"]] %>%
      select(-ends_with("_se"))
  } else {
    summariser_local[["call_result"]] <- summariser_local[["call_result"]] %>%
      select(-ends_with("_low"), -ends_with("_upp"))    
  }
  
  if (summariser_local[["vartype"]] == "ci" & 
      !missing(group_var) & 
      !summariser_local[["is numeric"]] &
      summariser_local[["type"]] != "flipped") {
    
    # reordering columns
    numrow <- length(unique(data[, quo_name(group_var)]))
    numcol <- ncol(summariser_local[["call_result"]])
    
    col_order <- c(1, 3, seq(5 + numrow + 1, numcol, 3), 
                   2, seq(6, 6 + numrow - 1, 1),
                   sort(c(4, seq(5 + numrow + 2, numcol, 3),
                          5, seq(5 + numrow + 3, numcol, 3))))
    
    summariser_local[["call_result"]] <- summariser_local[["call_result"]][, col_order]
    
    rm(numrow, numcol, col_order)
  }
    
  if (summariser_local[["vartype"]] == "ci" & 
      !missing(group_var) & 
      !summariser_local[["is numeric"]] &
      summariser_local[["type"]] == "flipped") {
    
    # reordering columns
    numcol <- ncol(summariser_local[["call_result"]])
    numrow <- (numcol - 3)/4
    
    col_order <- c(1, 2, seq(3 + numrow + 1, numcol, 3), 
                   3, seq(4, 4 + numrow - 1, 1),
                   sort(c(seq(3 + numrow + 2, numcol, 3),
                          seq(3 + numrow + 3, numcol, 3))))
    
    summariser_local[["call_result"]] <- summariser_local[["call_result"]][, col_order]
    
    rm(numrow, numcol, col_order)
  }
  
  # rename a few columns
  colnames(summariser_local[["call_result"]]) <- 
    gsub("percent_", "perc_", colnames(summariser_local[["call_result"]]))
  
  colnames(summariser_local[["call_result"]]) <- 
    gsub("total", "overall", colnames(summariser_local[["call_result"]]))
  
  # individual spread
  name <- colnames(summariser_local[["call_result"]])
  for (var2 in colnames(summariser_local[["call_result"]][,grep("^perc_.*(?<!_low|_upp)$", 
                                                               name, perl = T)])){
    vect <- summariser_local[["call_result"]][[var2]]
    maxn <-  max(nchar(vect))+3-2
    summariser_local[["call_result"]][, var2] <- case_when(
      is.na(summariser_local[["call_result"]][[paste0(var2, "_upp")]]) | 
        is.na(summariser_local[["call_result"]][[paste0(var2, "_low")]]) ~ "",
      perc_to_dec(summariser_local[["call_result"]][[paste0(var2, "_upp")]]) -
        perc_to_dec(summariser_local[["call_result"]][[paste0(var2, "_low")]]) >
        spread_threshold ~ paste0("[]", strrep(" ", (maxn-nchar(vect)))),
      TRUE ~ "") %>% 
      paste0(vect)
    rm(maxn, vect)
  }
  
  rm(name, var2)
  
  # Returning the final called result
  return(summariser_local[["call_result"]])
}

# Function for ttests
hidden_env$welch_ttest <- function(m1, m2, s1, s2, n1, n2, m0 = 0, equal.variance = FALSE,
                                   digits = 4) {
  
  # correct for providing SE instead of SD
  s1 <- s1 * sqrt(n1)
  s2 <- s2 * sqrt(n2)  
  
  if (equal.variance == FALSE) {
    se <- sqrt( (s1^2/n1) + (s2^2/n2) )
    # welch-satterthwaite df
    df <- ((s1^2/n1 + s2^2/n2)^2)/((s1^2/n1)^2/(n1 - 1) + (s2^2/n2)^2/(n2 - 1))    
  } else {
    # pooled standard deviation, scaled by the sample sizes
    se <- sqrt((1/n1 + 1/n2) * ((n1 - 1) * s1^2 + (n2 - 1) * s2^2)/(n1 + n2 - 2) ) 
    df <- n1 + n2 - 2
  }      
  
  t <- (m1 - m2 - m0)/se 
  
  dat = tibble(
    difference_of_means = round((m1 - m2), digits),
    std_error = round(se, digits),
    t_value = round(t, digits),
    p_value = round(2 * pt(-abs(t), df), digits)
  )
  
  return(dat)
}

# Testing significance of result against the overall picture (only available with groups)
hidden_env$.significance_test_overall <- function(summarised_data,
                                                  var,
                                                  total_column,
                                                  mean_column,
                                                  se_column,
                                                  compare_against = "overall",
                                                  overall_in = c("row", "column"),
                                                  stat) {
  
  # browser()
  
  # Function to test significance of result versus the national average 
  compare_against <- trimws(tolower(compare_against))
  
  # if numeric variable, make one_of a random column that will not exists
  if ("value" %in% colnames(summarised_data)) {
    cols_to_mutate <- c("______PLEASE IGNORE WARNING_______")
  } else {
    cols_to_mutate <- c(mean_column, se_column)
  }
  
  # Mutate any percentages in the data to decimals (if stat is not total)
  if (stat != "total") {
    summarised_data <- summarised_data %>%
      mutate_at(vars(one_of(cols_to_mutate), 
                     starts_with(paste0("percent_", compare_against))), 
                perc_to_dec)    
  }
  
  # browser()
  
  # If row, select the overall and choose 
  if (overall_in == "row") {
    overall_data <- summarised_data %>%
      mutate(temporary_significance_variable = tolower(trimws(!!var))) %>%
      filter(temporary_significance_variable == compare_against) %>%
      select(-temporary_significance_variable)
    
    means_data <- summarised_data %>%
      mutate(temporary_significance_variable = tolower(trimws(!!var))) %>%
      filter(temporary_significance_variable != compare_against) %>%
      select(-temporary_significance_variable)
    
    test_result <- map_dfr(1:nrow(means_data), function(x) {
      selected_row <- means_data[x, ]
      
      hidden_env$welch_ttest(pull(selected_row, mean_column),
                             pull(overall_data, mean_column),
                             pull(selected_row, se_column),
                             pull(overall_data, se_column),
                             pull(selected_row, total_column),
                             pull(overall_data, total_column)) %>%
        mutate(!!var := pull(selected_row, !!var))
    })
  } else {
    overall_data <- summarised_data
    
    colnames(overall_data) <- tolower(colnames(summarised_data))
    
    if (compare_against == "overall") {
      overall_data <- overall_data %>%
        select(!!var, n_overall = total, contains("overall"))
    } else {
      overall_data <- overall_data %>%
        select(!!var, contains(compare_against))      
    }
    
    means_data <- summarised_data %>%
      select(!!var, total_column, mean_column, se_column)
    
    responses <- pull(summarised_data, !!var)
    
    test_result <- map_dfr(responses, function(x) {
      selected_row <- means_data %>%
        filter(!!var == x)
      
      overall_row <- overall_data %>%
        filter(!!var == x)
      
      hidden_env$welch_ttest(select(selected_row, mean_column) %>% pull(1),
                             select(overall_row, contains(compare_against), -ends_with("_se"), -starts_with("n_")) %>% pull(1),
                             select(selected_row, se_column) %>% pull(1),
                             select(overall_row, ends_with("_se")) %>% pull(1),
                             select(selected_row, total_column) %>% pull(1),
                             select(overall_row, starts_with("n_")) %>% pull(1)) %>%
        mutate(!!var := x)
      
    }) 
  }
  
  test_result <- test_result %>%
    mutate(significance = case_when(
      is.na(p_value) |
        is.nan(p_value) ~ "",
      p_value <= 0.01 ~ "***",
      p_value <= 0.025 ~ "**",
      p_value <= 0.05 ~ "*",
      TRUE ~ ""
    )) %>%
    select(!!var, significance) %>%
    right_join(summarised_data, by = quo_name(var)) %>%
    mutate(significance = ifelse(is.na(significance), "", significance))
  
  if (stat != "total") {
    test_result <- test_result %>%
      mutate_at(vars(one_of(cols_to_mutate), 
                     starts_with(paste0("percent_", compare_against))), dec_to_perc)
  }
  
  test_result <- test_result %>%
    unite(!!sym(mean_column), significance, !!sym(mean_column), sep = "")
  
  
  test_result[, tolower(colnames(summarised_data))]
}

# Workhorse summariser function
hidden_env$.sub_summariser_base <- function(data,
                                            var,
                                            group_var = NULL,
                                            type = c("default", "flipped"),
                                            vartype = c("se", "ci"),
                                            stat = c("mean", "median", "total"),
                                            significance_level = 0.95,
                                            survey_design = list( 
                                              # List of named arguments for the parameter. Will be passed as is to as_survey function from srvyr
                                              id = NULL,
                                              strata = NULL,
                                              weight = NULL
                                            )) {
  
  # Initialising the summariser function and conducting basic data sanity checks
  summariser_local <- list()
  # browser()
  
  # Catching variable name and saving as a symbol
  quo_var_name <- var
  
  # If the provided variable is logical, convert to numeric and send a message
  summariser_local[["variable is logical"]] <- data %>%
    pull(!!quo_var_name) %>%
    is_logical() %>%
    all()
  
  if (summariser_local[["variable is logical"]]) {
    temp_message <- paste(
      "The variable provided is logical in nature. It will be converted to numeric in order to continue.",
      "Please force to character/ factor variable if you want to get groups. The survey mean function will be automatically converted to use proportions"
    )
    
    message(temp_message)
    
    data <- data %>%
      mutate(!!quo_var_name := !!quo_var_name*1)
  }
  
  # If provided, check if the variable is numeric
  summariser_local[["variable is numeric"]] <- data %>%
    pull(!!quo_var_name) %>%
    is.numeric() %>%
    all()
  
  # Catching group variable if provided
  if (!is.null(group_var)) quo_group_var <- group_var
  
  # Match arguments for type, vartype, and stat
  summariser_local[["type"]] <- type
  summariser_local[["vartype"]] <- vartype
  summariser_local[["stat"]] <- stat
  
  # Catch confidence level
  summariser_local[["significance_level"]] <- significance_level
  
  # If non numeric variable is provided with median, convert it back to mean with a message
  if (!summariser_local[["variable is numeric"]] & summariser_local[["stat"]] == "median") {
    message("Medians cannot be used with a character/factor variable. Converting to the default of mean")
    summariser_local[["stat"]] <- "mean"
  }
  
  summariser_local[["stat function"]] <- switch(
    summariser_local[["stat"]],
    "mean" = survey_mean,
    "median" = survey_median,
    "total" = survey_total,
    survey_mean
  )
  
  survey_data <- do.call(as_survey, c(list(data), survey_design))
  
  # message("Sanity checks have been completed, and survey data is ready. Starting summarisation")
  # START OF SUMMARISER
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # A. Numeric variable ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (summariser_local[["variable is numeric"]]) {
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # 1. Numeric variable without groups ----
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    temp_result <- survey_data %>%
      summarise(value = summariser_local[["stat function"]](!!quo_var_name, 
                                                            vartype = summariser_local[["vartype"]],
                                                            na.rm = TRUE,
                                                            level = summariser_local[["significance_level"]],
                                                            proportion = summariser_local[["variable is logical"]]),
                total = unweighted(n())) %>%
      mutate(!!quo_var_name := "Overall") %>%
      select(!!quo_var_name, total, value, everything())
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # 2. Numeric variable with groups ----
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (!is.null(group_var)) {
      
      # message("Warning: This may break some code. Just convert numeric vector to character if it does.")
      
      # Renaming varname of temp_result to match the group
      temp_result <- temp_result %>%
        rename(!!quo_group_var := !!quo_var_name)
      
      # If mean then use proportions. Else, remove the option for proportions
      if (summariser_local[["type"]] == "mean") {
        temp_result <- survey_data %>%
          group_by(!!quo_group_var) %>%
          summarise(value = summariser_local[["stat function"]](!!quo_var_name, 
                                                                vartype = summariser_local[["vartype"]],
                                                                na.rm = TRUE,
                                                                level = summariser_local[["significance_level"]],
                                                                proportion = summariser_local[["variable is logical"]]),
                    total = unweighted(n())) %>%
          arrange(-value) %>%
          bind_rows(temp_result, .)
      } else {
        temp_result <- survey_data %>%
          group_by(!!quo_group_var) %>%
          summarise(value = summariser_local[["stat function"]](!!quo_var_name, 
                                                                vartype = summariser_local[["vartype"]],
                                                                level = summariser_local[["significance_level"]],
                                                                na.rm = TRUE),
                    total = unweighted(n())) %>%
          arrange(-value) %>%
          bind_rows(temp_result, .)
      }
      
      # If type is flipped, convert the dataset
      if (summariser_local[["type"]] == "flipped") {
        temp_result <- temp_result %>%
          gather(metric, value, -!!quo_group_var) %>%
          spread(!!quo_group_var, value) %>%
          clean_names()
      }
    }
  } else {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # B. Non-numeric variable ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 3. Non-numeric variable without groups ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # Counting number of responses for the factor variable
    summariser_local[["length of factor variable"]] <- data %>%
      pull(!!quo_var_name) %>%
      unique() %>%
      length()
    
    # If only one level, adding a temporary factor that will be removed
    if (summariser_local[["length of factor variable"]] == 1) {
      survey_data <- survey_data %>%
        mutate(!!quo_var_name := fct_expand(!!quo_var_name, "____TEMP_FACTOR____"))
    }
    
    temp_result <- survey_data %>%
      group_by(!!quo_var_name) %>%
      summarise(total = unweighted(n()),
                percent_overall = summariser_local[["stat function"]](vartype = summariser_local[["vartype"]],
                                                                      level = summariser_local[["significance_level"]],
                                                                      na.rm = TRUE))
    
    # If only one level, removing the temporay factor
    if (summariser_local[["length of factor variable"]] == 1) {
      temp_result <- temp_result %>%
        mutate(!!quo_var_name := fct_drop(!!quo_var_name, "____TEMP_FACTOR____"))
    }
    
    if (!is.null(group_var)) {
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # 4. Non-numeric variable with groups ----
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # browser()
      
      # Counting the number of responses per group
      temp_count <- survey_data %>%
        as_tibble() %>%
        group_by(!!quo_group_var, !!quo_var_name) %>%
        count(name = "n")
      
      summariser_local[["list of responses"]] <- data %>%
        pull(!!quo_var_name) %>%
        unique()
      
      grouped_data <- map_dfr(summariser_local[["list of responses"]], function(response) {
        survey_data %>%
          group_by(!!quo_group_var) %>%
          mutate(`___TEMP_VAR___` = (!!quo_var_name == response)*1) %>%
          summarise(percent_overall = summariser_local[["stat function"]](`___TEMP_VAR___`,
                                                                          vartype = summariser_local[["vartype"]],
                                                                          level = summariser_local[["significance_level"]],
                                                                          na.rm = TRUE,
                                                                          proportion = TRUE)) %>%
          mutate(!!quo_var_name := response)
      }) %>%
        left_join(temp_count, by = c(quo_name(quo_group_var),
                                     quo_name(quo_var_name)))
      
      # Checking if the data is flipped
      if (summariser_local[["type"]] == "flipped") {
        .temp <- temp_result %>%
          gather(key, value, -!!quo_var_name) %>%
          rename(temp_var = !!quo_var_name) %>%
          mutate(key = case_when(
            key == "total" ~ glue::glue("n_{temp_var}"),
            TRUE ~ str_replace(key, "overall", temp_var)
          )) %>%
          select(-temp_var) %>%
          spread(key, value, fill = 0) %>%
          mutate(!!quo_group_var := "Overall")
        
        temp_result <- grouped_data %>%
          gather(key, value, -!!quo_group_var, -!!quo_var_name) %>%
          rename(temp_var = !!quo_var_name) %>%
          mutate(key = case_when(
            key == "n" ~ glue::glue("n_{temp_var}"),
            TRUE ~ str_replace(key, "overall", temp_var)
          )) %>%
          select(-temp_var) %>%
          spread(key, value, fill = 0) %>%
          bind_rows(.temp, .)
        
      } else {
        temp_result <- grouped_data %>%
          gather(key, value, -!!quo_group_var, -!!quo_var_name) %>%
          rename(temp_var = !!quo_group_var) %>%
          mutate(temp_var = tolower(temp_var),
                 key = case_when(
                   key == "n" ~ glue::glue("n_{temp_var}"),
                   TRUE ~ str_replace(key, "overall", temp_var)
                 )) %>%
          select(-temp_var) %>%
          spread(key, value, fill = 0) %>%
          full_join(temp_result, ., by = quo_name(quo_var_name))
      }
    }
    
    if (summariser_local[["stat"]] == "mean") {
      temp_result <- temp_result  %>% mutate_at(vars(starts_with("percent_")), ~ dec_to_perc(., 3))
    } else {
      # Renaming variables if asked for total estimate
      temp_result <- temp_result  %>% rename_at(vars(starts_with("percent_")), ~ str_replace(., "percent_", "estimated_"))
    }
  }
  
  if (is.null(survey_design$weight)) message("Please take the SE and CI values with a pinch of salt as weights have not been provided")
  
  return(temp_result)
}

environment(summariser_base) <- hidden_env