library(tidyverse)
library(dplyr)
library(rstatix)
library(purrr)
library(sqldf)

cat("###### Rcode_safe_stats.R ######\n# Use these functions to conduct pairwise tests across subsets of data... ")
cat("\n# safe_pairwise_t_tests ")
cat("\n# safe_pairwise_wilcox_tests ")
cat("\n")

pstars  <- function(p, show_p_LT_point1 = F)
{
  p_LT_point1  <- ifelse(show_p_LT_point1==T, "+", "")
  
  stars <-
    ifelse(p < .001, "***",
    ifelse(p < .01, "**",
    ifelse(p < .05, "*",
    ifelse(p < .10, p_LT_point1,
    ""))))
  
  return(stars)
}

safe_wilcox_test <- function(data, x_var, g_var, g1, g2, ndigits) {
    # calculate N's in case it fails
  # n1  <- data  %>% filter(.data[[g_var]] == g1) %>% nrow()
  # n2  <- data  %>% filter(.data[[g_var]] == g2) %>% nrow()
  formula_str <- as.formula(paste(x_var, "~", g_var))
  
  # return
  tryCatch(
    {
      tmp  <- rstatix::wilcox_test(data, formula = formula_str
                                   #, comparisons = list(c(g1,g2))
                                    , detailed = T) %>%
        mutate(.test_status = "PASS", .err_msg="") %>% 
                mutate(estimate = round(estimate, ndigits)
              , statistic = round(statistic, 2)
              , conf.low = round(conf.low, ndigits)
              , conf.high = round(conf.high, ndigits))
    },
    error = function(e) {
      msg <-  conditionMessage(e)
      tibble(
        estimate = NA,
        .y. = x_var,
        group1 = g1,
        group2 = g2,
        statistic = NA,
        p = NA,
        conf.low = NA,
        conf.high = NA,
        method = "Wilcoxon",
        alternative = NA,
        .test_status = "FAIL",
        .err_msg = msg)
    }
  )
}


safe_t_test <- function(data, x_var, g_var, g1, g2, ndigits) {
  # calculate N's in case it fails
  # n1  <- data  %>% filter(.data[[g_var]] == g1) %>% nrow()
  # n2  <- data  %>% filter(.data[[g_var]] == g2) %>% nrow()
  
  formula_obj <- reformulate(g_var, response = x_var)
  # return
  tryCatch(
    {
      t_test(data, formula = formula_obj, detailed = T) %>%
        mutate( .test_status = "PASS") %>% 
        select(-c(estimate1, estimate2)) %>% 
        select(estimate, .y., group1, group2
               , statistic, p, df, conf.low, conf.high
               , method, alternative, .test_status) %>% 
        mutate(estimate = round(estimate, ndigits)
               , df = round(df, 2)
               , statistic = round(statistic, 2)
               , conf.low = round(conf.low, ndigits)
               , conf.high = round(conf.high, ndigits))
      
    },
    error = function(e) {
      tibble(
        estimate = NA,
        # estimate1 = NA,
        # estimate2 = NA,
        .y. = x_var,
        group1 = g1,
        group2 = g2,
        statistic = NA,
        p = NA,
        conf.low = NA,
        conf.high = NA,
        method = "T-test",
        alternative = NA,
        .test_status = "FAIL" )
    }
  )
}

# df  <- flo
# x_var = "frqlive"
# g_var="grp3"
# subset_vars =  c("smpltyp","pop")
# testtype = "Wilcox"
# ndigits = 3
# show_p_LT_point1 = F

safe_descstats  <- function(df, x_var, g_var, subset_vars, ndigits)
{
  df_descstats <- df %>% 
    filter(!is.na(.data[[x_var]])) %>% 
    group_by(!!!syms(c( g_var, subset_vars))) %>% 
    summarize(M = round(mean(.data[[x_var]]), ndigits)
              , SD = round(sd(.data[[x_var]]), ndigits)
              , MAX = round(max(.data[[x_var]]), ndigits)
              ) %>% ungroup() %>% 
    mutate(grpnum = as.numeric(.data[["grp3"]]))

  return(df_descstats)
}


safe_pairwise_tests <- function(df, x_var, g_var, subset_vars, testtype, ndigits, show_p_LT_point1)
{
  # check that g_var is a factor
  is_factor <- class(df[[g_var]])

  if(is_factor!="factor")
  {
    cat(paste0("\n'",g_var , "' is not a factor.  Please correct.\n"))
    return()
  } 

  final_results_list  <- list()
  for(k in 1:length(x_vars)) 
  {
      x_var  <- x_vars[k]
    cat(" . . . (", i,") . . . ", x_var, " . . . \n")
    # get the descriptive stats
    descstats  <- safe_descstats(df, x_var, g_var
                 , subset_vars, ndigits) %>% 
            mutate(grp = .data[[g_var]])
  
    # create the subsets
    subsets <- df %>% distinct(across(all_of(subset_vars)))
    # sort the columns 
    subsets <- subsets[do.call(order, as.data.frame(subsets)), ] %>% 
      mutate(subsetpk = row_number())
    
    # get the various levels in g_var (e.g., the groups to compare)
    group_levels <- levels(df[[g_var]])
    # get each pair of groups
    group_pairs <- combn(group_levels, 2, simplify = FALSE)
    
    resultsets  <- list()
    for(i in 1:nrow(subsets))
    {
      # i <- 2
      cat(i)
      if(i%%5 == 0) {cat(".")}
      if(i%%50 == 0) {cat("\n")}
      # filter by the subset
      tmp <- df
      for(c in 1:length(subset_vars)) 
      {    
        col  <- subset_vars[c]
        colval  <- subsets[i, c][1] %>% pull()
        tmp <- tmp %>% filter(.data[[col]] == colval)
      }
      
      # loop through the pairs of groups
      results <- list()
      for(g in 1:length(group_pairs))
      {
        # g <- 3
        g1 <-  group_pairs[[g]][1]
        g2 <-  group_pairs[[g]][2]
        
        # cat(paste("pre:",dim(tmp)))
        # filter: only group1 and group2, remove NA values of x_var
        tmppair  <- tmp %>% filter(.data[[g_var]] %in% group_pairs[[g]] & 
                                 !is.na(.data[[x_var]]))
        # cat(paste("  post:",dim(tmppair)), "\n")
        
        # drop unused levels 
        tmppair[ , g_var] <- droplevels(tmppair[ , g_var])
  
        # formula_obj <- reformulate(g_var, response = x_var)
        # formula_str <- as.formula(paste(x_var, "~", g_var))
        
        # print(paste(g1, g2))
        if(testtype == "T-test") {
          result <- safe_t_test(tmppair, x_var, g_var, g1, g2, ndigits)
        } else if(testtype == "Wilcox") {
          result <- safe_wilcox_test(tmppair, x_var, g_var, g1, g2, ndigits)
        }
        
        results[[g]] <- result
      }
      resultset <- bind_rows(results) %>% mutate(subsetpk1 = i)
      resultsets[[i]] <- resultset
    }
    final_results <- bind_rows(resultsets)
    
    final_results  <- sqldf("select *
                            from subsets a
                            join final_results b ON a.subsetpk = b.subsetpk1")
    # drop the final subsetpk var
    
    final_results  <- final_results %>% select(-subsetpk1)
    
    final_results$pstars <- pstars(final_results$p, show_p_LT_point1)
    
    final_results <- final_results %>% relocate(pstars, .after = p)
  
    join_1  <- paste0("a.", subset_vars, "=b1.", subset_vars , collapse=" and ")
    join_2  <- paste0("a.", subset_vars, "=b2.", subset_vars , collapse=" and ")
    
    sqlcode  <- paste("select a.*
              , b1.M as m1, b1.SD as sd1, b1.grpnum g1_num
              , b2.M as m2, b2.SD as sd2, b2.grpnum g2_num
              , (case when b1.MAX >= b2.MAX then b1.MAX else b2.MAX end) max_y
              from final_results a
              left join descstats b1 ON a.group1 = b1.grp and ", join_1, 
              "left join descstats b2 ON a.group2 = b2.grp and ", join_2)
    
    final_results_list[[k]] <- sqldf(sqlcode)
  }

  combined_final_results  <- bind_rows(final_results_list)
  
  print(combined_final_results %>% count(.test_status, pstars))
  return(combined_final_results)
}



safe_pairwise_t_tests <- function(df, x_vars, g_var, subset_vars, ndigits=3, show_p_LT_point1=F) {
  result <- safe_pairwise_tests(df, x_var, g_var, subset_vars, "T-test", ndigits, show_p_LT_point1)
  return(results)
} 

safe_pairwise_wilcox_tests <- function(df, x_vars, g_var, subset_vars, ndigits=3, show_p_LT_point1=F) {
  result <- safe_pairwise_tests(df, x_vars, g_var, subset_vars, "Wilcox", ndigits, show_p_LT_point1)
  return(result)
} 







