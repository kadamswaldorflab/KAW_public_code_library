library(tidyverse)
library(dplyr)
library(rstatix)
library(purrr)
library(sqldf)

cat("\n###### Rcode_safe_stats.R ######\n# Use these functions to conduct pairwise tests across subsets of data... ")
cat("\n# safe_pairwise_tests ")
cat("\n")

safe_descstats  <- function(df, x_var, g_var, subset_vars, ndigits)
{
  df_descstats <- df %>% 
    filter(!is.na(.data[[x_var]])) %>% 
    mutate(rank = rank(.data[[x_var]])) %>%
    group_by(!!!syms(c( g_var, subset_vars))) %>% 
    summarize(  N = n()
              , M = round(mean(.data[[x_var]]), ndigits)
              , SD = round(sd(.data[[x_var]]), ndigits)
              , MAX = round(max(.data[[x_var]]), ndigits)
              ) %>% ungroup() %>% 
    mutate(grpnum = as.numeric(.data[[g_var]]))

  df_rank_sum <- df %>%
     filter(!is.na(.data[[x_var]])) %>% 
      group_by(!!!syms(c( subset_vars))) %>% 
      mutate(rank = rank(.data[[x_var]])) %>%
      ungroup() %>% 
      group_by(!!!syms(c( g_var, subset_vars))) %>% 
      summarize( SUM_RANKS = sum(rank)) %>% ungroup()
  
  df_final <- inner_join(df_descstats, df_rank_sum, by=c(g_var, subset_vars))

  return(df_final)
}


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

is_na_or_zero  <- function(x1, x2) {
  #y <- ifelse(is.na(x1) | is.na(x2) | x1==0 | x2==0, T, F)
  y <- ifelse(is.na(x1), T, ifelse(is.na(x2), T, ifelse( x1==0 | x2==0, T, F)))
  return(y)
}

# adds the "sig_result" variable which is populated when a result is p < .05 and 
#  shows the direction of effect.  (E.g., when group 1 is greater than group 2, this fx returns "1>2") 
sig_result  <- function(df){
  df <- df %>% 
    mutate(sig_result = ifelse(pstars=="", "",
          paste0(g1_num, ifelse(statistic < 0, "<",">"), g2_num)))
           
  df$sig_result  <-  coalesce(df$sig_result,"")
  return(df)
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
        mutate(test_status = "PASS", err_msg="") %>% 
                mutate(estimate = round(estimate, ndigits)
              , statistic = round(statistic, 2)
              , conf.low = round(conf.low, ndigits)
              , conf.high = round(conf.high, ndigits)
              , pstars = pstars(p)) %>% 
        rename(variable = .y.) %>%
      select(-c(n1, n2))
    },
    error = function(e) {
      msg <-  conditionMessage(e)
      tibble(
        estimate = NA,
        variable = x_var,
        group1 = g1,
        group2 = g2,
        statistic = NA,
        p = NA,
        pstars=NA,
        conf.low = NA,
        conf.high = NA,
        method = "Wilcoxon",
        alternative = NA,
        test_status = "FAIL",
        err_msg = msg)
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
        mutate( test_status = "PASS", err_msg="") %>% 
        select(-c(estimate1, estimate2)) %>% 
        select(estimate, .y., group1, group2
               , statistic, p, df, conf.low, conf.high
               , method, alternative, test_status, err_msg) %>% 
        mutate(estimate = round(estimate, ndigits)
               , df = round(df, 2)
               , statistic = round(statistic, 2)
               , conf.low = round(conf.low, ndigits)
               , conf.high = round(conf.high, ndigits)
               , pstars = pstars(p)) %>% 
        rename(variable = .y.)
      
    },
    error = function(e) {
      msg <-  conditionMessage(e)
      tibble(
        estimate = NA,
        # estimate1 = NA,
        # estimate2 = NA,
        variable = x_var,
        group1 = g1,
        group2 = g2,
        statistic = NA,
        p = NA,
        pstars = NA,
        conf.low = NA,
        conf.high = NA,
        method = "T-test",
        alternative = NA,
        test_status = "FAIL",
        err_msg = msg )
    }
  )
}


safe_pairwise_test <- function(df, x_vars, g_var, subset_vars, testtype, ndigits, show_p_LT_point1)
{
  # check that g_var is a factor
  is_factor <- class(df[[g_var]])

  if(is_factor!="factor")
  {
    cat(paste0("\n'",g_var , "' is not a factor.  Please correct.\n"))
    return()
  } else {
    cat(" . . . Pairwise comparisons across levels of ", g_var, ". . . \n")
  }

  final_results_list  <- list()
  for(k in 1:length(x_vars)) 
  {
    x_var  <- x_vars[k]
    cat("\n . . . (", k,") . . . ", testtype, " . . . ", x_var, " . . . \n")
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
      if(i%%25 == 0) {cat("\n")}
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
        g1 <-  group_pairs[[g]][1]
        g2 <-  group_pairs[[g]][2]
        
        tmppair  <- tmp %>% filter(.data[[g_var]] %in% group_pairs[[g]] & 
                                 !is.na(.data[[x_var]]))
        
        tmppair[ , g_var] <- droplevels(tmppair[ , g_var])
          
        if(testtype == "T-test") {
          result <- safe_t_test(tmppair, x_var, g_var, g1, g2, ndigits)
        } else if(testtype == "Wilcoxon") {
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

    final_results <- final_results %>% relocate(pstars, .after = p)
      
    join_1  <- paste0("a.", subset_vars, "=b1.", subset_vars , collapse=" and ")
    join_2  <- paste0("a.", subset_vars, "=b2.", subset_vars , collapse=" and ")
    
    sqlcode  <- paste("select a.*
              , b1.N as n1, b1.M as m1, b1.SD as sd1, b1.SUM_RANKS sum_ranks1
              , b2.N as n2, b2.M as m2, b2.SD as sd2, b2.SUM_RANKS sum_ranks2
              , b1.grpnum g1_num, b2.grpnum g2_num
              , (case when b1.MAX >= b2.MAX then b1.MAX else b2.MAX end) max_y
              from final_results a
              left join descstats b1 ON a.group1 = b1.grp and ", join_1, 
             "left join descstats b2 ON a.group2 = b2.grp and ", join_2)
    
    final_results_list[[k]] <- sqldf(sqlcode)
  }

  combined_final_results  <- bind_rows(final_results_list)

  combined_final_results  <- combined_final_results %>%  sig_result()      

  # QA check for zero variance
  combined_final_results$zerovar  <- is_na_or_zero(combined_final_results$sd1, combined_final_results$sd2)
  combined_final_results$statistic <- ifelse(combined_final_results$zerovar==TRUE
                                             , NA, combined_final_results$statistic)
  combined_final_results$p <- ifelse(combined_final_results$zerovar==TRUE
                                             , NA, combined_final_results$p)
  combined_final_results$pstars <- ifelse(combined_final_results$zerovar==TRUE
                                             , "", combined_final_results$pstars)
  combined_final_results$test_status <- ifelse(combined_final_results$zerovar==TRUE
                                             , "FAIL", combined_final_results$test_status)
  combined_final_results$err_msg <- ifelse(is.na(combined_final_results$sd1), "no obs. in 1+ groups",
                     ifelse(is.na(combined_final_results$sd2), "no obs. in 1+ groups",
              ifelse(combined_final_results$zerovar==TRUE, "zero variance in 1 or more groups", combined_final_results$err_msg)))
 

  # drop the zerovar var
  combined_final_results <- combined_final_results %>% select(-zerovar)
  
  # print a table of significant results 
  sig_table  <- combined_final_results %>% 
    mutate(sig = paste0(test_status, coalesce(pstars,"_NS"))) %>% 
    count(variable, group1, group2, test_status, sig)  %>% 
    pivot_wider(id_cols=c(variable, group1, group2 )
            , names_from = sig
            , values_from = n)
  
  print(sig_table)
  return(combined_final_results)
}


safe_pairwise_tests  <- function(df, x_vars, g_var
        , subset_vars
        , testtype="both", ndigits=3, show_p_LT_point1=F
        , format_stacked_or_wide = "stacked") {

  if(testtype!="both" & testtype!="T-test" & testtype!="Wilcoxon")
    {
       cat(paste0("\n'testtype' parameter must be one of the following: \"T-Test\", \"Wilcoxon\", \"both\". Please try again.\n"))
       return()
    }
  if(testtype=="both") {
    result_t <- safe_pairwise_test(df, x_vars, g_var, subset_vars, "T-test", ndigits, show_p_LT_point1)
    result_w <- safe_pairwise_test(df, x_vars, g_var, subset_vars, "Wilcoxon", ndigits, show_p_LT_point1)
    if(format_stacked_or_wide == "wide") { 
        result  <- merge_safe_t_wilc(result_t, result_w)    
    } else  {
        result  <- bind_rows(result_t, result_w)
    }
    } else if(testtype=="T-test") {
    result <- safe_pairwise_test(df, x_vars, g_var, subset_vars, "T-test", ndigits, show_p_LT_point1)
  } else if(testtype=="Wilcoxon") {
    result <- safe_pairwise_test(df, x_vars, g_var, subset_vars, "Wilcoxon", ndigits, show_p_LT_point1)
  }
  cat("\n . . Output format: ", format_stacked_or_wide, "\n")

    out  <- list("stats" = result
                 , "params" = list("x_vars" = x_vars
                                   , "g_var" = g_var
                                   , "subset_vars" = subset_vars))
    
  return(out)
}






merge_safe_t_wilc  <- function(df_ttest, df_wilctest){
df_pairwisetests  <- 
  sqldf("select a.* 
    , b.statistic as wilc_statistic
    , b.p as wilc_p
    , b.pstars as wilc_pstars
    , b.sig_result as wilc_sig_result
  from df_ttest a
    join df_wilctest b 
    ON  a.variable = b.variable
    AND a.subsetpk = b.subsetpk
    AND a.group1 = b.group1
    AND a.group2 = b.group2") %>% 
  rename(t_statitic = statistic, t_p = p, t_pstars = pstars, t_sig_result = sig_result)

return(df_pairwisetests)
}






