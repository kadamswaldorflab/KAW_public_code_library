# Rcode_plots.R

########################################################################################
#            ,,                        
#`7MM"""Mq.`7MM           mm           
#  MM   `MM. MM           MM           
#  MM   ,M9  MM  ,pW"Wq.mmMMmm ,pP"Ybd 
#  MMmmdM9   MM 6W'   `Wb MM   8I   `" 
#  MM        MM 8M     M8 MM   `YMMMa. 
#  MM        MM YA.   ,A9 MM   L.   I8 
#.JMML.    .JMML.`Ybmd9'  `MbmoM9mmmP' 
#                                      
########################################################################################


# FACET_PLOTS_1_ROW -----
cat("###### Rcode_plots.R ######\n# Use these functions to create faceted plots with 1 row... ")
cat("\n# facet_plots_1row ")
cat("\n# facet_plots_1row_add_sig ")
cat("\n")


facet_plots_1row <- function(df, group_by, facet_by, x_var, y_var, color_var
                             , xlab, ylab, mytheme, n_facet_rows = 1)
{
  has_value_source  <- ifelse("value_source" %in% names(cyt), T, F)
  if(has_value_source) {
    df$value_source <- ifelse(str_detect(df$value_source, "estim"), "estim", df$value_source)
  }

  # get the levels of the var you are going to group by (1 plot per level)
  group_by_levs  <- unique(df[[group_by]])
  plots1  <-  list()
  for(i in 1:length(group_by_levs))
  {
    cat(group_by_levs[i], "..")
    mytitle  <- group_by_levs[i]
    tmp <- df %>% filter(.data[[group_by]] == group_by_levs[i])
    p1 <- ggplot(tmp)+
      scale_color_manual(values = c("blue","darkred","red"))+
      scale_shape_manual(values=c(4,19))+
      stat_summary(geom="pointrange", fun.data = "mean_sdl", fun.args=list(mult=1)
                   , aes(x= .data[[x_var]], y=.data[[y_var]], color=.data[[color_var]])
                   , alpha=.4, pch=18, size=1.5, lwd=1) + 
      geom_beeswarm(aes(x= .data[[x_var]], y=.data[[y_var]], color=.data[[color_var]]
                        , pch=value_source), size=2, cex=2.5) + 
      facet_wrap(as.formula(paste("~", facet_by)), scales="free_y",nrow=n_facet_rows ) + 
      # scale_y_log10( labels = comma_format(big.mark = ",", decimal_mark=".")) +
      scale_y_continuous( labels = comma_format(big.mark = ",", decimal_mark=".")) +
      xlab(xlab) + ylab(ylab) + 
      labs(title=paste0(group_by,"=",mytitle)
           , subtitle="")+
      mytheme()
    
    plots1[[i]] <- p1
  }
  cat("\n")
  return(plots1)
}


facet_plots_1row_add_sig  <- function(plotlist, sigstats)
{
  # sigstats generated from safe_pairwise_t_tests
  #                      or safe_pairwise_wilcox_tests  (see Rcode_safe_stats.R)
  outlist <- list()
  for(i in 1:length(plotlist))
  {
      
    mysubtitle  <- plotlist[[i]]$labels[["subtitle"]]
    grpby_var_val  <- plotlist[[i]]$labels[["title"]]
    grpby_var <- str_split(grpby_var_val, "=")[[1]][1]
    grpby_val <- str_split(grpby_var_val, "=")[[1]][2]
    y_var  <- plotlist[[i]]$labels[["y"]]
    method  <- sigstats$method[1]
    tmp_sigstats <- sigstats %>% 
      filter(.data[[grpby_var]] == grpby_val & 
               .y. == y_var)

    tmp_sigstats$mylabel  <- paste0(tmp_sigstats$g1_num, " vs ",  tmp_sigstats$g2_num, " ", tmp_sigstats$pstars)
    tmp_sigstats$grpsnum  <- (tmp_sigstats$g1_num + tmp_sigstats$g2_num)/2.0
    tmp_sigstats$mylabel_y  <- tmp_sigstats$max_y  *
        (1+(tmp_sigstats$grpsnum  / 10))   # 15, 20, or 25% up
    tmp_sigstats$mylabel_maxy <- tmp_sigstats$mylabel_y * 1.3 #30% up
    
    # print(tmp_sigstats)
    pout  <- plotlist[[i]] + 
      geom_blank(data = tmp_sigstats %>% filter(p < .05) 
                 , aes(y = mylabel_maxy) ) +
    geom_label(data = tmp_sigstats %>% filter(p < .05) 
        , aes(x=grpsnum, y = mylabel_y, label= mylabel)
         # , direction = "x", min.segment.length = 5
        , vjust=0.5, color="black", size=2,alpha=.5) + 
      labs(subtitle = paste(mysubtitle, 
          " (significant pairwise ", method, "displayed)"))
    
      outlist[[i]] <- pout
  }
  
    return(outlist)
}



