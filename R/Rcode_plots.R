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
cat("\n###### Rcode_plots.R ######\n# Use these functions to create faceted plots with 1 row... ")
# Print to the console some header/info when the file is sourced / functions are used.
cat("\n# facet_plots_1row ")
cat("\n# facet_plots_1row_add_sig ")
cat("\n")


# plot_pairwise_tests: convenience wrapper that builds plots for each variable found in dfstats.
# - df: main data frame containing raw/processed observations
# - dfstats: data frame containing summary / pairwise test results (used to annotate plots)
# - group_by: string name of column to split plots by (one plot per group level)
# - facet_by: string name of column used as facet variable inside each plot (facet_wrap)
# - x_var: string name of column to use on the x-axis
# - color_var: string name of column to map to color
# - xlab: label for x-axis
# - mytheme: a callable that returns a ggplot2 theme (e.g. function() theme_minimal())
# - mycolorscale: a ggplot2 scale (e.g. scale_color_manual(...)) or expression
# - n_facet_rows: number of rows to use in facet_wrap (default 1)
plot_pairwise_tests  <- function(df, dfstats, group_by, facet_by
            , x_var, color_var
            , xlab, mytheme, mycolorscale
            , n_facet_rows = 1
            , add_sig = F)
{
  cat("\n")
  # Count distinct variables reported in dfstats (assumes a column 'variable' exists)
  vars  <- dfstats %>% count(variable)

  # Initialize a named list to hold final annotated plots (one list element per variable)
  plot_list_by_var  <- list()

  for(i in 1:nrow(vars))
  {
    my_y_var <- vars$variable[i]

    cat("# Plotting: ", my_y_var, " . . . \n")

    # Build base plot(s) for this y-variable. facet_plots_1row returns a list of plots,
    # one for each level of the 'group_by' variable.
    p  <- facet_plots_1row(df, dfstats, group_by, facet_by
            , x_var, my_y_var, color_var
            , xlab, my_y_var, mytheme, mycolorscale
            , n_facet_rows)

    if(add_sig==T) {
    # Add significance annotations using the subset of dfstats for this variable.
    # facet_plots_1row_add_sig expects the plot list and the sigstats corresponding to the variable.
    psig <- facet_plots_1row_add_sig(p, 
      dfstats %>% filter(variable == my_y_var))

    # Store annotated plots keyed by variable name
    plot_list_by_var[[my_y_var]] <- psig
    } else {
       plot_list_by_var[[my_y_var]] <- p
    }
  }

  return(plot_list_by_var)
}


# Define function facet_plots_1row:
# - df: input data.frame / tibble
# - group_by: column name (string) used to create separate plots (one plot per level)
# - facet_by: column name (string) used to create facets within each plot (facet_wrap)
# - x_var, y_var, color_var: column names (strings) for aesthetics
# - xlab, ylab: axis labels (strings)
# - mytheme: function returning a ggplot2 theme (callable)
# - n_facet_rows: number of rows to use in facet_wrap (default 1)
facet_plots_1row <- function(df, dfstats, group_by, facet_by, x_var, y_var, color_var
                             , xlab, ylab, mytheme, mycolorscale, n_facet_rows = 1)
{
  # Check whether a "value_source" column exists in the data frame 'df'
  # NOTE: this line refers to 'cyt' which is not defined inside this function.
  # That means this will error unless 'cyt' exists in the parent environment.
  # The intent appears to be to detect if the data contains a 'value_source' column.
  has_value_source  <- ifelse("value_source" %in% names(df), T, F)

  # If 'value_source' exists, collapse any entries containing "estim" to the string "estim".
  # This standardizes different estimator labels into one "estim" category.
  if(has_value_source) {
    df$value_source <- ifelse(str_detect(df$value_source, "estim"), "estim", df$value_source)
    show_shape_legend <- TRUE
  } else {
    df$value_source  <- "measured"
    show_shape_legend <- FALSE
  }

  # Get the unique levels (values) of the grouping variable so we can build one plot per level.
  group_by_levs  <- unique(df[[group_by]])

  # Initialize an empty list to store the ggplot objects we will create.
  plots1  <-  list()

  # Loop over each level of the group_by variable to create a separate plot for that level.
  for(i in 1:length(group_by_levs))
  {
    # Print the current group value to the console (progress indicator).
    cat(group_by_levs[i], "..")

    # Create a title string for the plot using the group value.
    mytitle  <- group_by_levs[i]

    # Subset the data to the rows matching the current group level and there is no missing data
    tmp <- df %>% filter(.data[[group_by]] == group_by_levs[i] & 
                        !is.na(.data[[y_var]]))
    if (nrow(tmp) == 0) {
    # placeholder plot
      cat("0recs")
       p1  <- ggplot() +  annotate("text", x = 0.5, y = 0.5, 
        label = paste0("No data available for [", y_var, "] when ", group_by," = '", group_by_levs[i],"'" ), size = 4) +
        theme_void()
        
        mytitle <- paste0(mytitle, "_no_data")
    } else {
                          
    # Build the ggplot for this subset:
    p1 <- ggplot(tmp)+
      # Manually define the colors used for the color aesthetic.
      #scale_color_manual(values = c("blue","darkred","red"))+
      mycolorscale +
      # Manually set the shapes used for the shape aesthetic.
      scale_shape_manual(values=c("estim" = 4, "measured" = 19))+
      # Add summary statistics as "pointrange": mean +/- 1 SD (mean_sdl with mult=1).
      # - geom: pointrange
      # - fun.data = "mean_sdl": compute mean and sd
      # - aes: map x, y and color to columns specified by x_var, y_var and color_var
      # - alpha, pch, size, lwd: styling for the summary geom
      stat_summary(geom="pointrange", fun.data = "mean_sdl", fun.args=list(mult=1)
                   , aes(x= .data[[x_var]], y=.data[[y_var]], color=.data[[color_var]])
                   , alpha=.4, pch=18, size=1.5, lwd=1) + 
      # Add individual data points using geom_beeswarm (from ggbeeswarm package)
      # - beeswarm spreads points to avoid overplotting while keeping x positions
      # - pch is mapped to value_source (if present) so points can use different shapes
      geom_beeswarm(aes(x= .data[[x_var]], y=.data[[y_var]], color=.data[[color_var]]
                        , pch=value_source), size=2, cex=2.5) + 
      # Facet the plot by the 'facet_by' variable, allow each facet to have its own y-scale
      # nrow is controlled by n_facet_rows parameter (often 1 for a single row layout).
      facet_wrap(as.formula(paste("~", facet_by)), scales="free_y",nrow=n_facet_rows ) + 
      # (Commented out) alternative y-scale transformation: log10. Left disabled.
      # scale_y_log10( labels = comma_format(big.mark = ",", decimal_mark="."))
      # Use continuous y-scale with formatted labels (commas for thousands).
      scale_y_continuous( labels = comma_format(big.mark = ",", decimal_mark=".")) +
      # Set axis labels
      xlab(xlab) + ylab(ylab) + 
      # Add title and subtitle; title includes the group_by variable name and its value.
      labs(title=paste0(group_by,"=",mytitle)
           , subtitle="")+
      # show the shape legend conditionally
      guides(shape = if (show_shape_legend) guide_legend() else "none")+
      # Apply the user-provided theme function. Note that mytheme is expected to be a function
      # that returns a ggplot2 theme (e.g., function() theme_minimal()).
      mytheme()
   }

    # Store the created plot in the list.
    cat(mytitle)
    plots1[[mytitle]] <- p1
    # names(plots1)[i] <- mytitle
  }

  # Print a newline after the loop's progress output.
  cat("\n")

  # Return the list of ggplot objects (one per group_by level).
  return(plots1)
}




# Define function facet_plots_1row_add_sig:
# - plotlist: list of ggplot objects (expected format produced by facet_plots_1row)
# - sigstats: a data.frame/tibble produced by safe_pairwise_tests 
#   which contains pairwise comparison statistics (p-values, group indices, max y-values, etc.)
facet_plots_1row_add_sig  <- function(plotlist, sigstats, mymethod="T-test")
{
  # The sigstats input is expected to be a data.frame with columns such as:
  # - method: name of the statistical test
  # - variable: target y variable name (string)
  # - g1_num, g2_num: numeric identifiers for comparison groups along the x-axis
  # - p: p-value for the pairwise comparison
  # - pstars: human-readable stars or formatting for significance
  # - max_y: maximum y observed in the group (used to position labels)
  #
  # We will iterate through each plot in plotlist and add significance annotations
  # (labels) for pairwise comparisons that are significant (p < 0.05).

  sigtxtsz <- 2 # text size for sig differences

  outlist <- list()

  # Loop over each plot in the provided list.
  for(i in 1:length(plotlist))
  {
    mytitle <- names(plotlist)[i]
    if(str_detect(mytitle, "no_data"))
      {
         outlist[[i]] <- plotlist[[i]]   
         names(outlist)[i] <- mytitle
      } else {
          
                # Extract the existing subtitle from the plot (if any).
                mysubtitle  <- plotlist[[i]]$labels[["subtitle"]]
            
                # Extract the title that contains something like "group_by=group_value"
                grpby_var_val  <- plotlist[[i]]$labels[["title"]]
            
                # Derive the group_by variable name by splitting the title at "=" and taking the left part.
                grpby_var <- str_split(grpby_var_val, "=")[[1]][1]
            
                # Derive the group_by value (the specific level) by splitting the title at "=" and taking the right part.
                grpby_val <- str_split(grpby_var_val, "=")[[1]][2]
            
                # Extract the y-axis variable name used in the plot (from the plot labels)
                y_var  <- plotlist[[i]]$labels[["y"]]
                        
                # Filter the sigstats table to only the comparisons relevant to the current group-by level
                # and the current y variable. This assumes sigstats contains a column with the group_by name.
                tmp_sigstats <- sigstats %>% 
                  filter(.data[[grpby_var]] == grpby_val & 
                           variable == y_var &
                           method == mymethod)

                # Extract the method (stat test) from the first row of sigstats (assumes consistent method)
                method  <- sigstats$method[1]

                # Create a human-readable label for each comparison: "g1 vs g2 <pstars>"
                tmp_sigstats$mylabel  <- 
                  ifelse(is.na(tmp_sigstats$sig_result), "", 
                  ifelse(tmp_sigstats$sig_result=="", "",
                    paste0(tmp_sigstats$sig_result, tmp_sigstats$pstars)))
            
                # Compute a central x position for the label by averaging the numeric group indices.
                # This is used to position the label between the two groups on the x-axis.
                tmp_sigstats$grpsnum  <- (tmp_sigstats$g1_num + tmp_sigstats$g2_num)/2.0
            
                # Compute a y position slightly above the observed maximum y for the comparison:
                # - mylabel_y is max_y multiplied by (1 + (grpsnum / 10))
                #   This nudges labels higher when comparing groups with higher numeric positions.
                tmp_sigstats$mylabel_y  <- tmp_sigstats$max_y  *
                    (1+(tmp_sigstats$grpsnum  / 10))   # e.g., 15%, 20%, or 25% up depending on grpsnum
            
                # Compute an even higher value that will be used with geom_blank to ensure plot limits
                # include enough space for the label. This is a safety margin (30% more).
                tmp_sigstats$mylabel_maxy <- tmp_sigstats$mylabel_y * 1.3 # 30% up
            
                # Optionally print the tmp_sigstats for debugging (commented out in original).
                # print(tmp_sigstats)
            
                # Add annotations to the plot for comparisons where p < 0.05:
                pout  <- plotlist[[i]] + 
                  # geom_blank with y = mylabel_maxy ensures the plot's y-limits stretch high enough
                  # to accommodate labels; we only add blank data for significant comparisons.
                  geom_blank(data = tmp_sigstats %>% filter(p < .05) 
                             , aes(y = mylabel_maxy) ) +
                # Add a semi-transparent label for each significant pairwise comparison:
                geom_label(data = tmp_sigstats %>% filter(p < .05) 
                    , aes(x=grpsnum, y = mylabel_y, label= mylabel)
                     # , direction = "x", min.segment.length = 5
                    , vjust=0.5, color="black", size=sigtxtsz ,alpha=.5) + 
                  # Update the subtitle to indicate that pairwise significant comparisons are displayed
                  labs(subtitle = paste(mysubtitle, 
                      " (significant pairwise ", method, "displayed)"))
            
                  # Append the annotated plot to the output list.
                  outlist[[i]] <- pout
                  names(outlist)[i] <- mytitle
              }
      }

  # Return the list of annotated ggplot objects.
    return(outlist)
}


# End of file

















