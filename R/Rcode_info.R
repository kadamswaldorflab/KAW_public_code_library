cat("\n###### Rcode_info.R ######")
cat("\n# info -> function to return summarized descriptive stats for a data frame")
cat("\n# usage:  df %>% info()")

info_dates <- function(x, digits=2, ...){
  
  daysdiff <- difftime( as.Date(max(na.omit(x)))
                        , as.Date(min(na.omit(x))), units="days")
  yrsdiff <- round((daysdiff / 365),2)
  
  c(N=length(na.omit(x)), Nunq=length(unique(na.omit(x)))
    , Min=as.character(as.Date(min(na.omit(x))))
    , Max=as.character(as.Date(max(na.omit(x))))
    , Span=paste(daysdiff, " days / ",yrsdiff, " yrs",sep="")
  )   }



info_numeric <- function(x, digits=2, ...){
  c(N=length(na.omit(x)), Nunq=length(unique(na.omit(x)))
    , M   =round(mean(na.omit(x)), digits)
    , SD  =round(sd(na.omit(x)), digits)
    , Mtrim05   =round(mean(na.omit(x), trim = .025), digits)
    , Med =round(median(na.omit(x)), digits)
    , Min =round(min(na.omit(x)), digits)
    , Max =round(max(na.omit(x)), digits)
    , Sum =round(sum(na.omit(x)), digits)
    #, Skew=round(skewness(na.omit(x)), digits)
    #, Kurt=round(kurtosis(na.omit(x)), digits)
  )   }


info_text <- function(x, digits=2, ...){
  
  x<-na.omit(x)
  
  c(N=length(x), Nunq=length(unique(x))
    , maxLeng=max(nchar(x))
    , PctMin= round(( length(x[which(x==min(x))])  / length(x) )*100,2)
    , PctMax= round(( length(x[which(x==max(x))])  / length(x) )*100,2)
    , Min=substr(min(x, ...),1,25), Max=substr(max(x, ...),1,25)
    , Mode = mode_txt(x)
  )   }


info_factor <- function(x, digits=2, ...){
  
  nlevs <- length(levels(x))
  levs <- paste0(levels(x))
  
  #x<-na.omit(as.character(x))
  
  c(N=length(x), Nunq=length(unique(x))
    , maxLeng=max(nchar(as.character(x)))
    , Nlevs = nlevs
    , Levels=fct_levels_info(x, nlevs)
  )   }


info <- function(df, mode="", newline="\n", dfname="", levelinfo="")
{
  options(tibble.print_max = Inf)
  
  outlist <- list()
  require(moments)
  require(dplyr)
  
  options(warn=-1) #turn off warnings
  
  continue <- ifelse(class(df)[1] %in% c("tbl_df","data.frame","grouped_df"), T, F)
  
  if(!continue) {
    print("##ERROR: Input parameter must be a data.frame, tibble, or grouped_df")
  }
  else
  {
    
    if(dfname=="")
    {
      dfname <- substitute(df)
      dfname <- ifelse (length(dfname) == 1, deparse(dfname) , sub("\\(.", "", dfname[2]))
    }
    
    # create the summary information
    dates <- data.frame(t(sapply(df[sapply( df, function(x) class(x)[1] %in% c("Date", "POSIXct"))], info_dates)))
    nums  <- data.frame(t(sapply(df[sapply( df, function(x) class(x)[1] %in% c("integer","numeric"))], info_numeric)))
    txts  <- data.frame(t(sapply(df[sapply( df, function(x) class(x)[1]=="character")], info_text)))
    fcts  <- data.frame(t(sapply(df[sapply( df, function(x) class(x)[1]=="factor")], info_factor)))
    
    dates <- add_rownames(dates, "VarName")
    nums <- add_rownames(nums, "VarName")
    txts <- add_rownames(txts, "VarName")
    fcts <- add_rownames(fcts, "VarName")
    
    if((nrow(dates) * ncol(dates))==1) dates$VarName[1] <- "No date vars."
    if(nrow(nums) * ncol(nums)==1) nums$VarName[1] <- "No numeric vars."
    if(nrow(txts) * ncol(txts)==1) txts$VarName[1] <- "No text vars."
    if(nrow(fcts) * ncol(fcts)==1) fcts$VarName[1] <- "No factor vars."
    
    # create objects
    dfinfo <- list()
    dfinfo[[1]] <- dfname
    dfinfo[[2]] <- levelinfo
    dfinfo[[3]] <- nrow(df)
    dfinfo[[4]] <- ncol(df)
    names(dfinfo) <- c("dfname","levelinfo","nrows","ncols")
    
    outlist[[1]] <- dfinfo
    outlist[[2]] <- dates
    outlist[[3]] <- nums
    outlist[[4]] <- txts
    outlist[[5]] <- fcts
    
    names(outlist) <- c("dfinfo","dates","nums","txts","fcts")
    class(outlist) <- "info"
    
    if(mode=="")
    {
      
    }
    
  }
  options(warn=0) #turn on warnings
  
  # print(outlist)
  return(outlist)
}



