# Rcode_files.R  

#             ,,    ,,                  
#  `7MM"""YMM db  `7MM                  
#    MM    `7       MM                  
#    MM   d `7MM    MM  .gP"Ya  ,pP"Ybd 
#    MM""MM   MM    MM ,M'   Yb 8I   `" 
#    MM   Y   MM    MM 8M"""""" `YMMMa. 
#    MM       MM    MM YM.    , L.   I8 
#  .JMML.   .JMML..JMML.`Mbmmd' M9mmmP' 
#                                      
                                     
cat("\n###### Rcode_files.R ######")
cat("\n# R functions for loading and saving files...")
cat("\n# loaddf")
cat("\n# savedf")
cat("\n")

loaddf <- function( df_to_load, vers, path, printflag=T)
{
  if(class(df_to_load)!="character")
  {
    cat("\n\nFirst parameter must be a string.\n\n")
    return (NA)
  }
  filepath <- paste(path,df_to_load,vers,".rds",sep="")
  if(printflag)
  {
    cat(paste("\n...Loading ",df_to_load," version:",vers,"   '", filepath, "'\n",sep=""))
  }
  dif <-  Sys.time() - file.mtime(filepath)
  if(printflag)
  {
    cat(paste("...created",round(dif[[1]],2),units(dif),"ago at",file.mtime(filepath),"\n")) 
  }
  
  result = tryCatch({
    readRDS(filepath)  #was just this command before the tryCatch
  }, error = function(e) {
    print(paste("ERROR:",e))
  })
  if(printflag)
  {
    if(class(result)[1]=="list")
    {
      tot_n      <- length(result)
      n_in_list  <-  length(names(result)[which(names(result) != "dfvers")] )
      if(tot_n > n_in_list) {
        result  <- result[1:(n_in_list)]
      }
      cat(paste0("...list with ", n_in_list, " elements"))
    } else {
      cat(paste0("...nrows = ", nrow(result), "  ...ncols = ", ncol(result) ,"\n"))   
    }
  }
  
  return(result)
}




savedf <- function( df_to_save, vers, path, printflag=T)
{
  dfname <- deparse(substitute(df_to_save))
  pathname <- deparse(substitute(path))
  
  if(path == pathname) pathname <- paste0("\"", path, "\"")
  
  filepath <- paste(path,dfname,vers,".rds",sep="")
  df_to_save$dfvers <- vers
  saveRDS(df_to_save, filepath)
  if(printflag)
  {
    cat(paste("\nDataFrame '",dfname,"' version:",vers," saved to ", filepath ,"\n",sep=""))
    
    cat(paste0("\n## Load it in the future with: \n", dfname, 
               " <- loaddf(\"", dfname , "\", ", vers, ", ", pathname, ")"  ))
  }
}


savedf2 <- function( df_to_save, vers, path, newname_for_df, printflag=T)
{
  filepath <- paste(path,newname_for_df,vers,".rds",sep="")
  df_to_save$dfvers <- vers
  saveRDS(df_to_save, filepath)
  if(printflag)
  {
    cat(paste("\nDataFrame '",newname_for_df,"' version:",vers," saved to ", filepath ,"\n",sep=""))
  }
}


# title generated via:
# https://patorjk.com/software/taag/#p=display&f=Georgia11&t=Files




