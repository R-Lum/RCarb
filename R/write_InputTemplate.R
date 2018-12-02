#' @title Write table input template
#'
#' @description This function creates a template table that can be used as input for the function
#' [model_DoseRate]
#'
#' @param file [character] (optional): output path, if `NULL` nothing is written, but a template
#' [data.frame] is returned.
#'
#' @param ... additional arguments that can be passed to function [write.table] if `file != NULL`.
#' Supported arguments are: `sep`, `dec, `fileEncoding`
#'
#' @author Sebastian Kreutzer, IRAMAT-CRP2A, UMR 5060, CNRS - Université Bordeaux Montaigne (France)
#'
#' @section Function version:  0.1.0
#'
#' @examples
#'
#' ##create template without file creation
#' write_InputTemplate()
#'
#' \dontrun{
#' ##Example with file output
#' ##To run: uncomment and modify path
#' #write_InputTemplate(file = "[YOUR PATH]")
#'
#' }
#'
#' @seealso [Example_Data], [write.table]
#'
#' @md
#' @export
write_InputTemplate <- function(
  file = NULL,
  ...
){

  ##load example data, this is our starting point
  Example_Data <- NULL
  data("Example_Data", envir = environment())
  df <- Example_Data

  ##remove all rows, except the first
  df <- df[1,]

  ##replace name
  df$SAMP_NAME[1] <- "EXAMPLE"

  ##restore attributes
  df_list <- lapply(1:ncol(df), function(x){
    attr.names <- names(attributes(Example_Data[[x]]))
    attributes(df[[x]])[attr.names] <- attributes(Example_Data[[x]])[attr.names]
    return(df[[x]])
  })

  ##restore names
  names(df_list) <- colnames(Example_Data)

  ##transform to data.frame
  df <- as.data.frame(df_list,stringsAsFactors = FALSE)
  attr(df, which = "pacakge") <- "RCarb"

  # Output --------------------------------------------------------------------------------------
  if(is.null(file)){
    return(df)

  }else{

    ##allow some flexibility
    write_settings <- list(
      sep = ",",
      dec = ".",
      fileEncoding = ""

    )

    ##modify if needed
    write_settings <- modifyList(x = write_settings, val = list(...))

    ##write table
    write.table(
      x = df,
      file = file,
      append = FALSE,
      quote = FALSE,
      row.names = FALSE,
      col.names = TRUE,
      sep = write_settings$sep,
      fileEncoding = write_settings$fileEncoding
    )

  }

}