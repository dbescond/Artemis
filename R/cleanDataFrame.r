#' clean dataframe
#'
#' Clean dataframe, return a tbl, with character colums, clean up NA and trim characters 
#'
#' Helper function to efficiently query data from ILOSTAT SDMX API.
#'
#' @param df, data frame to clean.
#' @param header, header yet define by default TRUE,  or store at the first row.
#' @author ILO bescond
#' @keywords ILO, SDMX, R
#' @seealso \code{\link{getData}} \code{\link{getDataStructure}}
#' @importFrom stringr str_trim
#' @export



cleanDataFrame 	<-	function(df, header  = TRUE, tolower = FALSE){

	if(header){		
		df <- df %>%
			mutate_each(funs(as.character),everything() ) %>%
			mutate_each(funs(str_trim),everything() ) %>%
			mutate_each(funs(mapvalues(.,c('NaN', '', 'NA'), c(NA,NA, NA), warn_missing = FALSE)), everything()) 
	} else {
		ref <- df %>% slice(1) %>% t %>% c
		colnames(df)[!is.na(ref)] <- ref[!is.na(ref)]
		df <- df[,ref[!is.na(ref)]] %>% as.tbl %>%
			slice(-1) %>%
			mutate_each(funs(as.character),everything() ) %>%
			mutate_each(funs(str_trim),everything() ) %>%
			mutate_each(funs(mapvalues(.,c('NaN', '', 'NA'), c(NA,NA, NA), warn_missing = FALSE)), everything())
	}
	if(tolower   %in% TRUE) {
	df <- eval(parse(text= paste0("  df %>% rename(",paste0(tolower(colnames(df))," = ", colnames(df), collapse = ", "),")")))
	}
	df
} 
