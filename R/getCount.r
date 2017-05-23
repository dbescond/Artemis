#' Query Data ILOSTAT count
#'
#' Query data from ILOSTAT SDMX API
#'
#' Helper function to efficiently query data from ILOSTAT SDMX API.
#'
#' @param DSD a datastructure definition identified by the triplet \code{[collection; country; indicator]}. Arguments description come from 'http://www.ilo.org/ilostat/content/conn/ILOSTATContentServer/path/Contribution Folders/statistics/web_pages/static_pages/technical_page/ilostat_appl/SDMX_User_Guide.pdf' .
#' @author ILO bescond  
#' @keywords ILO, SDMX, R
#' @seealso \code{\link{getCodelist}} \code{\link{getDataStructure}} \code{\link{getData}}
#' @export
#' @import xml2
#' @examples
#' ################################## use to check data available
#'
#' # example with attribute
#' res <- getCount("YI_AFG_EMP_TEMP_SEX_AGE_NB/....")
#'
#' # example without attribute
#' res <- getCount("YI_AFG_ALL/.....?detail=dataonly")
#'
#' # example of last N data
#' res <- getCount("YI_AFG_EMP_TEMP_SEX_AGE_NB/.....?lastNObservations=1")   
#'
#' # example of first N data
#' res <- getCount("YI_AFG_EMP_TEMP_SEX_AGE_NB/.....?firstNObservations=2")
#'
#' # example with multi country
#' res <- getCount("YI_ALL_EMP_TEMP_SEX_AGE_NB/.MEX+ESP")
#'
#' # check availability of time series	
#' res <- getCount("YI_ALL_EMP_TEMP_SEX_AGE_NB/.....?detail=serieskeysonly")
#' ### as from 2009
#' res <- getCount("YI_ALL_EMP_TEMP_SEX_AGE_NB/.....?startPeriod=2009-01-01&detail=serieskeysonly")

getCount <- function(	DSD,								
						test = "-test"){


Detail  <- grep("\\?", DSD)%in%1 ; if(length(Detail)%in%0)	{Detail 	<- FALSE}
	DSD <- 	ifelse(	str_detect(DSD,"[?]"), 
				paste0(DSD, "&format=compact_2_1"), 
				paste0(DSD, "?format=compact_2_1"))
# set if SeriesKeysOnly is requested (NO Obs, No Attrs)
SeriesKeysOnly 	<- grep("DETAIL=SERIESKEYSONLY", toupper(DSD))%in%1 	; if(length(SeriesKeysOnly)%in%0)	{SeriesKeysOnly 	<- FALSE}
# set if DataOnly are requested (No Attrs)
DataOnly 	<- grep("DETAIL=DATAONLY", toupper(DSD))%in%1	; if(length(DataOnly)%in%0){DataOnly 	<- FALSE}


if(Detail & !SeriesKeysOnly & !DataOnly){
	DSD <- paste0(DSD,"&detail=dataonly")
	DataOnly = TRUE 
}

if(!Detail){
	DSD <- paste0(DSD,"?detail=dataonly") 
	DataOnly = TRUE
}

	X <- 	try(
				read_xml(paste0("http://www.ilo.org/ilostat/sdmx",test,"/ws/rest/data/ILO,DF_",DSD)), 
				silent = TRUE)
			
		# test error message
	if(substr(X[1], 1, 5)%in%"Error"){ 
		return(NULL)
	}
	
		# extract namespace of the xml doc
	ns <- xml_ns(X)		
	
		# test dataset exist
	if(length(xml_find_all(X, ".//message:DataSet", ns))==0){ 
		return(NULL)
	}
if(DataOnly){
	length(xml_find_all(X, ".//Obs", ns)) 
} else {
	length(xml_find_all(X, ".//Series", ns)) 
}

}





nths <- function (x, n, order_by = NULL, default = default_missing(x)) 
{

    n <- trunc(n)
    if (n == 0 || n > length(x)) {
        return(default)
    }
    if (is.null(order_by)) {
        x[[n]]
    }
    else {
        x[[order(order_by)[n]]]
    }
}

 
