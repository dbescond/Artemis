#' Query Data ILOSTAT data
#'
#' Query data from ILOSTAT SDMX API
#'
#' Helper function to efficiently query data from ILOSTAT SDMX API.
#'
#' @param DSD a datastructure definition identified by the triplet \code{[collection; country; indicator]}. Arguments description come from 'http://www.ilo.org/ilostat/content/conn/ILOSTATContentServer/path/Contribution Folders/statistics/web_pages/static_pages/technical_page/ilostat_appl/SDMX_User_Guide.pdf' .
#' @param test internal ILO test parameter.
#' @author ILO bescond  
#' @keywords ILO, SDMX, R
#' @seealso \code{\link{getCodelist}} \code{\link{getDataStructure}}
#' @export
#' @import xml2 dplyr
#' @importFrom plyr ldply
#' @importFrom plyr rlply
#' @importFrom plyr llply
#' @importFrom doSNOW registerDoSNOW
#' @examples
#' ################################## use to fetch data and metadata related
#'
#' # example with attribute
#' res <- getData("YI_AFG_EMP_TEMP_SEX_AGE_NB/....")
#'
#' # example without attribute
#' res <- getData("YI_AFG_ALL/......?detail=dataonly")
#'
#' # example of last N data
#' res <- getData("YI_AFG_EMP_TEMP_SEX_AGE_NB/......?lastNObservations=1")   
#'
#' # example of first N data
#' res <- getData("YI_AFG_EMP_TEMP_SEX_AGE_NB/......?firstNObservations=2")
#'
#' # example with multi country
#' res <- getData("YI_ALL_EMP_TEMP_SEX_AGE_NB/.MEX+ESP.....")
#'
#' # check availability of time series	
#' res <- getData("YI_FRA_EMP_TEMP_SEX_AGE_NB/......?detail=serieskeysonly")
#' ### as from 2009
#' res <- getData("YI_ALL_EMP_TEMP_SEX_AGE_NB/......?startPeriod=2009-01-01&detail=serieskeysonly")

getData <- function(	DSD,								
						test = "-test")
{

		# add attribute format compact
	DSD <- 	ifelse(	str_detect(DSD,"[?]"), 
				paste0(DSD, "&format=compact_2_1"), 
				paste0(DSD, "?format=compact_2_1"))

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

		# return SeriesKey only
	if(str_detect(DSD,"detail=serieskeysonly")){ 
		xml_attrs(xml_find_all(X, ".//Series", ns)) %>% lapply(as.list)  %>% bind_rows  %>%
			return(.)
	} else {
		# return all 
		llply(xml_find_all(X, ".//Series", ns), 
			function(y){ 
				left_join(
					xml_attrs(y) %>% as.list %>% as_data_frame %>% mutate(id = 1),
					xml_attrs(xml_find_all(y, ".//Obs", ns)) %>% lapply(as.list) %>% bind_rows %>% mutate(id = 1)  , 
					by = "id"
				) %>% select(COLLECTION, COUNTRY, FREQ, SURVEY, REPRESENTED_VARIABLE, contains("CLASSIF_"), contains("TIME_PERIOD"), contains("OBS_VALUE"), contains("VALUE_STATUS"),starts_with("MET_"), starts_with("OVN_"))
			}
		) %>% 
		bind_rows %>%
		mutate_each(funs(as.character), everything())%>% 
		mutate(Value = as.numeric(OBS_VALUE)) %>% 
		select(COLLECTION, COUNTRY, FREQ, SURVEY, REPRESENTED_VARIABLE, contains("CLASSIF_"), contains("TIME_PERIOD"), contains("OBS_VALUE"), contains("VALUE_STATUS"),starts_with("MET_"), starts_with("OVN_")) %>% 
		return(.)
	}
}

