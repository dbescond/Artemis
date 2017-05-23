#' Query Data ILOSTAT codelist
#'
#' Query data from ILOSTAT SDMX API
#'
#' Helper function to efficiently query data from ILOSTAT SDMX API.
#'
#' @param codelist a codelist as returned by \code{scheme="datastructure", dimension="codelist"}.
#' @param lang return label in es, fr or en .
#' @param name return just codelist name default FALSE.
#' @param test internal ILO test parameter.
#' @author ILO bescond
#' @keywords ILO, SDMX, R
#' @seealso \code{\link{getData}} \code{\link{getDataStructure}}
#' @export
#' @import xml2 dplyr
#' @importFrom plyr ldply
#' @importFrom stringr str_split
#' @examples
#' ################################# use to identify correct 'DSD'		 
#'
#' # fetch collection define on ILOSTAT
#' res <- getCodelist(codelist = "CL_COLLECTION", lang ="en")	
#'						
#' # fetch country available on ILOSTAT
#' res <- getCodelist(codelist = "CL_COUNTRY", lang ="es")							
#'
#' # fetch indicator define in YI collection (code, Label in french), also available en (English), es (Spanish)
#' res <- getCodelist(codelist = "CL_INDICATOR_YI", lang ="fr") 
	

getCodelist	<- function(	codelist, 
								lang 	= "en", 
								name 	= FALSE, 
								test	= "-test")
{

X <- try(read_xml(paste0("http://www.ilo.org/ilostat/sdmx",test,"/ws/rest/codelist/ILO/",codelist,"?format=generic_2_1"), encoding = "UTF-8"), silent = TRUE)

if(substr(X[1], 1, 5)%in%"Error"){ # error test
	return(NULL)
}
ns <- xml_ns(X)	# extract namespace of the xml doc

if(length(xml_find_all(X, ".//str:Codelists", ns))==0){ # empty test 
	return(NULL)
}
if(name == TRUE)	{ # return only the name of the codelist 
	return( xml_text(xml_find_all(X,".//com:Name", ns)[1:3])[xml_attr(xml_find_all(X,".//com:Name", ns)[1:3],"lang")%in%lang])
}

xml_find_all(X,".//str:Code", ns) %>%
						ldply( 
							function(y){
								res 	<-	c(	code 	= xml_attr(y,"id") , 
												label 	= ifelse(	length(xml_text(xml_find_all(y,".//com:Name",ns)[xml_attr(xml_find_all(y,".//com:Name",ns),"lang")%in%lang]))>0,
																		xml_text(xml_find_all(y,".//com:Name",ns)[xml_attr(xml_find_all(y,".//com:Name",ns),"lang")%in%lang]) ,
																		NA),
												sort 	=   ifelse(	length(xml_find_all(y,".//com:Annotation",ns)[xml_text(xml_find_all(y,".//com:AnnotationTitle",ns)) %in% "Sort"])>0,
																		if(length(xml_attr(xml_find_all(xml_find_all(y,".//com:Annotation",ns)[xml_text(xml_find_all(y,".//com:AnnotationTitle",ns)) %in% "Sort"],".//com:AnnotationText",ns), "lang"))==1){
																			xml_text(xml_find_all(xml_find_all(y,".//com:Annotation",ns)[xml_text(xml_find_all(y,".//com:AnnotationTitle",ns)) %in% "Sort"],".//com:AnnotationText",ns))
																		} else {
																			xml_text(xml_find_all(xml_find_all(y,".//com:Annotation",ns)[xml_text(xml_find_all(y,".//com:AnnotationTitle",ns)) %in% "Sort"],".//com:AnnotationText",ns)[xml_attr(xml_find_all(xml_find_all(y,".//com:Annotation",ns)[xml_text(xml_find_all(y,".//com:AnnotationTitle",ns)) %in% "Sort"],".//com:AnnotationText",ns),"lang")%in%lang])
																		},
																		NA),
												default 	=   ifelse(	length(xml_find_all(y,".//com:Annotation",ns)[xml_text(xml_find_all(y,".//com:AnnotationTitle",ns)) %in% "Default"])>0,
																		if(length(xml_attr(xml_find_all(xml_find_all(y,".//com:Annotation",ns)[xml_text(xml_find_all(y,".//com:AnnotationTitle",ns)) %in% "Default"],".//com:AnnotationText",ns), "lang"))==1){
																			xml_text(xml_find_all(xml_find_all(y,".//com:Annotation",ns)[xml_text(xml_find_all(y,".//com:AnnotationTitle",ns)) %in% "Default"],".//com:AnnotationText",ns))
																		} else {
																			xml_text(xml_find_all(xml_find_all(y,".//com:Annotation",ns)[xml_text(xml_find_all(y,".//com:AnnotationTitle",ns)) %in% "Default"],".//com:AnnotationText",ns)[xml_attr(xml_find_all(xml_find_all(y,".//com:Annotation",ns)[xml_text(xml_find_all(y,".//com:AnnotationTitle",ns)) %in% "Default"],".//com:AnnotationText",ns),"lang")%in%lang])
																		},
																		NA),
												description	= ifelse(	length(xml_text(xml_find_all(y,".//com:Description",ns)[xml_attr(xml_find_all(y,".//com:Description",ns),"lang")%in%lang]))>0,
																		xml_text(xml_find_all(y,".//com:Description",ns)[xml_attr(xml_find_all(y,".//com:Description",ns),"lang")%in%lang]) ,
																		NA)

											) %>% t 
								Encoding(res[2]) <- "UTF-8"
								Encoding(res[5]) <- "UTF-8"
								res
							} 
						) %>% 
						as.tbl %>% 
						mutate(	code = as.character(code), 
								label = as.character(label), 
								default = as.character(default), 
								description = as.character(description), 
								sort = as.numeric(as.character(sort))
						) %>%	
						arrange(sort,  label, code) %>%
						filter(!substr(code,nchar(code), nchar(code)) %in% "@")
}