#' Query Data ILOSTAT structure
#'
#' Query data from ILOSTAT SDMX API
#'
#' Helper function to efficiently query data from ILOSTAT SDMX API.
#'
#' @param DSD a datastructure definition identified by the triplet \code{[collection; country; indicator]}.
#' @param dimension for scheme "datastructure": the format of dimensions returned. The dimension format "codelist" is compatible with scheme "codelist".
#' @param component default FALSE return just the name else all components.
#' @param lang if component = TRUE return label in es, fr or en .
#' @param test internal ILO test parameter.
#' @author ILO bescond  
#' @keywords ILO, SDMX, R
#' @seealso \code{\link{getData}} \code{\link{getCodelist}}
#' @export
#' @import xml2
#' @importFrom stringr str_split
#' @examples
#' ################################# use to identify 'codelist'		
#' 
#' # fetch structure of codelists in YI collection 
#' res <- getDataStructure(dimension = "codelist", DSD = "YI_ALL_ALL") 	
#'
#' # fetch structure of codelists in YI collection for MEX country
#' res <- getDataStructure(dimension = "codelist", DSD = "YI_MEX_ALL") 	
#'
#' # fetch structure of codelists in YI collection for a specific indicator 
#' res <- getDataStructure(dimension = "codelist", DSD = "YI_ALL_UNE_TUNE_SEX_AGE_NB") 
#'
#' ################################# use to define correct 'filter'
#'
#' # fetch structure of conceptRef in YI collection 
#' res <- getDataStructure(DSD = "YI_ALL_ALL", dimension = "conceptRef") 	
#'
#' res <- getDataStructure(DSD = "YI_ALB_ALL", dimension = "conceptRef")
#'
#' res <- getDataStructure(DSD = "YI_ALL_EMP_TEMP_SEX_AGE_NB", dimension = "conceptRef")
#' 
#' ################################# use to get the all data structure 'filter'
#'
#' res <- getDataStructure(DSD = "YI_FRA_EMP_TEMP_SEX_AGE_NB", component = TRUE)
#' names(res) 
#' ## return a list with : 
#' ## --KeyFamily = df definition of this DSD
#' res$KeyFamily
#' ## --dataStructure = df data structure definition with dimensions and attributes	
#' res$dataStructure
#' ## --codelist = list of codelists df		
#' names(res$codelist) 	; res$codelist$CL_COUNTRY	

getDataStructure <- function(	DSD = "YI_ALB_EAP_TEAP_SEX_AGE_NB", 
								dimension = "conceptRef", 
								component = FALSE ,
								lang ="en",								
								test = NULL){
								
if(!component){		
	path <- paste0("http://www.ilo.org/ilostat/sdmx",test,"/ws/rest/datastructure/ILO/") 
	X <- try(read_xml(paste0(path,DSD), encoding = "UTF-8"), silent = TRUE)

	if(substr(X[1], 1, 5)%in%"Error"){ # error test
		return(NULL)
	}
	ns <- xml_ns(X)[2]		# extract namespace of the xml doc

	if(length(xml_find_all(X, ".//str:KeyFamily", ns))==0){ # empty test 
		return(NULL)
	}
	return(xml_attr(xml_find_all(X, ".//str:Dimension", ns),dimension))
}
else{
	if(toupper(substr(DSD,nchar(DSD)-3, nchar(DSD)))%in% ".XML"){
		path = DSD} else{
		path <- paste0("http://www.ilo.org/ilostat/sdmx",test,"/ws/rest/datastructure/ILO/",DSD,"?references=children")
	}
	X <- try(read_xml(path), silent = TRUE)
	if(substr(X[1], 1, 5)%in%"Error"){ 
		return(NULL)
	}
	ns <- xml_ns(X)				# extract namespace of the xml doc
	if(length(xml_find_all(X, ".//mes:KeyFamilies", ns))==0){ 
		return(NULL)
	}

	DSD <- list( 
		KeyFamily = 
			xml_find_all(X, ".//str:KeyFamily", ns) %>% 
				ldply( 
					function(y){
						Name 	<- cbind(ldply(xml_attrs(xml_find_all(y,".//str:Name",ns))) , ldply(xml_text(xml_find_all(y,".//str:Name",ns))))
						res 	<- c(	KeyFamilyRef = xml_attr(y,"id"), 
										KeyFamilyRefName = ifelse(empty(Name[Name$lang%in%lang,]), NA, Name[Name$lang%in%lang,"V1"])
									) %>% t 
						Encoding(res[2]) <- "UTF-8"
						res
					} 
				) %>% 
				as.tbl %>% 
				mutate_each(funs(as.character), everything())
		,
		dataStructure = 
			bind_rows(
				xml_attrs(xml_find_all(X, ".//str:Dimension", ns)) 	%>% ldply(rbind) %>% as.tbl %>% filter(!codelist %in% NA) %>% select(codelist, conceptSchemeRef, conceptRef) %>% mutate(Components = "Dimension")  %>% 
				mutate_each(funs(as.character), everything())
				,
				xml_attrs(xml_find_all(X, ".//str:Attribute", ns)) 	%>% ldply(rbind) %>% as.tbl %>% filter(!codelist %in% NA) %>% select(codelist, conceptSchemeRef, conceptRef) %>% mutate(Components = "Attribute") %>% 
				mutate_each(funs(as.character), everything()) 
			) %>% 
			rename(codelistRef = codelist) %>% 
			left_join(
				xml_find_all(X, ".//str:Concept", ns) %>%
					ldply( 
						function(y){
							Name 	<- 	cbind(ldply(xml_attrs(xml_find_all(y,".//str:Name",ns))) , ldply(xml_text(xml_find_all(y,".//str:Name",ns))))
							res 	<-	c(	conceptRef = xml_attr(y,"id"), 
											conceptName = ifelse(empty(Name[Name$lang%in%lang,]), NA, Name[Name$lang%in%lang,"V1"])
										) %>% t 
							Encoding(res[2]) <- "UTF-8"
							res			
						} 
					) %>% 
				as.tbl %>% 
				mutate_each(funs(as.character), everything())
			, by = "conceptRef"
			) %>%
			left_join(	
				xml_find_all(X, ".//str:ConceptScheme", ns) %>%
					ldply( 
						function(y){
							Name 	<- 	cbind(ldply(xml_attrs(xml_find_all(y,".//str:Name",ns))) , ldply(xml_text(xml_find_all(y,".//str:Name",ns))))
							res 	<-	c(	conceptSchemeRef = xml_attr(y,"id"), 
											ConceptSchemeName = ifelse(empty(Name[Name$lang%in%lang,]), NA, Name[Name$lang%in%lang,"V1"])
										) %>% t 
							Encoding(res[2]) <- "UTF-8"
							res
						} 
					) %>% 
					as.tbl %>% 
					mutate_each(funs(as.character), everything())
			, by = "conceptSchemeRef"
			) %>%
			left_join(	
				xml_find_all(X, ".//str:CodeList", ns) %>%
					ldply( 
						function(y){
							Name 	<- 	cbind(ldply(xml_attrs(xml_find_all(y,".//str:Name",ns))) , ldply(xml_text(xml_find_all(y,".//str:Name",ns))))
							res 	<-	c(	codelistRef = xml_attr(y,"id"), 
											codelistName = ifelse(empty(Name[Name$lang%in%lang,]), NA, Name[Name$lang%in%lang,"V1"])
										) %>% t 
							Encoding(res[2]) <- "UTF-8"
							res
						} 
					) %>% 
					as.tbl %>% 
					mutate_each(funs(as.character), everything())
			, by = "codelistRef"
		),
		codelist = 
			xml_find_all(X, ".//str:CodeList", ns) %>%
				llply(
					function(z){
						xml_find_all(z,".//str:Code", ns)%>%
						ldply( 
							function(y){
								res 	<-	c(	code 	= xml_attr(y,"value") , 
												label 	= ifelse(length(xml_text(xml_find_all(y,".//str:Description",ns)[xml_attr(xml_find_all(y,".//str:Description",ns),"lang")%in%lang]))>0,
																	xml_text(xml_find_all(y,".//str:Description",ns)[xml_attr(xml_find_all(y,".//str:Description",ns),"lang")%in%lang]) ,
																	NA),
												sort 	= ifelse(length(xml_find_all(y,".//str:Annotations",ns))>0, 
																	xml_text(xml_children(xml_contents(xml_find_all(y,".//str:Annotations",ns))[2])[2]), 
																	as.numeric(NA))
											) %>% t 
								Encoding(res[2]) <- "UTF-8"
								res
							} 
						) %>% 
						as.tbl %>% 
						mutate(	code = as.character(code), 
								label = as.character(label),
								sort = as.numeric(as.character(sort))
						) %>%	
						arrange(sort, label, code) %>%
						filter(!substr(code,nchar(code), nchar(code)) %in% "@")
					}
				)
	)
	names(DSD$codelist) <- xml_attr(xml_find_all(X, ".//str:CodeList", ns), "id")

	DSD$codelist$CL_COLLECTION <- DSD$codelist$CL_COLLECTION %>% filter(code %in% str_split(DSD$KeyFamily$KeyFamilyRef, "_",3)[[1]][1])
	
	if(!str_split(DSD$KeyFamily$KeyFamilyRef, "_",3)[[1]][2] %in% "ALL"){
	DSD$codelist$CL_COUNTRY <- DSD$codelist$CL_COUNTRY %>% filter(code %in% str_split(DSD$KeyFamily$KeyFamilyRef, "_",3)[[1]][2])
	}
	if(!str_split(DSD$KeyFamily$KeyFamilyRef, "_",3)[[1]][3] %in% "ALL"){
	DSD$codelist[[paste0("CL_INDICATOR_",str_split(DSD$KeyFamily$KeyFamilyRef, "_",3)[[1]][1])]] <- DSD$codelist[[paste0("CL_INDICATOR_",str_split(DSD$KeyFamily$KeyFamilyRef, "_",3)[[1]][1])]] %>% filter(code %in% paste0(str_split(DSD$KeyFamily$KeyFamilyRef, "_",3)[[1]][3], collapse="_"))
	}
	return(DSD)
}

}

#' @export

getDataStructureV2 <- function(	DSD = "YI_FRA_EMP_TEMP_SEX_AGE_NB", 
								dimension = "conceptRef", 
								component = FALSE ,
								lang ="en",								
								test = "-test"){
								
if(!component){		
	path <- paste0("http://www.ilo.org/ilostat/sdmx",test,"/ws/rest/datastructure/ILO/") 
	X <- try(read_xml(paste0(path,DSD), encoding = "UTF-8"), silent = TRUE)

	if(substr(X[1], 1, 5)%in%"Error"){ # error test
		return(NULL)
	}
	ns <- xml_ns(X)[2]		# extract namespace of the xml doc

	if(length(xml_find_all(X, ".//str:DataStructures", ns))==0){ # empty test 
		return(NULL)
	}
	return(xml_attr(xml_find_all(X, ".//str:DataStructures", ns),dimension))
}
else{
	if(toupper(substr(DSD,nchar(DSD)-3, nchar(DSD)))%in% ".XML"){
		path = DSD} 
	else{
		path <- paste0("http://www.ilo.org/ilostat/sdmx",test,"/ws/rest/datastructure/ILO/",DSD,"?references=children&format=generic_2_1")
	}
	X <- try(read_xml(path), silent = TRUE)
	if(substr(X[1], 1, 5)%in%"Error"){ 
		return(NULL)
	}
	ns <- xml_ns(X)				# extract namespace of the xml doc
	if(length(xml_find_all(X, ".//str:DataStructures", ns))==0){ 
		return(NULL)
	}

	DSD <- list( 
		KeyFamily = 
			xml_find_all(X, ".//str:DataStructure ", ns) %>% 
				ldply( 
					function(y){
						Name 	<- cbind(ldply(xml_attrs(xml_find_all(y,".//str:Name",ns))) , ldply(xml_text(xml_find_all(y,".//str:Name",ns))))
						res 	<- c(	KeyFamilyRef = xml_attr(y,"id"), 
										KeyFamilyRefName = ifelse(empty(Name[Name$lang%in%lang,]), NA, Name[Name$lang%in%lang,"V1"])
									) %>% t 
						Encoding(res[2]) <- "UTF-8"
						res
					} 
				) %>% 
				as.tbl %>% 
				mutate_each(funs(as.character), everything())
		,
		dataStructure = 
			bind_rows(
			
				xml_find_all(X, ".//str:Dimension", ns) 	%>% 
						ldply(function(y){
								c(	codelistRef 			= xml_attr(xml_children(xml_find_all(y,".//str:Enumeration", ns)), "id") , 
									conceptRef 			= xml_attr(xml_children(xml_find_all(y,".//str:ConceptIdentity", ns)), "id"),
									conceptSchemeRef 	= xml_attr(xml_children(xml_find_all(y,".//str:ConceptIdentity", ns)), "maintainableParentID")
								) %>% t 
							}
						)%>% 
						as.tbl %>% 
						mutate_each(funs(as.character), everything()) %>%
						filter(!codelistRef %in% NA) %>% 
						mutate(Components = "Dimension")
				,
				xml_find_all(X, ".//str:Attribute", ns) 	%>% 
						ldply(function(y){
								c(	codelistRef 			= xml_attr(xml_children(xml_find_all(y,".//str:Enumeration", ns)), "id") , 
									conceptRef 			= xml_attr(xml_children(xml_find_all(y,".//str:ConceptIdentity", ns)), "id"),
									conceptSchemeRef 	= xml_attr(xml_children(xml_find_all(y,".//str:ConceptIdentity", ns)), "maintainableParentID")
								) %>% t 
							}
						)%>% 
						as.tbl %>% 
						mutate_each(funs(as.character), everything()) %>%
						filter(!codelistRef %in% NA) %>% 
						mutate(Components = "Attribute")
			) %>% 
			left_join(
				xml_find_all(X, ".//str:Concept", ns) %>%
					ldply( 
						function(y){
							Name 	<- 	cbind(ldply(xml_attrs(xml_find_all(y,".//com:Name",ns))) , ldply(xml_text(xml_find_all(y,".//com:Name",ns))))
							res 	<-	c(	conceptRef = xml_attr(y,"id"), 
											conceptName = ifelse(empty(Name[Name$lang%in%lang,]), NA, Name[Name$lang%in%lang,"V1"])
										) %>% t 
							Encoding(res[2]) <- "UTF-8"
							res			
						} 
					) %>% 
				as.tbl %>% 
				mutate_each(funs(as.character), everything())
			, by = "conceptRef"
			) %>%
			left_join(	
				xml_find_all(X, ".//str:ConceptScheme", ns) %>%
					ldply( 
						function(y){
							Name 	<- 	cbind(ldply(xml_attrs(xml_find_all(y,".//com:Name",ns))) , ldply(xml_text(xml_find_all(y,".//com:Name",ns))))
							res 	<-	c(	conceptSchemeRef = xml_attr(y,"id"), 
											ConceptSchemeName = ifelse(empty(Name[Name$lang%in%lang,]), NA, Name[Name$lang%in%lang,"V1"])
										) %>% t 
							Encoding(res[2]) <- "UTF-8"
							res
						} 
					) %>% 
					as.tbl %>% 
					mutate_each(funs(as.character), everything())
			, by = "conceptSchemeRef"
			) %>%
			left_join(	
				xml_find_all(X, ".//str:Codelist", ns) %>%
					ldply( 
						function(y){
							Name 	<- 	cbind(ldply(xml_attrs(xml_find_all(y,".//com:Name",ns))) , ldply(xml_text(xml_find_all(y,".//com:Name",ns))))
							res 	<-	c(	codelistRef = xml_attr(y,"id"), 
											codelistName = ifelse(empty(Name[Name$lang%in%lang,]), NA, Name[Name$lang%in%lang,"V1"])
										) %>% t 
							Encoding(res[2]) <- "UTF-8"
							res
						} 
					) %>% 
					as.tbl %>% 
					mutate_each(funs(as.character), everything())
			, by = "codelistRef"
		),
		codelist = 
			xml_find_all(X, ".//str:Codelist", ns) %>%
				llply(
					function(z){
						xml_find_all(z,".//str:Code", ns)%>%
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
				)
	)
	names(DSD$codelist) <- xml_attr(xml_find_all(X, ".//str:Codelist", ns), "id")

	DSD$codelist$CL_COLLECTION <- DSD$codelist$CL_COLLECTION %>% filter(code %in% str_split(DSD$KeyFamily$KeyFamilyRef, "_",3)[[1]][1])
	
	if(!str_split(DSD$KeyFamily$KeyFamilyRef, "_",3)[[1]][2] %in% "ALL"){
	DSD$codelist$CL_COUNTRY <- DSD$codelist$CL_COUNTRY %>% filter(code %in% str_split(DSD$KeyFamily$KeyFamilyRef, "_",3)[[1]][2])
	}
	if(!str_split(DSD$KeyFamily$KeyFamilyRef, "_",3)[[1]][3] %in% "ALL"){
	DSD$codelist[[paste0("CL_INDICATOR_",str_split(DSD$KeyFamily$KeyFamilyRef, "_",3)[[1]][1])]] <- DSD$codelist[[paste0("CL_INDICATOR_",str_split(DSD$KeyFamily$KeyFamilyRef, "_",3)[[1]][1])]] %>% filter(code %in% paste0(str_split(DSD$KeyFamily$KeyFamilyRef, "_",3)[[1]][3], collapse="_"))
	}
	return(DSD)
}

}

#?format=generic_2_1
#?format=generic_2_0
#?format=compact_2_1
#?format=json