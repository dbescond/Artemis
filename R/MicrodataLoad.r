#' load microdataset
#'
#' load microdata in various formats .dbf, .dta (13 and before),  
#'
#' Helper function to efficiently load microdataset.
#'
#' @param path, path and filenames of the original microdataset.
#' @param resume, if true, return list of colnames and label ($Label), colnames and vector class ($Class), colnames and summary for numeric and table for other ($Summary), instead of the full dataset.
#' @author ILO bescond
#' @keywords ILO, R
#' @seealso \code{\link{MicroFrameWork}} \code{\link{MicroProcessTime}}
#' @import foreign haven readr
#' @export
#' @examples
#' MicrodataLoad("H:\\COMMON\\A0 Short term indicators\\Collection\\COU_ZAF_TEST\\Input\\qlfs-2009-03.dta", TRUE) %>% filter(!value %in% c('UQNO', 'Weight')) %>% unnest %>% ilo::save_ilo(. = 'my_summary')



MicrodataLoad <- function(	path, 
							resume = FALSE)
{

X <- NULL
		# file resave in rds
	if(file.exists(paste0(path,".rds"))){
		X <- readRDS(paste0(path,".rds"))
	}
		# Stata
	if(unique(substr(path,nchar(path)-3, nchar(path)))%in%".dta" & is.null(X) & file.exists(path)){ 
		X <- haven::read_dta(path)
	}
		# dbf
	if(unique(substr(path,nchar(path)-3, nchar(path)))%in%".dbf" & is.null(X) & file.exists(path)){ 
		X <- foreign::read.dbf(path)
	}
		# spss (sav)
	if(unique(substr(path,nchar(path)-3, nchar(path)))%in%".sav" &is.null(X) & file.exists(path)){ 
		X <- haven::read_spss(path)	 	
	}
		# sas7bdat
	if(unique(substr(path,nchar(path)-8, nchar(path)))%in%c(".sas7bdat") & is.null(X) & file.exists(path)){ 
		X <- haven::read_sas(path)	 	
	}
	if(resume){
		res <- X %>% 
				ldply(., function(x) if(is.null(attributes(x))) c(class(x), as.character(NA)) else c(class(x),ifelse(is.null(attributes(x)$label), as.character(NA), attributes(x)$label))) %>% 
				as.tbl %>% 
				rename(value = .id, class = V1, label = V2)%>% 
				mutate(values = NA)
		try(Encoding(res$label) <- 'latin1', silent = TRUE) 
		try(res$label <- iconv(res$label, "latin1", "UTF-8"), silent = TRUE) 
		for (i in 1:nrow(res)) {
			res[i,'values'] <- 
				ifelse(res[i,'class'] %>% t %>% as.character %in% 'labelled' ,
					checklabelled(X %>% select_(res[i,'value'] %>% t %>% as.character)),
					checkother(X %>% select_(res[i,'value'] %>% t %>% as.character))
				)	
		}
		invisible(gc(reset = TRUE))	
		return(res %>% mutate_each(funs(as.character), -values) )
	}
invisible(gc(reset = TRUE))		
	
return(X)	
	
}




checkother 	<- function(x) {
			x %>% group_by_(names(x)) %>% summarise(id = 1, labels = NA, count = n()) %>% mutate_(values = as.character(.[1])) %>% mutate(values = as.character(values))%>% select(id, values,labels, count) %>% mutate(percent = round(count / sum(count, na.rm = TRUE) * 100, 1)) %>% nest(-id) %>% select(-id) 
} 
 

checklabelled 	<- function(x) {
			b <- x %>% group_by_(names(x)) %>% summarise(count = n()) %>% mutate_(values = as.character(.[1])) %>% mutate( values = as.character(values)) %>% select(values, count) 
			x <- x %>% distinct %>% .[[names(x)]]
			a <- cbind(	id = 1, 
						values =  attributes(x)$labels, 
						labels = attributes(x)$labels %>% names)
			rm(x)			
			row.names(a) = NULL 
			a <- as.tbl(as.data.frame(a)) %>% 
					mutate(values = as.character(values), labels = as.character(labels)) %>% 
					left_join(b, by  = 'values') %>% 
					mutate(percent = round(count / sum(count, na.rm = TRUE) * 100, 1)) 
			try(Encoding(a$labels) <- 'latin1', silent = TRUE) 	
			try(a$labels <- iconv(a$labels, "latin1", "UTF-8"), silent = TRUE) 	
					 
			a %>% 	nest(-id) %>% 
					select(-id)
} 
 

 