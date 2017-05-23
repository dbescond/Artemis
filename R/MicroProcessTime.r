#' process microdata time 
#'
#' Process one time period from microdataset to ilo framework,  
#'
#' Helper function to efficiently process microdataset.
#'
#' @param File, file name(s) of the original microdataset(s), if multiple, ";" separate.
#' @param Frame, ilo collection framework from MicroFrameWork function.
#' @author ILO bescond
#' @keywords ILO, R
#' @seealso \code{\link{MicroFrameWork}} \code{\link{MicrodataLoad}}
#' @export

								
MicroProcessTime <- function(	File, 
								Frame)
{

# File <- File[i, ]

	file_name 		<- 	unlist(stringr::str_split(as.character(File$file_name), ";"))
	X <- NULL
	for (j in 1:length(file_name)){
		X <- X %>% bind_rows(MicrodataLoad(paste0( File$path,"/Input/", file_name[j])) %>% as.tbl %>% cleanDataFrame(tolower = TRUE) %>% mutate(ilo_id = j) 	)
	}

	#options(show.error.messages = FALSE)
	
	# used the Filter if exist
	if(colnames(File)[colnames(File) %in% 'Filter'] %>% length ==1){ if(!File$Filter%in%NA){ 
		try(	X <- eval(parse(text= paste0("  X %>% filter(",paste0(c(unlist(stringr::str_split(as.character(tolower(File$Filter)), ";"))), collapse=", "),")"))))}
	}
	# used the Select if exist
	if( colnames(File)[colnames(File) %in% 'Select'] %>% length ==1 ){if(!File$Select%in%NA){ 
		try(	X 	<- 	eval(parse(text= paste0("  X %>% select(",paste0(c(unlist(stringr::str_split(as.character(tolower(File$Select)), ";"))), collapse=", "),")"))))}
	}		
	
	if( colnames(File)[colnames(File) %in% 'DropFrame'] %>% length ==1 ){if(!File$DropFrame%in%NA){ 
		try(	Frame 	<- 	eval(parse(text= paste0("  Frame %>% filter(!",paste0(c(unlist(stringr::str_split(as.character(File$DropFrame), ";"))), collapse=", "),")"))))}
	}	

	if(colnames(File)[colnames(File) %in% 'Rename'] %>% length ==1 ){ if(!File$Rename%in%NA) {
		try(	X <- eval(parse(text= paste0("  X %>% rename(",tolower(File$Rename),")"))))}
	}
	
	# used the Mutate if exist
	if({colnames(File)[colnames(File) %in% 'Mutate'] %>% length ==1} ){
		if(!File$Mutate%in%NA){
			try(	X 	<- 	eval(parse(text= paste0("  X %>% mutate(",paste0(c(unlist(stringr::str_split(as.character(File$Mutate), ";"))), collapse=", "),")"))))
		}
	}		
	#options(show.error.messages = TRUE)

	Frame %>%	rowwise() %>%
				mutate(Value = eval(parse(text= paste0("X %>% ",cmd )))) %>%
				select( -cmd) %>% 
				mutate(	Collection_Code 	= as.character(File$Collection_Code),
						Time 				= as.character(File$Time), 
						Freq_Code 			= as.character(File$Freq_Code), 
						Value_Status_Code 	= as.character(File$Value_Status_Code))	%>% ungroup
}	 