#' get Microdata framework
#'
#' preape the ILO framework, from the national definition and the ilo targeted indicator,  
#'
#' Helper function to efficiently process microdataset.
#'
#' @param Definition, dataframe mapping between microdataset and ilo framework.
#' @param Indicator_Target, dataframe with ilo indicators target.
#' @author ILO bescond
#' @keywords ILO, R
#' @seealso \code{\link{MicroProcessTime}} \code{\link{MicrodataLoad}}
#' @export

MicroFrameWork <- function(	Definition, 
							Indicator_Target)
{		

	Definition <- Definition %>% 
		mutate_each(funs(as.character),everything() ) %>%
		mutate_each(funs(str_trim),everything() ) %>%
		mutate_each(funs(mapvalues(.,c('NaN', '', 'NA'), c(NA,NA, NA), warn_missing = FALSE)), everything()) %>%
		mutate(	ID = 1:nrow(.), 
				ILO_Note = NA,
				NAT_FILTER = NA, 
				NAT_VAR = NA)
			
	Ind_Weight <- Definition %>% 
		filter(Dimension %in% "SURVEY") %>% 
		select(NAT_WEIGHT) %>% t %>% c %>% tolower
	
	unites <- str_split(Ind_Weight, pattern = " = ")	
	
	Ind_Weight <- unites[[1]][1]
	
	unites <- as.numeric( unites[[1]][2])
						
	VAR <- strsplit(Definition$NAT_VARIABLE, " | ", fixed = TRUE)
	CODE <- strsplit(Definition$NAT_CODE, " | ", fixed = TRUE)
	for (i in 1:nrow(Definition)){

		if (i > 1){
			Definition$ID[i] <- Definition$ID[i-1]+1
			if(Definition$Dimension[i] != Definition$Dimension[i-1]){
				Definition$ID[i] <- 1
			}
		}

		if(!is.na(VAR[[i]][1])){
			ref <- NULL
			vari <- NULL
			for (j in 1:length(VAR[[i]])){
				if(str_detect(CODE[[i]][j], ":")%in% TRUE){
					if(		as.numeric(substr(CODE[[i]][j], str_locate(CODE[[i]][j], ":")[1,1]-1,str_locate(CODE[[i]][j], ":")[1,1]-1)) %in%0:9 & 
							as.numeric(substr(CODE[[i]][j], str_locate(CODE[[i]][j], ":")[1,1]+1,str_locate(CODE[[i]][j], ":")[1,1]+1)) %in%0:9)
					{	pass <- str_split(CODE[[i]][j], pattern = ";") %>% unlist
						for (k in 1:length(pass)){
							pass[k] <- paste0(paste0(eval(parse(text = pass[k]))), collapse = ";")
						}
						CODE[[i]][j] <- paste0(pass, collapse = ";")	
					}
				}
				if(str_detect(CODE[[i]][j], "/")%in% TRUE){
						pass <- str_split(CODE[[i]][j], pattern = ";") %>% unlist
						for (k in 1:length(pass)){
							pass[k] <- paste0("FromTo(",tolower(VAR[[i]][j]),",'",str_split(pass[k], pattern = "/")[[1]][1], "','",str_split(pass[k], pattern = "/")[[1]][2], "')")
						}
						CODE[[i]][j] <- paste0(pass, collapse = ";")	
				}
				ref <- paste0(ref,tolower(VAR[[i]][j])," %in% tolower(as.character(c(",gsub(";",",",CODE[[i]][j], fixed = TRUE),")))", sep = " , ")
				vari <- paste0(vari,tolower(VAR[[i]][j]), sep = " , ")
			}

			if(substr(ref, nchar(ref)-2,nchar(ref)) %in% " , ")	{
				ref <- substr(ref, 1, nchar(ref)-3)
			}
		
			if(substr(vari, nchar(vari)-2,nchar(vari)) %in% " , ")	{
				vari <- substr(vari, 1, nchar(vari)-3)
			}
		
			Definition$NAT_FILTER[i] <- ref
		
			Definition$NAT_VAR[i] <- vari
		
			rm(ref, vari)
		}
	}

	rm(VAR,CODE)

	Type <- strsplit(Definition$Attribute, " | ", fixed = TRUE)
	Note <- strsplit(Definition$Attribute_Code, " | ", fixed = TRUE)						
						
	for (i in 1:nrow(Definition)){						
		if(!is.na(Type[[i]][1])){
			ref <- NULL
			for (j in 1:length(Type[[i]])){
				ref <- paste0(ref,"_",Type[[i]][j],":" ,Note[[i]][j])
			}
			if(substr(ref, 1,1) %in% "_")	{
				ref <- substr(ref, 2, nchar(ref))
			}
			Definition$ILO_Note[i] <- ref
			rm(ref)
		}									
	}
						
	Definition <- Definition %>% 
		as.tbl %>% 
		select(ID, Dimension, Dimension_Code, ILO_Note, NAT_VAR, NAT_FILTER, NAT_WEIGHT) %>%
		mutate(ID = as.character(ID))
						
	Indicator_Target <- Indicator_Target	%>% 
		select(INDICATOR) %>% 
		mutate(	Country_Code = (Definition %>% filter(Dimension %in% "COUNTRY") %>% select(Dimension_Code) %>% t %>% c), 
				Collection_Code = as.character(NA),
				Indicator_Code = INDICATOR,
				Source_Code = (Definition %>% filter(Dimension %in% "SURVEY") %>% select(Dimension_Code) %>% t %>% c), 
				Classif1_Version_Code = as.character(NA), 
				Classif2_Version_Code = as.character(NA),
				Select = Ind_Weight, 
				Ind_Weight = paste0("as.numeric(",Select,")"), 
				Select  = paste0("ilo_id, ", Select),
				Rep_Var_Code = paste0(substr(INDICATOR, 1,9), substr(INDICATOR, nchar(INDICATOR)-1,nchar(INDICATOR))), 
				CLASS = substr(INDICATOR, 10,nchar(INDICATOR)-3)) %>%
		select(-INDICATOR) %>%
		separate(CLASS, c("Sex_Code", "Classif1_Code", "Classif2_Code"), sep = "_", extra = "drop", fill = "right")  %>% 
		as.tbl %>%
		mutate(Notes_Source_Code = Definition %>% filter(Dimension %in% "SURVEY") %>% select(ILO_Note) %>% t %>% c)	


	X <- NULL
	for (i in 1:nrow(Indicator_Target)){
		X 	<- 	X %>% 
			bind_rows(	
				Indicator_Target %>% 	
					slice(i) %>%
						left_join(	
							select(Definition, Rep_Var_Code = Dimension_Code, ILO_Note, NAT_FILTER, NAT_WEIGHT, NAT_VAR), by  ="Rep_Var_Code") %>%
							mutate(	Notes_Indicator_Code = ILO_Note, 
							Filter = NAT_FILTER,
							Select = ifelse(Select %in% NA, NAT_WEIGHT, paste0(Select, ", ", tolower(NAT_WEIGHT))), 
							Select = ifelse(Select %in% NA, NAT_VAR, paste0(Select, ", ", NAT_VAR)), 
							Oth_Weight = ifelse(is.na(NAT_WEIGHT), "1", paste0("as.numeric(",tolower(NAT_WEIGHT),")"))) %>%
							select(-ILO_Note, -NAT_FILTER, -NAT_WEIGHT, -NAT_VAR) %>%
						left_join( 	
							select(Definition, Sex_Code = Dimension, Dimension_Code, ILO_Note, NAT_FILTER, NAT_VAR),by = "Sex_Code") %>% 
							mutate(	Sex_Code = Dimension_Code, 
									Notes_Classif_Code = ILO_Note, 
									Select = ifelse(Select %in% NA, NAT_VAR, paste0(Select, ", ", NAT_VAR)),
									Filter = ifelse(Filter %in% NA, NAT_FILTER, paste0(Filter, ", ", NAT_FILTER))) %>%
							select(-Dimension_Code, -ILO_Note, -NAT_FILTER, -NAT_VAR) %>%	
						left_join( 	
							select(Definition, Classif1_Code = Dimension, Dimension_Code, ILO_Note, NAT_FILTER, NAT_VAR),by = "Classif1_Code") %>% 
							mutate(	Classif1_Code = Dimension_Code, 
									Notes_Classif_Code = ifelse(Notes_Classif_Code%in%NA, ILO_Note, paste0(Notes_Classif_Code, "_", ILO_Note)), 
									Notes_Classif_Code = gsub("_NA", "", Notes_Classif_Code, fixed = TRUE), 
									Select = ifelse(Select %in% NA, NAT_VAR, paste0(Select, ", ", NAT_VAR)),
									Filter = ifelse(Filter %in% NA, NAT_FILTER, paste0(Filter, ", ", NAT_FILTER))) %>%
							select(-Dimension_Code, -ILO_Note, -NAT_FILTER, -NAT_VAR) %>%
						left_join( 	
							select(Definition, Classif2_Code = Dimension, Dimension_Code, ILO_Note, NAT_FILTER, NAT_VAR),by = "Classif2_Code") %>% 
							mutate(	Classif2_Code = Dimension_Code, 
									Notes_Classif_Code = ifelse(Notes_Classif_Code%in%NA, ILO_Note, paste0(Notes_Classif_Code, "_", ILO_Note)), 
									Notes_Classif_Code = gsub("_NA", "", Notes_Classif_Code, fixed = TRUE),
									Select = ifelse(Select %in% NA, NAT_VAR, paste0(Select, ", ", NAT_VAR)),
									Filter = ifelse(Filter %in% NA, NAT_FILTER, paste0(Filter, ", ", NAT_FILTER))) %>%
									select(-Dimension_Code, -ILO_Note, -NAT_FILTER, -NAT_VAR)
			) %>% 
			as.tbl %>% 
			select(contains("_Code"), Select, Filter, Ind_Weight, Oth_Weight) %>% 
			mutate(	Filter = gsub(", NA", "", Filter, fixed = TRUE), 
					Select = gsub(", NA", "", Select, fixed = TRUE))
	}

	X %>% 
		mutate(	Freq_Code = as.character(NA), 
				Time = as.character(NA), 
				Value = as.numeric(NA),
				Value_Status_Code = as.character(NA), 
				cmd = 	ifelse(Oth_Weight %in% 1 ,
								paste0(" select(",Select,") %>% filter(",Filter,") %>% mutate(Ind_Weight = ",Ind_Weight,") %>% select(Ind_Weight, ilo_id) %>% group_by(ilo_id) %>% summarise(by_dataset = sum(Ind_Weight, na.rm = TRUE) / ",1000/ifelse(unites%in%NA, 1000, as.numeric(unites)),") %>% ungroup %>% summarise(mean(by_dataset, na.rm=TRUE)) %>% t %>% c"),
								paste0(" select(",Select,") %>% filter(",Filter,") %>% mutate(Ind_Weight = ",Ind_Weight,") %>% mutate(Oth_Weight = ",Ind_Weight," * ",Oth_Weight,") %>% select(Ind_Weight, Oth_Weight, ilo_id) %>% summarise(sum(Oth_Weight) / sum(Ind_Weight) / length(unique(ilo_id))) %>% t %>% c")		
						)
		) %>% 
		
		separate(Classif1_Code, c("top","rep"), sep = "_", remove = FALSE, extra = "drop") %>% 
		mutate(Classif1_Version_Code =  paste0(top, "_", rep)) %>% select(-top, -rep) %>% 
		mutate(Classif1_Version_Code = gsub("NA_NA", NA,Classif1_Version_Code)) %>%
		separate(Classif2_Code, c("top","rep"), sep = "_", remove = FALSE, extra = "drop") %>% 
		mutate(Classif2_Version_Code =  paste0(top, "_", rep))  %>% select(-top, -rep) %>% 
		mutate(Classif2_Version_Code = gsub("NA_NA", NA,Classif2_Version_Code)) %>%
		select(-Select, -Filter, -Ind_Weight, -Oth_Weight, -Rep_Var_Code)
}

#' @export

FromTo <- function(	Column, 
					Start, 
					End)
{
	# select character from a column from alpha order , ie id columns = c(A, B, C, D), Start = B, End = D, return B, C, D
	# manage alph bad order in the start and End
	# manage missing Start as first, missing End as last
	Column <- levels(as.factor(Column)) %>% sort 

	# reorder Start/End
	pass <-  c(Start, End) %>% sort
	Start <- pass[1]
	End <- pass[2]
	
	Start <- Column[substr(Column, 1, nchar(pass[1])) %in% pass[1] ] %>% first
	End <- Column[substr(Column, 1, nchar(pass[2])) %in% pass[2] ] %>% last
	Start <- ifelse(Start %in% NA,Column %>% first, Start )
	End <- ifelse(End %in% NA,Column %>% last, End )
	

	Column[{str_locate(Column, Start) %>%  as.data.frame %>% mutate(n = 1:n()) %>% filter(start %in% 1) %>% select(n) %>% t %>% c}:{str_locate(Column, End) %>%  as.data.frame %>% mutate(n = 1:n()) %>% filter(start %in% 1) %>% select(n) %>% t %>% c}] %>% sort

}
