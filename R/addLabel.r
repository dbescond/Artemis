#' add microdata  label
#'

#' @author ILO bescond  
#' @keywords ILO
#' @export


addLabel <- function(df, Lang = "en", Drop = FALSE){


# create a list to contains codelist
CL <- as.list(colnames(df)[!colnames(df)%in%c("TIME_PERIOD","OBS_VALUE", "VALUE_STATUS")])
names(CL) <- CL
CL <- as_data_frame(CL) 
# adjsut codelist names
CL[1,] <- gsub("CLASSIF_","", CL)
CL[1,] <- gsub("MET_","NOTE_", CL)
CL[1,] <- gsub("OVN_","NOTE_", CL)
# CL[1,] <- gsub("INDICATOR",paste0("INDICATOR_", unique(df$COLLECTION)), CL)

CL <- as.list(CL)

print(paste0(Sys.time() ,": Start loading codelist" ))

		options(warn = -1)
for (i in 1:length(CL)){

	CL[[i]] <- as.tbl(getCodelist(paste0("CL_",CL[[i]]))) %>% filter(code %in% unique(as.character(df[[names(CL)[i]]])))
	CL[[i]] <- eval(parse(text= paste0(" CL[[i]] %>% rename(",names(CL[i])," = code, ",names(CL[i]),"_LABEL = label)")))
}
		options(warn = 0)
print(paste0(Sys.time() ,": Start processing mapping" ))

for (i in 1:length(CL)){

	df <- eval(parse(text= paste0("df %>% mutate(",names(CL[i])," = as.character(",names(CL[i]),"))"))) %>% left_join(select(CL[[i]], -contains("sort"), -contains("description")), by = names(CL[i]))
	
}


df <- df %>% select(contains("COUNTRY"), contains("FREQ"), contains("INDICATOR"), contains("SURVEY"), contains("REPRESENTED_VARIABLE"), contains("CLASSIF_"),  TIME_PERIOD,OBS_VALUE, contains("VALUE_STATUS"), contains("MET_"), contains("OVN_"))		

df <- separate(df, SURVEY_LABEL, c("a", "b", "ke"), sep = " - ", remove = TRUE,
  convert = FALSE, extra = "merge", fill = "right") %>% select(-a, -b) %>% rename(SURVEY_LABEL = ke)

if(!Drop){
	df <- df %>% select(contains("_LABEL"),TIME_PERIOD,OBS_VALUE)		
}
print(paste0(Sys.time() ,": End Processing" ))		

return(df)
}
