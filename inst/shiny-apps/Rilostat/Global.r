

library(shiny,quietly=TRUE)
library(shinydashboard,quietly=TRUE)
library(DT,quietly=TRUE)
library(Rilostat,quietly=TRUE)
library(doSNOW,quietly=TRUE)
library(dplyr,quietly=TRUE)
library(tidyr,quietly=TRUE)
library(stringr,quietly=TRUE)

# ref clustare type
# cl_type <- "SOCK" # or "SOCK", "PSOCK", "FORK" == shiny io
cl_core <- max(1, parallel::detectCores() - 1)
# ref codelist to download
ref <- c("CL_COLLECTION","CL_COUNTRY", "CL_FREQ") #, "CL_SOURCE", "CL_REPRESENTED_VARIABLE","CL_SUBJECT", "CL_SURVEY")

# create and register clusters
cl <- parallel::makeCluster(cl_core) ; registerDoSNOW(cl)

# assign codelist on a list CODE_ORA try :: names(CODE_ORA)
CODE_ORA 	<- 	foreach(i=1:length(ref),.inorder=FALSE, .packages = c("Rilostat", "dplyr")) %dopar% 
				eval(parse(text= paste0("getCodelist(codelist = '",ref[i],"')")))			
names(CODE_ORA) <- ref


# create type of collection
CODE_ORA$CL_COLLECTION_TYPE <- data_frame(	code = c('M','D','A'), 
											label = c('Main indicators','Details indicators','Ad hoc indicators'))


ref <- as.numeric(format(Sys.time(),"%Y"))
# init collection
ref <- data_frame(	code = 				c('YI', 'STI', 	'MIG', 	'YTH', 	'EAPEP', 'GWR', 'MPI', 'CPI'), 
					minYear = 			c(1945, 1930, 	1986, 	2000, 	1990, 	1995, 	1945, 	1980), 
					maxYear = 			c(ref, 	ref, 	ref, 	2013, 	2030, 	2013, 	2013, 	ref),
					collection_type = 	c('D', 	'D', 	'A', 	'A', 	'D', 	'D', 	'A', 	'A'), # Main, Details, Adhoc
					Monthly = 			c(0, 	1, 		0, 		0, 		0, 		0, 		0, 		0),
					Quarterly = 		c(0, 	1, 		0, 		0, 		0, 		0, 		0, 		0), 
					Annual = 			c(1, 	0, 		1, 		1, 		1, 		1, 		1, 		1),
					sort_ = 			c(2, 	3, 		6, 		5, 		1, 		4, 		7, 		9)
				) %>% arrange(as.numeric(sort_))
			
			
CODE_ORA$CL_COLLECTION <- CODE_ORA$CL_COLLECTION %>% filter(code %in% ref$code) %>% left_join(ref, by = "code")



ref <- CODE_ORA$CL_COLLECTION %>% 	group_by(collection_type) %>% 
									summarise(	minYear = min(as.numeric(minYear)), 
												maxYear = max(as.numeric(maxYear)),
												collectionCode = paste0(code, collapse = ", ")) %>%
									ungroup() %>% 
									rename(code = collection_type)
										
										
CODE_ORA$CL_COLLECTION_TYPE <- CODE_ORA$CL_COLLECTION_TYPE %>% left_join(ref, by = "code")
												

# ref codelist "Indicatr_collection" to download		
ref	<- 	CODE_ORA$CL_COLLECTION %>% select(code) %>% t %>% c %>% paste0("CL_INDICATOR_", .)


# assign in CL_INDICATOR_PLUS (ie. indicator by collection) on CODE_ORA try :: names(CODE_ORA)
CODE_ORA$CL_INDICATOR_PLUS 		<- 	foreach(i=1:length(ref),.inorder=FALSE, .packages = c("Rilostat", "dplyr")) %dopar% 
									mutate(eval(parse(text= paste0("getCodelist(codelist = '",ref[i],"')"))), collection = gsub("CL_INDICATOR_", "",ref[i])) %>% 
									bind_rows %>% arrange(sort)
									
# close clusters
stopCluster(cl) ; rm(ref, cl); gc()

# Improve codelist
# CODE_ORA$CL_SURVEY 		<-  CODE_ORA$CL_SURVEY %>% separate(label, c("country", "source", "label"), sep=" - ", extra = "merge")

