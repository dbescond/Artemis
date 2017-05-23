shinyServer(function(input, output, session) {
############################ init session

gc()
cl <- parallel::makeCluster(cl_core) ; registerDoSNOW(cl)


TimeInput  		<- 	function(){
						switch(input$Radio_frequency, 	"Annual"= c(CODE_ORA$CL_COLLECTION %>% filter(Annual ==1) 	%>% select(minYear) %>% t %>% c %>% min, CODE_ORA$CL_COLLECTION %>% filter(Annual ==1) 		%>% select(maxYear) %>% t %>% c %>% max),	
														"Quarterly"= c(CODE_ORA$CL_COLLECTION %>% filter(Quarterly ==1)	%>% select(minYear) %>% t %>% c %>% min, CODE_ORA$CL_COLLECTION %>% filter(Quarterly ==1) 	%>% select(maxYear) %>% t %>% c %>% max), 
														"Monthly"=  c(CODE_ORA$CL_COLLECTION %>% filter(Monthly ==1) 	%>% select(minYear) %>% t %>% c %>% min, CODE_ORA$CL_COLLECTION %>% filter(Monthly ==1) 	%>% select(maxYear) %>% t %>% c %>% max))						
					}

					
					
ls_country 		<- 	function(){
						if(is.null(input$Country_Code)){
							return(NULL)
						}
						CODE_ORA$CL_COUNTRY %>% filter(label %in%input$Country_Code) %>% select(code) %>% t %>% c
					}
				
ls_indicator 	<- 	function(){					 
						if(is.null(input$Indicator_Code) & is.null(input$Collection_Code)) {
							return(NULL)} else {x <- CODE_ORA$CL_INDICATOR_PLUS}					
						if(!is.null(input$Collection_Code)){
							ref <- CODE_ORA$CL_COLLECTION %>% filter(label %in% input$Collection_Code) %>% select(code) %>% t %>% c
							x <- x %>% filter( collection %in% ref)}
						if(!is.null(input$Indicator_Code)){
							x <- x %>% filter( label %in% input$Indicator_Code)}
						x
					}

ls_collection 	<- 	function(){	
						if(is.null(input$Collection_Code)){
							return(NULL)
						}
						CODE_ORA$CL_COLLECTION %>% filter(label %in%input$Collection_Code) %>% select(code) %>% t %>% c					
					}
					

output$Country 		<- 	renderUI({
						selectizeInput(inputId = 'Country_Code'	,NULL , 	CODE_ORA$CL_COUNTRY %>% select(label) %>% t %>% c , multiple=TRUE, options = list(plugins = list('remove_button'), placeholder = "by_name:"))
					})		
output$Collection <- 	renderUI({
						selectizeInput(inputId = 'Collection_Code'	,NULL , 	CODE_ORA$CL_COLLECTION %>% select(label) %>% t %>% c, multiple=TRUE, options = list(plugins = list('remove_button'), placeholder = "by_collection:"))
					})
output$Indicator <- 	renderUI({
						selectizeInput(inputId = 'Indicator_Code'	,NULL , 	CODE_ORA$CL_INDICATOR_PLUS %>% select(label) %>% distinct %>% t %>% c, multiple=TRUE, options = list(plugins = list('remove_button'), placeholder = "by_name:"))
					})	

test_selection 	<- 	function(){
						if(	is.null(input$Country_Code) | (is.null(input$Collection_Code) & is.null(input$Indicator_Code))){
							return(FALSE)
						}else
							(return(TRUE)
						)
					}
df_dsd				<-	function(){
						freq <- switch(input$Radio_frequency, "Annual" = "A",  			"Quarterly" = "Q", 			"Monthly" = "M")
						ind_ <- ls_indicator()  
						cou_ <- ls_country()
						para_ <- paste0("startPeriod=",as.numeric(input$Slider_Year[1]),"-01-01&endPeriod=",as.numeric(input$Slider_Year[2]),"-12-31", "&detail=",as.character(input$Radio_Data))
						
						#dsd <- foreach(i=1:length(cou_)) %do% mutate(ind_,id = paste0(collection, "_", cou_[i], "_",  code, "/..",freq,"..."))
						dsd <- mutate(ind_,id = paste0(collection, "_", "ALL", "_",  code, "/.",paste0(cou_, collapse="+"),".",freq,"...?", para_))
						
						dsd %>% bind_rows %>% distinct(id) %>% select(id) %>% t %>% c
					}					
					
df_count 		<- 	function(){

						if(!test_selection()){
								return("No Variable selected, Plse select at least one country and one indicator then click on Count or View!")
							}
						Dsd_vector <- df_dsd()
						#count_cluster <- parallel::makeCluster(cl_core, timeout = 100000) ; registerDoSNOW(count_cluster) # registerDoSNOW(cl)
						x <- foreach(i=1:length(Dsd_vector),.inorder=FALSE, .packages = c("Rilostat")) %dopar% getCount(Dsd_vector[i]) %>%  unlist %>% c 
						#stopCluster(count_cluster); rm(count_cluster)
						if(is.null(x)){
							return("No ")
						} else { 
							x %>% sum(., na.rm = TRUE)
						}
					}

					
					
df_data				<-	function(){	
							if(!test_selection()){
								return(data_frame("Status:" = "No Variable selected", "Suggest:" = "Plse select at least one country and one indicator then click on Count or View!"))
							}
							Dsd_vector <- df_dsd() # paste0(df_dsd(), "?detail=dataonly")
							x <- foreach(i=1:length(Dsd_vector),.inorder=FALSE, .packages = c("Rilostat")) %dopar% getData(Dsd_vector[i]) %>% bind_rows 
							if (nrow(x)==0){
								data_frame("Status:" = "No Data found", "Suggest:" = "Plse change selection then test by using 'Count' button")
							} else {	
								x <- x %>% 	mutate_each(funs(factor), everything())  %>%
								select(COLLECTION, COUNTRY, FREQ, SURVEY, REPRESENTED_VARIABLE, contains("CLASSIF_"),  contains("TIME_PERIOD"), contains("OBS_VALUE"), contains("VALUE_STATUS"),contains("MET_"), contains("OVN_"))
								if(!str_detect(Dsd_vector[1], "serieskeysonly")){
									x <- x %>% mutate(OBS_VALUE = round(as.numeric(as.character(OBS_VALUE)),3))
								}
							} 
							x
						}


observeEvent(	input$Button_view, {
						X <<- df_data()
		})		
						
observe({				input$Radio_frequency
							isolate(		
								updateSliderInput(session , inputId = "Slider_Year", 	label   = "Year range:", min=TimeInput()[1], max=TimeInput()[2], value=c(as.numeric(format(Sys.time(),"%Y"))-11,as.numeric(format(Sys.time(),"%Y"))-1))
							)		
		})

						
output$Print_count 	<- 	renderPrint({
							timerer <- Sys.time()
							if(input$Button_count!=0){	
									isolate(			 
										cat(paste0(df_count(),  " data (",round(difftime(Sys.time(),timerer, units = "secs" ), 0)," secs)"))								
									) 
							}
					}) 
					
output$table1 	<- 	renderUI({	
						if(input$Button_view!=0){
							box( title  ="View Data",
								div(style = 'overflow-x: scroll', dataTableOutput('View'))
							,width = 12 
							)
						}
					})
					
output$View 		<- 	renderDataTable({
							input$Button_view
							X
							#if(input$Button_view!=0){	
							#	isolate(
							#		# options(show.error.messages = FALSE)
							#		df_data()
							#	)
							# }
							
							
							
						},							
						rownames = TRUE, 
						filter = 'top',		
						selection = 'single',
						extensions = c('ColReorder','ColVis','TableTools'), 
						options = 	list(
										stateSave = TRUE,
										dom = "<'top'>TRClfrip<'clear'>t", 
										searchHighlight = TRUE,
										pageLength = 15, 
										autoWidth = TRUE,
										tableTools =list(
														sSwfPath = copySWF('www'),
														aButtons = 	list('copy', 'print', 
																		list(
																			sExtends = 'collection',
																			sButtonText = 'Save',
																			aButtons = c('csv', 'xls')
																		)
																	)
														),
										lengthChange =TRUE
									), 
						class = c('compact', 'cell-border'),
						server = TRUE,
						escape = TRUE
					)
})