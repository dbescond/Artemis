shinyUI = function(){dashboardPage(

	dashboardHeader(title = "ILOSTAT sdmx query : "),
	dashboardSidebar(disable = TRUE),
	# dashboardSidebar(
		# sidebarMenu(
  			# menuItem("Select", tabName = "SELECT", icon = icon("th")),
 			# menuItem("Table", tabName = "TABLE", icon = icon("th"))
		# )
	# ),
	dashboardBody(
		tagList(singleton(tags$head(tags$link(href='www/custom.css',rel='stylesheet',type='text/css')))),

		fluidRow( 
			box( title  ="Select frequency", 
				fluidRow(
					column(width = 9,
						radioButtons('Radio_frequency', '', c('Annual','Quarterly','Monthly'), select= 'Annual',  inline = TRUE)
					)
				), 
				collapsible = TRUE, width = 6
			),
			box( title  ="Select collection :", 
				fluidRow(
					column(width = 11, 	offset = 0,
						uiOutput('Collection')
					)		
				),
				collapsible = TRUE, width = 6 
			)
		),
		fluidRow(
			box( title  ="Then select country :", 
				fluidRow(
					column(width = 11, 	offset = 0,
						uiOutput('Country')
					)		
				),
				collapsible = TRUE, width = 6 
			),
			box( title  ="Then select Indicator :", 
				fluidRow(
					column(width = 11, 	offset = 0,
						uiOutput('Indicator')
					)		
				),
				collapsible = TRUE, width = 6 
			)
		),
				fluidRow(
			box( 	title  ="Generate :", 
				fluidRow(
					column(width = 6, 	offset = 0,
						actionButton(inputId = "Button_count", "Count data")
					),
					column(width = 6, 	offset = 0,
						actionButton(inputId = "Button_view", "View data")
					)
				),
				fluidRow(HTML("<BR>")),
				fluidRow(
					column(width = 6, 	offset = 0,
						verbatimTextOutput('Print_count')
					)
				), collapsible = TRUE, width = 6
			), 
			box( 	title  ="Parameters :", 
				fluidRow(
					column(width = 6,
						sliderInput("Slider_Year", "Year range:", 	min=CODE_ORA$CL_COLLECTION %>% select(minYear) %>% t %>% c %>% min, 
														max=CODE_ORA$CL_COLLECTION %>% select(maxYear) %>% t %>% c %>% max , 
														value=c(as.numeric(format(Sys.time(),"%Y"))-11,as.numeric(format(Sys.time(),"%Y"))-1), sep="")
						), 
						column(width = 6,
							HTML('')
						),
						column(width = 6,
						radioButtons('Radio_Data', 'Output :', c('serieskeysonly', 'dataonly','full'), select= 'dataonly',  inline = TRUE)
					)
				), collapsible = TRUE , width = 6
			)
		
		),
		fluidRow(
			uiOutput("table1")
		)
			
	)


	)
}