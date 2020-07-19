savedf<- reactiveValues(fid=NULL, filtab=NULL)
#' @export
#' @title xfr
#' @description currently applied filters
xfr<- reactiveValues(currfilters=NULL)
fobs<- 0

#' @rawNamespace import(shiny, except = c(renderDataTable, dataTableOutput))
#' @import shinydashboard
#' @import shinydashboardPlus
#' @import shinyWidgets
#' @import shinyBS
#' @import DT
#' @import libcubmeta
#' @import libcubolap
#' @import dplyr
#' @export
#' @title addfilter
#' @description Add the selected filter item and values to the filter structure 'currfilters'
#' @param M is the meta data connection structure
#' @param fid item id of the filter
#' @param fval filter values for this item
#' @param fexcl flag to exclude values 
addfilter<- function(M, fid, fval, fexcl) {
	fid<- as.character(fid)
	if(is.null(fval) | length(fval) == 0)
		xfr$currfilters[[fid]]<- NULL
	else {
		xfr$currfilters[[fid]]$fval<- fval
		xfr$currfilters[[fid]]$fexcl<- fexcl
		mdtl<- getdims(M$cfg, fid)[[1]]
		xfr$currfilters[[fid]]$mdtl<- mdtl
		}
	return(xfr$currfilters[[fid]])
	}

getdimtab<- function(M, D, dim) {
	if(dim!='') {
		dim<- getdims(M$cfg, dim)[[1]]
		if(dim$md_table != 'time')
			db<- D$datadb
		else
			db<- M$cfg$configdb
		dxy<- tbl(db, sql(paste('select distinct', dim$md_column, 'from', dim$md_table, 'order by 1 limit 500')))
		dxy<- as.data.frame(dxy)
		dxy[,1]
		}
	}

#' @export
#' @title filters
#' @description facility to create, save and selected saved filters
#' @param input is shiny input variable
#' @param output is shiny output variable
#' @param session is shiny session variable
#' @param M is the meta data connection structure
#' @param D is the data connection structure
filters<- function(input, output, session, M, D) {
	ns<- session$ns

	if(!fobs) {
	observeEvent(input$clearfil, {
		xfr$currfilters<<- NULL
		})

	observeEvent(input$applyfil, {
		if(!is.null(input$id4)) {
			fid<- as.character(lookupid(M, input$id3, 'd')[[1]])
			fil<- addfilter(fid, input$id4, input$fexcl)
			if(!is.null(input$fname))
				savefilter(M$mycfg, fil, input$fname)
			}
		})
	observeEvent(input$savedfils_rows_selected, ignoreNULL=T, ignoreInit=T,{
		rownum<- isolate(input$savedfils_rows_selected)
		fid<- as.character(savedf$fid)
		fexcl<- savedf$filtab[rownum, 2]
		if(is.na(fexcl)) fexcl<- FALSE
		fvals<- savedf$filtab[rownum, 3]
		fvals<- strsplit(fvals, ',')[[1]]
		addfilter(fid, fvals, fexcl)
		})
	fobs<<-1
	}

	output$showfsel<- renderText({
		paste("Filtered", input$id3, ":", paste(input$id4, collapse=", "))
		})
	output$fsel<- renderUI({
		req(input$id3)
		id<- lookupid(M, isolate(input$id3), 'd')
		pickerInput(inputId=ns("id4"), multiple=T,# label=paste('Select', input$id3),
			choices=getdimtab(M, D, id),
			options = pickerOptions(dropupAuto=T, liveSearch=T, size=10, title=paste('Select', input$id3), selectedTextFormat='static')
			)
		})
	output$savedfils<- renderDataTable({
		fid<- lookupid(M, input$id3, 'd')
		q<- paste("select fh_id, fh_name, fh_excl from filter_header where fh_itemid=", fid, "order by fh_name");
		filtab<- as.data.frame(tbl(M$cfg$configdb, sql(q)))

		if(nrow(filtab) > 0) {
			for(i in 1:nrow(filtab)) {
				q<- paste("select fd_value from filter_details where fd_id=", filtab[i,1])
				fval<- as.data.frame(tbl(M$cfg$configdb, sql(q)))
				filtab[i,4]<- paste(fval[,1], collapse=",")
				}
			filtab[,1]<- NULL
			}
		else
			filtab<- data.frame(matrix(nrow=0,ncol=3))
		colnames(filtab)<- c('Filter', 'Exclude', 'Values')
		savedf$fid<- fid
		savedf$filtab<- filtab
		datatable(filtab, selection='single', escape=F, caption=paste('Filters for', input$id3), class='compact', options=list(pageLength=5, dom='tp', language=list(zeroRecords='No Filters Available')))
		})
	}

#' @export
#' @title filtersUI
#' @description UI for facility to create, save and selected saved filters
#' @param id is caller id
#' @param dimgrp is the dimension group applicable for selected measures
#' @param M is the meta data connection structure
filtersUI<- function(id, dimgrp, M) {
	ns<- NS(id)

	flist<- xmakepicklist(M$cfg, dimgrp)
	fluidPage(
#	box(title='Filter On', status='info', width=12, closable=F,
		pickerInput(inputId=ns("id3"), multiple=F, label=NULL, selected='',
		choices=flist,,
		options = pickerOptions(dropupAuto=T, selectedTextFormat='auto', liveSearch=T, size=10)
	#	)
		),
	tags$style(".card .avatar {max-width: 0em; max-height:0em; margin-top:0em;}"),
	tags$style(".card .card-background-1 {height: 0em;}"),
	flipBox(width=12, id=1, main_img='blank.jpg', front_title='Select a Filter', back_title='Create a New Filter', front_btn_text='Create Filter', back_btn_text='Select Filter',
	tags$div(dataTableOutput(ns('savedfils')),br(),br(),br()),
	back_content=
		fluidPage(
		textInput(ns('fname'), label=NULL,  placeholder='Optional Filter Name'),
		fluidRow(
		column(8,
			textOutput(ns('showfsel')),
			checkboxInput(ns('fexcl'), 'Exclude Selections'),
			uiOutput(ns('fsel'))
			),
		column(4, align='right',
			bsButton(ns("applyfil"), label="Create",block=T, icon=icon("filter"), style='info',size='default')
			)
		)
		)
		)
		)
	}
