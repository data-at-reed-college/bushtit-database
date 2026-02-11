library(tidyr); library(dplyr)
library(shiny); library(htmltools); library(DT); library(bslib) # last two libs might not be necessary

ui = page_navbar(
  title = "Bushtit Database Entry",
  sidebar = sidebar(
    textOutput("testing_submit_form_update_nest"),
    tags$script(
      paste(system("cat get_location.js", intern = TRUE), collapse="\n")) # query location
  ),
  nav_panel(
    title = "Nest Update",
    textInput("observer", "Observer", value = ""), # could be a choice if there are a discrete number of people... would also require more maintenance
    # No need to query time, R can do that on submission
    # Working on GPS coordinates. Ideally we shouldn't need users to enter them
    # Date comes free with R
    selectInput("nest_state", "Nest State", 
      list("Select a value" = -1, # default value
	   "Stage 1: Base of material present, with little shape visible" = 1,
	   "Stage 2: I don't know what the description is" = 2,
  	   "Stage 3: Nest complete, but with no sign of brooding or feeding" = 3,
	   "Stage 4+: We can add more" = 4)
    ),
    selectInput("parents", "Parents",
		  list(
		    "Select a value" = -1, # default value
		    "0 Parents" = 0,
		    "1 Parent" = 1,
		    "2 Parents" = 2,
		    "3+ Parents?" = 3)
    ),
    textInput("tags", "Tags?", "None"), # I don't know what type of data tags are. This may be better suited to a selectInput()
    selectInput("bag_construction", "Bag Construction Types", 
		  list(
		    "Select a value" = -1, # default value
		    "The first type" = 1,
		    "The second type?" = 2)
    ),
    selectInput("predation", "Signs of Predation?", 
		  list(
		    "Select a value" = -1, # default value. Not Working???
		    "none" = 0,
		    "some?" = 1,
		    "a lot??" = 2)
    ),
    selectInput("stage", "Stage",
		  list(
		    "Select a value" = -1, # default value
		    "1" = 1,
		    "2" = 2,
	            "3" = 3,
		    "4?" = 4)
    ), # Is this the same as nest state? If so, we don't need it and we can just derive it
    actionButton("submit_form_update_nest", "Submit Nest Update", width = 300)
  ),
  nav_panel(
    title = "New Nest"
  ),
  nav_panel(
    title = "Description"
  ),
  nav_menu( # looks bad on the shiny app right now, but will look better
    title = "Links",
    nav_item(tagList(a("GitHub Repo",href="https://github.com/data-at-reed-college/bushtit-database"))),
    nav_item("lab pages?")
  )
)


server = function(input, output) {
  # I think that these output$* assignments aren't necessary
#  output$observer = reactive({input$observer})
#  output$parents = reactive({input$parents})
#  output$tags = reactive({input$tags})
#  output$predation = reactive({input$predation})
#  output$stage = reactive({input$stage})

  observeEvent(input$submit_form_update_nest, {
    output$testing_submit_form_update_nest = renderText( # renderText just for testing purposes
      if (input$geolocation) { # if there is geolocation
        paste(input$observer, 
	      input$parents, 
	      input$tags, 
	      input$predation, 
	      input$stage, 
	      format(Sys.time(), "%x"), 
	      format(Sys.time(), "%R:%S:%p"),
	      input$lat,
	      input$long,
	      sep = ",")
      } else {
	paste("Unable to get location. Please allow location")
      }
    )
  })
}

shinyApp(ui=ui, server=server)
