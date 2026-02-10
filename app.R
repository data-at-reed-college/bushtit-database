library(tidyr); library(dplyr)
library(shiny); library(htmltools); library(DT); library(bslib) # last two libs might not be necessary

## TO DO:
### Have fields for all the data they want - requires the google sheets with sample data
#### Do this for the other form as well
### Have R query IP, and then match that to a location (ambitious), or have location prompt
### Have a description; need that from them
### Add backend code. Shouldn't be too complicated
#### We could have a sample of the form update they'll submit though, could be fun.
##### would require that we split the page into two, might be hard for phones.

ui = page_navbar(
  title = "Bushtit Database Entry",
  sidebar = sidebar(
    # don't do anything yet
  ),
  nav_panel(
    title = "Nest Update",
    textInput("observer", "Observer", value = ""), # could be a choice if there are a discrete number of people... would also require more maintenance
    # No need to query time, R can do that on submission
    # Working on GPS coordinates. Ideally we shouldn't need users to enter them
    # Date comes free with R
    selectInput("nest_state", "Nest State", 
      list("Stage 1: Base of material present, with little shape visible" = 1,
	   "Stage 2: I don't know what the description is" = 2,
  	   "Stage 3: Nest complete, but with no sign of brooding or feeding" = 3,
	   "Stage 4+: We can add more" = 4)
    ),
    selectInput("parents", "Parents",
		  list(
		    "0 Parents" = 0,
		    "1 Parent" = 1,
		    "2 Parents" = 2,
		    "3+ Parents?" = 3)
    ),
    textInput("tags", "Tags?", "None"), # I don't know what type of data tags are. This may be better suited to a selectInput()
    selectInput("bag_construction", "Bag Construction Types", 
		  list(
		    "The first type" = 1,
		    "The second type?" = 2)
    ),
    selectInput("predation", "Signs of Predation?", 
		  list(
		    "none" = 0,
		    "some?" = 1,
		    "a lot??" = 2)
    ),
    selectInput("stage", "Stage",
		  list("1" = 1,
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
    nav_item("the github repo?"),
    nav_item("lab pages?")
  )
)


server = function(input, output) {
  output$name = reactive({input$name})
  output$value = reactive({input$value_1}) # this should be updated when we aren't using `value_1` as a variable
}

shinyApp(ui=ui, server=server)
