library(tidyr); library(dplyr)
library(shiny); library(htmltools); library(bslib) # last two libs might not be necessary

nest_state_options = list(
  "Select a value" = -1, # default value
  "Stage 1: Base of material present, with little shape visible" = 1,
  "Stage 2: I don't know what the description is" = 2,
  "Stage 3: Nest complete, but with no sign of brooding or feeding" = 3,
  "Stage 4+: We can add more" = 4
)

parent_presence_options = list(
  "Select a value" = -1, # default value
  "0 Parents" = 0,
  "1 Parent" = 1,
  "2 Parents" = 2,
  "3+ Parents?" = 3
)

bag_construction_options = list(
  "Select a value" = -1, # default value
  "The first type" = 1,
  "The second type?" = 2
)

predation_options = list(
  "Select a value" = -1, # default value. Not Working???
  "none" = 0,
  "some?" = 1,
  "a lot??" = 2
)

stage_options = list(
  "Select a value" = -1, # default value
  "1" = 1,
  "2" = 2,
  "3" = 3,
  "4?" = 4
)

ui = page_navbar(
  title = "Bushtit Database Entry",
  sidebar = sidebar(
    tags$script(
      paste(system("cat get_location.js", intern = TRUE), collapse="\n")) # query location
  ),
  nav_panel(
    style="margin-bottom:50px;",
    title = "Nest Update",
    textInput("observer_update_form", "Observer", value = ""), # could be a choice if there are a discrete number of people... would also require more maintenance
    # No need to query time, R can do that on submission
    # Working on GPS coordinates. Ideally we shouldn't need users to enter them
    # Date comes free with R
    selectInput("nest_state_update_form", "Nest State", nest_state_options),
    selectInput("parents_update_form", "Parent presence", parent_presence_options),
    textInput("tags_update_form", "Tags?", "None"), # I don't know what type of data tags_update_form are. This may be better suited to a selectInput()
    selectInput("bag_construction_update_form", "Bag Construction Types", bag_construction_options),
    selectInput("predation_update_form", "Signs of Predation?", predation_options),
    selectInput("stage_udpate_form", "Stage", stage_options), # Is this the same as nest state? If so, we don't need it and we can just derive it
    actionButton("submit_form_update_nest", "Submit Nest Update", width = 300),
    textOutput("submit_form_update_nest_result"),
    headerPanel(""), headerPanel("") # so we have extra white space at the end of the page
  ),
  nav_panel(
    style="margin-bottom:50px;",
    title = "New Nest",
    textInput("observer", "Observer", value = ""), # could be a choice if there are a discrete number of people... would also require more maintenance
    # R can get time for free
    # we can get gps for free
    # We can get date for free
    selectInput("nest_state_new_nest", "Nest State", nest_state_options),
    selectInput("parents_new_nest", "Parent presence", parent_presence_options),
    textInput("tags_new_nest", "Tags?", "None"), # I don't know what type of data tags are. This may be better suited to a selectInput()
    selectInput("bag_construction_new_nest", "Bag Construction Types", bag_construction_options),
    selectInput("predation_new_nest", "Signs of Predation?", predation_options),
    selectInput("stage_new_nest", "Stage", stage_options), # Is this the same as nest state? If so, we don't need it and we can just derive it
    actionButton("submit_form_new_nest", "Submit New Nest form", width = 300),
    textOutput("submit_form_new_nest_result"),
    headerPanel(""), headerPanel("") # so there's extra white space
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
#  output$parents_update_form = reactive({input$parents_update_form})
#  output$tags_update_form = reactive({input$tags_update_form})
#  output$predation = reactive({input$predation})
#  output$stage = reactive({input$stage})

  observeEvent(input$submit_form_update_nest, isolate({
    # I'm using an isolate block to "isolate" the function from the reactive input fields I use here. This is because otherwise any change in an input would re-evaluate this and  possibly duplicate the data many times.
#    if (input$geolocation) {
      if ((input$observer_update_form != "") && (input$parents_update_form != -1) && (input$predation_update_form != -1) && (input$stage_udpate_form != -1) && (input$bag_construction_update_form != -1)) { 
	# if there is geolocation and all fields are filled out
        data_string = paste(
	  input$observer_update_form, 
          input$parents_update_form, 
	  input$tags_update_form, 
          input$predation_update_form, 
	  input$stage_udpate_form, 
	  input$bag_construction_update_form,
          format(Sys.time(), "%x"), 
          format(Sys.time(), "%R:%S:%p"),
	  input$lat,
	  input$long,
          sep = ",")
	data_string = gsub("\'", "\\\\\'", data_string)
	data_string = gsub("\"", "\\\\\'", data_string) # escape single and double quotes
	system(paste("echo", data_string, ">> nest_updates.csv")) # save into a CSV
	output$submit_form_update_nest_result = renderText("Saved data")
      } else {
	output$submit_form_update_nest_result = renderText("Please fill out all fields")
      } 
#    } else {
#	output$submit_form_update_nest_result = renderText("Unable to get location. Please allow location")
#      }
    })
  )
}

shinyApp(ui=ui, server=server)
