library(tidyr); library(dplyr) # these are good to have, but might not be necessary. This is mostly base R or Shiny
library(shiny); library(htmltools); library(bslib) # shiny libraries
library(httr)

# We want to add weather utility, this is not too too hard to do
# We can use the wttr.in utility to do this and curl it through httr::GET
# I've been having some trouble getting it to work though with lat and lon
# but it should work just curling as usual
# try it out in your terminal
#curl wttr.in
# we could also do it in R with httr::GET("wttr.in")
# but this gives too much info, we want to get just the current temp, humidity, and wind
# this https://wttr.in/:help or this https://itsfoss.gitlab.io/post/how-to-check-weather-details-from-command-line-in-linux/ probably can help
# I ran out of time to keep working on this or make any actual progress :/


submit_form = function(input, output, extension) {
  return(isolate({
    # I'm using an isolate block to "isolate" the function from the reactive input fields I use here. This is because otherwise any change in an input would re-evaluate this and  possibly duplicate the data many times.
#    if (input$geolocation) {
    truth = TRUE
    truth = truth && (input[[paste0("observer_", extension)]] != "")

    for (var in c("nest_state_",
		  "parents_",
		  "bag_construction_",
		  "predation_",
		  "stage_")) {
      truth = truth && (input[[paste0(var, extension)]] != -1)
    }

    if (truth) { 
      # if there is geolocation and all fields are filled out
      data_string = paste(
	input[[paste0("observer_", extension)]],
        input[[paste0("parents_", extension)]],
	input[[paste0("tags_", extension)]],
        input[[paste0("predation_", extension)]],
	input[[paste0("stage_", extension)]],
        input[[paste0("bag_", extension)]],
        format(Sys.time(), "%x"),  # get the date
        format(Sys.time(), "%R:%S:%p"), # get the time
	input$lat, # latitude and longitude from the widget we force on page load
        input$long,
        sep = ",") # sep should be a comma for csv format
      data_string = gsub("\'", "\\\\\'", data_string) # escape single and double quotes
      data_string = gsub("\"", "\\\\\'", data_string) 
      system(paste0("echo ", data_string, " >> ", extension, ".csv")) # save into a CSV
      output[[paste0("submit_form_", extension, "_result")]] = renderText("Saved data")
    } else {
      output[[paste0("submit_form_", extension, "_result")]] = renderText("Please fill out all fields")
    } 
  }))
}


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

ui = page_navbar(includeCSS("style.css"),
  title = "Bushtit Database Entry",
#  sidebar = sidebar(
#    tags$script(
#      paste(system("cat get_location.js", intern = TRUE), collapse="\n")) # query location
#  ),
  nav_panel(
    tags$script(
      paste(system("cat get_location.js", intern = TRUE), collapse="\n")), # query location
    style="margin-bottom:50px;",
    title = "Nest Update",
    card(layout_columns(
      p("weather"), fileInput("file1",NULL),gap="200px"),min_height=100), # this is where we can put the photo and weather info. I don't know how to query weather, but maybe we can get it from the GPS coordinates? We can also get time and date for free with R, so we don't need to ask users to input that.
    card(
      selectInput("nest_state_update_form", "Nest State", nest_state_options),
      selectInput("stage_update_form", "Stage", stage_options), # Is this the same as nest state? If so, we don't need it and we can just derive it
      selectInput("parents_update_form", "Parent presence", parent_presence_options),
      selectInput("bag_construction_update_form", "Bag Construction Types", bag_construction_options), min_height=400),
    card(layout_columns(
      textInput("observer_update_form", "Observers", value = ""), # could be a choice if there are a discrete number of people... would also require more maintenance
      # No need to query time, R can do that on submission
      # Working on GPS coordinates. Ideally we shouldn't need users to enter them
      # Date comes free with R
      textInput("tags_update_form", "Tags", "None"), # I don't know what type of data tags_update_form are. This may be better suited to a selectInput()
      selectInput("predation_update_form", "Predation", predation_options)),
      min_height=150),
    layout_column_wrap(actionButton("submit_form_update_form", "Submit Nest Update", width = 300),gap="100px"),
    textOutput("submit_form_update_form_result"),
    headerPanel(""), headerPanel("") # so we have extra white space at the end of the page
  ),
  nav_panel(
    style="margin-bottom:50px;",
    title = "New Nest",
    card(layout_columns(
      p("weather"), fileInput("file2",NULL),gap="200px"),min_height=100),
    # R can get time for free
    # we can get gps for free
    # We can get date for free
    card(
      selectInput("nest_state_new_nest", "Nest State", nest_state_options),
      selectInput("stage_new_nest", "Stage", stage_options), # Is this the same as nest state? If so, we don't need it and we can just derive it
      selectInput("parents_new_nest", "Parent presence", parent_presence_options),
      selectInput("bag_construction_new_nest", "Bag Construction Types", bag_construction_options), min_height=400),
    card(layout_columns(
      textInput("observer_new_nest", "Observers", value = ""), # could be a choice if there are a discrete number of people... would also require more maintenance
      textInput("tags_new_nest", "Tags", "None"), # I don't know what type of data tags are. This may be better suited to a selectInput()
      selectInput("predation_new_nest", "Predation", predation_options)), min_height=150),
    layout_column_wrap(actionButton("submit_form_new_nest", "Submit New Nest", width = 300),gap="100px"),
    textOutput("submit_form_new_nest_result"),
    headerPanel(""), headerPanel("") # so there's extra white space
  ),
  nav_panel( # we can put a description here
    title = "Description",
    h1("Welcome to our shiny app."),
    p("Hiii. This is a description. This one is temporary. This is a shiny app so people in the D'Amelio lab can submit update forms from their phone. In principle, this should be hosted on some server, and the csv files could be hooked up to a SQL database.")
  ),
  nav_menu( # looks bad on the shiny app right now, but will look better
    title = "Links",
    nav_item(tagList(a("GitHub Repo",href="https://github.com/data-at-reed-college/bushtit-database"))),
    nav_item(tagList(a("D'Amelio lab?", href="https://www.reed.edu/faculty-profiles/profiles/damelio-pietro.html"))),
    nav_item("lab pages?")
  )
)


server = function(input, output) {
  # I defined a helper function at the beginning of the document to automate submitting the form based on which form we  are submitting (because they require the same information, but I wanted them to be distinct fields.
  observeEvent(input$submit_form_update_form, submit_form(input, output, extension = "update_form"))
  observeEvent(input$submit_form_new_nest, submit_form(input, output, extension = "new_nest"))
}

shinyApp(ui=ui, server=server)
