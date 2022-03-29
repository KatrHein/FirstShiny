#' ---
#' title: "Shiny Exercise"
#' author: "Katrin Heinrichs"
#' date: "March 2020"
#' output:
#'    html_document:
#'         number_sections: true
#'         toc: yes
#'         toc_depth: 3
#' ---
#' 

#' For a real shiny app, ui and server should probably be separated...


library(shiny)

#library(ggplot2)
#library(tidyverse)
#library(reshape2)
#library(lubridate)
#library(stringi) # for stri_split


 
fct_calc_sth <- function(number_list){
  sth <-  mean(number_list, na.rm = TRUE) + 2 + length(number_list)
  return(sth)
}

# function for plot over one series

fct_sq_sum <- function(number_list){
  
  sq_sum <- sum( (number_list)^2 )
}



vectorList <- list(a = c(-1, 2, 4, 8), b = c(0, 1, -2, 7), c = c(11, 12, 13, 14))


# actual app 

ui <- fluidPage(
  # dark theme below, uncommented - find out about other themes!
  #theme = bslib::bs_theme(bootswatch = "darkly"),
  
  titlePanel(HTML("Useful Shiny Mini Examples"), 
             windowTitle = "Shiny Mini Examples"),
  
  fluidRow(column(12, HTML("A page to test various concepts 
                    that were more or less 
                    new to me when I discovered Shiny. <hr>") ) ),
  
  # TODO: add toc, poss. with insertTab?
  sidebarLayout(position = "left",
                
                sidebarPanel(h3("Exercises in..."),
                             
                            helpText("( This is a helpText()-widget-example: Turn the sidebarPanel into navigation bar 
                                      in later version? 
                                      Or add one elsewhere? )"),
                             
                            h3(code("reactive")),
                            HTML("<hr>"),
                            
                            p("Choose reactive variable,  i.e. short vector of four numbers (a, b or c):"),
                            selectInput("ListSelected", 
                                        label= "Vector (by name)", 
                                        choices = names(vectorList), #list("a", "b", "c"),
                                        selected =NULL),
                            p("Btw, the available vectors are"),
                            tableOutput("vecOverview"),
                            
                            HTML("<hr>"),
                            
                            h3(code("hover (plot)")),
                            
                            HTML("<hr>"),
                            
                            p("Displays x and y coordinates below plot when hovering over plot
                              (with intentional delay)."),
                            p("Not very satisfying yet. Work in Progress: The hover coordinates 
                              jump between NULL (when not hovering) and the hover coordinates. As a consequence, 
                              the plot 
                              jumps upwards or downwards in the browser when giving more coordinates than x and y."),
                            p("Might be worth to think about avoiding this jumpy behaviour."),
                            
                            HTML("<hr>")
                            
                            ), # end sidebarPanel
                
                mainPanel(
                  
                 h3( HTML("Exercises in <code> reactive </code>")),
                 HTML("<hr>"),
                 h4( HTML("See what happens when <b>reactive variable</b>  
                          (here: vector) changes.")),
                 p("Changes are made by picking vector by name on the left."),
                 p("Declaring the vector and associated functions as reactive
                   causes the automatic updating of said functions."),
  
                  fluidRow(

                    column(4, htmlOutput("showChoice")), # htmlOutput for formatting
                    column(4, htmlOutput("showDetails")) 
                    ),
                 
                 HTML("<br>"),
                 
                 p("When we choose the vector, 
                                       which is reactive, functions and other
                                       elements containing the reactive variable
                                       (vector in this case) automatically adjust."
                          ),
                 HTML("<br>"),
                 
                 fluidRow(
                    column(4, textOutput("applyFct")),
                    column(4, textOutput("Fct_wrong"))#,
                   # column(12, helpText("This could be help text"))        
                          ),
                 
                 HTML("<hr>"),
                 
                 column(12, plotOutput("vectPlot",
                                       hover = hoverOpts(
                                       id = "image_hover",
                                       delay = 350,
                                       delayType = "throttle"
                                       )
                                       )),
                 column(offset = 1, 10, 
                        verbatimTextOutput("image_hoverinfo")
                        ),
                 
                 HTML("<hr>"),
                 HTML("<br>")
                 
                ) # end sidebarLayout
                        ), # end mainPanel
  
  fluidRow(column(12, HTML(" <hr> This is a fluidRow below everything. And I add 
                           a lot of Text so you see that it spans the whole page
                           when I add a fluidRow after the end of mainPanel. 
                           For extremely wide screens this might still not be enough...
                           .......................................... :-) <hr>") ) ),
  
                  ) # end fluidPage

server <- function(input, output, session) {
  
  # declaration of reactive variables
  # declaring it this way makes the code easier to read
  # might be less efficient, though?
  
  list_index <- reactive({input$ListSelected})

  sth_funny <- reactive({ fct_calc_sth( vectorList[[list_index()]] ) })
  
  sq_sum <- reactive({ fct_sq_sum( vectorList[[list_index()]] ) })
  
  
  # sidebar output
  output$vecOverview <- renderTable( data.frame(vectorList), striped=TRUE )
  
  # main output
  output$showChoice  <- renderText({ 
                        paste("<b>Your choice: </b>", list_index()) 
                                  })
  output$showDetails <- renderText({ 
                        paste("Your chosen vector <b>", list_index() , 
                              "</b>contains the <b> elements: <br>",
                              paste( vectorList[[list_index()]] , collapse = " " ), "</b>" ) 
                                 }) 
  output$applyFct <- renderText({ 
                     paste("sum of squared elements of", 
                                         list_index(), ":", sq_sum() ) 
                                  })
  
  # ok, seems that i do not need reactive for the function if the variable is reactive. good!
  output$Fct_wrong <- renderText({ 
                      paste( "Some useless function result gives", sth_funny() )
                                })
  
  
  output$vectPlot <- renderPlot({
                     plot(vectorList[[list_index()]], ylab = list_index())
                                })
  
  output$image_hoverinfo <- renderPrint({
                            cat("Hover (throttled): \n.................
                            \nx: \t", round( as.numeric(input$image_hover$x), 2 ),
                            "\ny: \t", round( as.numeric(input$image_hover$y), 2 ),
                            "\n.................")
                                        })
  
}

shinyApp(ui, server)