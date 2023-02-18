#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

if (!requireNamespace("BiocManager", quietly = TRUE)){
  install.packages("BiocManager")
}
library(BiocManager)
options(repos = BiocManager::repositories())
library(shiny)
library(tidyverse)

load(url("https://github.com/johnkearns617/Princeton_Gym/blob/main/data.RData?raw=true"))

require(shinyjs)

# Define UI for application that draws a histogram
ui <- fluidPage(

   
    titlePanel("Should you go to the gym?"),
    mainPanel(textOutput('text1'),
              tags$head(tags$style(paste0("#text1{color: ",color,";
                                 font-size: 20px;
                                 font-style: italic;
                                 }")
              )
              ),
              plotOutput("plot2"))
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$text1 = renderText({do_i_go})
    
  output$plot2<-renderPlot({
    ggplot() +
      geom_bar(data=tail(gym_popular,1),aes(x=hour,y=current_popularity,fill="Live Popularity"),stat='identity') +
      geom_bar(data=manch_bars1$popular_times[[1]] %>% filter(day_of_week==lubridate::wday(Sys.time())),aes(x=hour,y=popularity,fill="Expected Popularity"),stat='identity',alpha=.5,width=.7) +
      labs(x='Hour',y='Popularity Level',caption=paste0(Sys.Date(),"\n\n100 denotes the gym is as busy as it gets during the week, or worse.")) +
      ggthemes::theme_fivethirtyeight() +
      ggthemes::scale_fill_fivethirtyeight() +
      theme(plot.caption = element_text(hjust = 0),axis.title = element_text(), axis.title.x = element_text(),legend.position="bottom") +
      lims(y=c(0,100)) +
      xlab("Hour")
  })

}
# Run the application 
shinyApp(ui = ui, server = server)
