library(DT)
library(ggvis)
options(scipen=5)

fluidPage(
  titlePanel('Hall of Fame Inductees'),
  
    fluidRow(
      column(3,
             wellPanel(
               h4("Filter"),
               selectInput('xcol', 'X Variable', names(df),selected="WAR_tot"),
               selectInput('ycol', 'Y Variable', names(df),
                           selected="H_tot")
             )
      ),
      
      column(
        9, ggvisOutput("plot1")
      )
    ),
  fluidRow(
    # Create a new row for the table.
      DT::dataTableOutput("table")
  )
  
)