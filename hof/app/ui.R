library(DT)
library(ggvis)
options(scipen=5)

avail_years = c(1936:2017, "All Ballots", "All Players") # for showing specific voting classes
fluidPage(
  titlePanel('Hall of Fame Inductees'),
  
    fluidRow(
      column(3,
             wellPanel(
               h4("Filter"),
               selectInput('xcol', 'X Variable', names(df),selected="WAR_tot"),
               selectInput('ycol', 'Y Variable', names(df),
                           selected="H_tot"),
               selectInput('voting_year', 'Voting Year', avail_years,selected="All Players"),
               checkboxInput("show_currents", label = "Show Current Players", value = FALSE)
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