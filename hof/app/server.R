library(ggplot2)
library(ggvis)
library(dplyr)
library(DT)

options(digits=3)

function(input, output, session) {
  df = read.csv('/Users/jledoux/Documents/projects/Saber/baseball-data/predict_hof_bat.csv')
  hof = read.csv('/Users/jledoux/Documents/projects/Saber/baseball-data/lahman/HallOfFame.csv')
  on_recent_ballot = hof[hof$yearid==2017,'playerID']
  df = df[!is.na(df$playerID),]
  print(head(df))
  df$is_current = ifelse(df$last_season==2017,1,0)
  df$Name = paste0(df$nameFirst, " ", df$nameLast)
  #df = read.csv('/Users/jledoux/Documents/projects/Saber/hof/data/all_past_orig.csv')
  preds = read.csv('/Users/jledoux/Documents/projects/Saber/hof/data/currents_with_preds_logitcv2.csv')
  head(preds)
  keeps = c('Name', 'first_season', 'last_season', 'years_in_league', 'G_tot', 'H_tot', 'HR_tot', 'AVG', 'BABIP', 'OBP', 'wOBA', 
            'SLG', 'BB_tot', 'WAR_tot', 'RAR_tot', 'prob_hof')
  data <- preds#left_join(preds, df[,c('playerID','nameFirst','nameLast')],by=c('playerID'='playerID'))[,keeps]
  #data$Name = paste0(data$nameFirst, " ", data$nameLast)
  #data$nameFirst = NULL
  #data$nameLast = NULL
  data = data[,c('Name', 'first_season', 'G_tot', 'H_tot', 'HR_tot', 'AVG', 'BABIP', 'OBP', 'wOBA', 
                 'SLG', 'BB_tot', 'WAR_tot', 'RAR_tot', 'prob_hof')]
  names(data) = c('Name', 'first_season', 'G', 'H', 'HR', 'AVG', 'BABIP', 'OBP', 'wOBA', 
                  'SLG', 'BB', 'WAR', 'RAR', 'Hall_Prob')
  is.num <- sapply(data, is.numeric)
  data[is.num] <- lapply(data[is.num], round, 3)
  data[data$Hall_Prob==1, 'Hall_Prob'] = 0.999
  
  df = df[(df$AB_tot>100),] # drop players with few atbats
  print(dim(df))
  print(length(unique(df$Name)))
  # Function for generating tooltip text
  player_tooltip <- function(x) {
    #if (is.null(x)) return(NULL)
    #if (is.null(x$playerID)) return(NULL)
    all_players = isolate(df)
    print(head(all_players))
    # Pick out the movie with this ID
    #all_players <- df#isolate(df)
    player <- head(all_players[all_players$playerID == x$playerID, ],1) # for some reason I'm getting a second row of NAs
    print(player)
    paste0("<b>", player$Name, "</b> <br>",
           paste("<a href=","'", paste0('https://www.baseball-reference.com/players/',substr(player$bbrefID,0,1),"/",player$bbrefID,'.shtml'),"'", ">Baseball Reference</a>"), "<br>",
           "Years in league: ", player$years_in_league, "<br>",
           "Hall of Fame Class: ", ifelse(player$inducted=="Y", player$yearid, " "), "<br>"
    )
  }
  
  # A reactive expression with the ggvis plot
  vis <- reactive({
    # Lables for axes
    xcol_name <- input$xcol
    ycol_name <- input$ycol
    voting_year = input$voting_year
    show_currents = input$show_currents
    if(show_currents == FALSE){
      df = df[df$is_current==0,]
    }
    print(head(df))
    if(voting_year!="All Players"){
      if(voting_year=="All Ballots"){
        df = df[df$playerID %in% unique(hof$playerID),]
      }
      else{
        df = df[df$playerID %in% unique(hof[hof$yearid==voting_year,'playerID']),]
        print(df[df$nameLast=='Rose',])
      }
    }
    
    # Normally we could do something like props(x = ~BoxOffice, y = ~Reviews),
    # but since the inputs are strings, we need to do a little more work.
    xcol <- prop("x", as.symbol(input$xcol))
    ycol <- prop("y", as.symbol(input$ycol))
    
    print(df)
    df$inducted = factor(df$inducted, levels=c("Y", "N"))
    df %>%
      ggvis(x = xcol, y = ycol) %>%
      layer_points(size := 50, size.hover := 200,
                   fillOpacity := 0.2, fillOpacity.hover := 0.5,
                   stroke = ~inducted, key := ~playerID) %>%
      add_tooltip(player_tooltip, "click") %>%
      add_axis("x", title = xcol_name) %>%
      add_axis("y", title = ycol_name) %>%
      add_legend("stroke", title = "Member of Hall of Fame", values = c("Yes", "No")) %>%
      scale_nominal("stroke", domain = c("Y", "N"),
                    range = c("orange", "#aaa")) %>%
      set_options(width = 500, height = 500)
  })
  
  vis %>% bind_shiny("plot1")
  
  output$table <- DT::renderDataTable(DT::datatable({
    data[order(-data$Hall_Prob),]
  }))
}
