library(tidyverse)
library(shiny)
library(shinydashboard)
library(gganimate)
library(ggimage)
library(grid)
library(jpeg)
library(gifski)
library(png)
library(shinycssloaders)
library(rintrojs)
library(googlesheets)
library(plotly)
library(png)
library(sf)


goal_crease <- data.frame(
  x = c(87, 86, 87),
  y = c(-6, 0, 6)
)

rink <- rasterGrob(readJPEG("full-rink.jpg"))


team_logo <- c("http://www.capsinfo.com/images/NHL_Team_Logos/NHL_Ducks_Primary.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/NHL_Coyotes_Primary.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/NHL_Bruins_Primary.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/NHL_Sabres_Primary.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/carolina.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/NHL_BlueJackets_Primary.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/calgary.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/chicago.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/colorado.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/NHL_Stars_Primary.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/detroit.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/NHL_Oilers_Primary.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/NHL_Panthers_Primary.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/NHL_Kings_Primary.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/NHL_Wild_Primary.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/montreal.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/newjersey.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/NHL_Predators_Primary.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/NY-Islanders-Primary.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/newyorkr.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/NHL_Senators_Primary.png",
               "http://www.stickpng.com/assets/images/5a4fbba3da2b4f099b95da1a.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/NHL_Penguins_Primary.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/NHL_Sharks_Primary.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/stlouis.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/NHL_Lightning_Primary.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/NHL_MapleLeafs_Primary.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/Vancouver_Canucks.png",
               "https://upload.wikimedia.org/wikipedia/en/thumb/a/ac/Vegas_Golden_Knights_logo.svg/184px-Vegas_Golden_Knights_logo.svg.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/NHL_Jets_Primary.png",
               "http://www.capsinfo.com/images/NHL_Team_Logos/NHL_Capitals_Primary.png"
)

# Event Player 1 Aggregated from 0708 to 1718
event_player_1_aggregated <- read_csv("event_player_1_aggregated.csv")

player_name_aggregated <- event_player_1_aggregated %>% 
  pull(event_player_1)


# Event Player 1 from 1819
event_player_1_1819season <- read_csv("event_player_1_1819season.csv")

player_name_1819 <- event_player_1_1819season %>% 
  pull(event_player_1) %>%
  unique()

player_name_combined <- c(player_name_aggregated, setdiff(player_name_1819, player_name_aggregated))

player_name_combined_title <- player_name_combined %>%
  str_replace_all("\\.", " ") %>%
  str_to_title()

# # Create title case names since choices argument says:
# # If elements of the list are named, then that name rather than the value is displayed to the user. 
names(player_name_combined) <- player_name_combined_title



# Beginning of Shiny App----

## UI----
ui <- fluidPage(
  
  introjsUI(),
  
  titlePanel(tagList(
    img(src = "nhl-logo.png", height = 60, width = 60),
    
    span("PLAY-BY-PLAY", 
         span(
           introBox(
             actionButton("help", 
                          label = "Help",
                          icon = icon("question"),
                          style="color: #fff; background-color: #B21212; border-color: #B21212"),
             actionButton("Twitter", 
                          label = "Twitter",
                          icon = icon("twitter"),
                          width = "80px",
                          onclick ="window.open(`https://twitter.com/jsonbaik`, '_blank')",
                          style="color: #fff; background-color: #00acee; border-color: #00acee"),
             actionButton("github",
                          label = "Code",
                          icon = icon("github"),
                          width = "80px",
                          onclick ="window.open(`https://github.com/jasonbaik94/nhl-pbp`, '_blank')",
                          style="color: #fff; background-color: #767676; border-color: #767676"),
             data.step = 6,
             data.intro = "Share on Twitter! or View Code"),
           style = "position:absolute;right:2em;"))),
    windowTitle = "NHL Play-by-Play"),
  
  hr(),
  
  
  fluidRow(
    column(6,
           
           tabBox(title = "",
                  tabPanel("Shot Chart",
                           plotOutput("shot_chart") %>% withSpinner(type = 1)),
                  tabPanel("Interactive Shot Chart",
                           plotlyOutput("int_shot_chart") %>% withSpinner(type = 1)),
                  tabPanel("Player Density Plot",
                           plotOutput("player_shot_plot",
                                      width = "100%",
                                      height = "450px") %>% withSpinner(type = 1)),
                  tabPanel("Shot Animation",
                           imageOutput("anim") %>% withSpinner(type = 1)),
                  width = NULL, side = "left")
    ),
    # column(2,
    #        tags$h3("GAME SCORE"),
    #        verbatimTextOutput("game_score")
    #        ),
    column(6,
           introBox(
             selectInput("season", label = h4("NHL SEASON"), 
                         choices = list("2007-2008" = 0708,
                                        "2008-2009" = 0809,
                                        "2009-2010" = 0910,
                                        "2010-2011" = 1011,
                                        "2011-2012" = 1112,
                                        "2012-2013" = 1213,
                                        "2013-2014" = 1314,
                                        "2014-2015" = 1415,
                                        "2015-2016" = 1516,
                                        "2016-2017" = 1617,
                                        "2017-2018" = 1718,
                                        "2018-2019" = 1819
                         ),
                         selected = 1718),
             data.step = 1,
             data.intro = "Select NHL Season (2018-2019 Updated Daily at 11:00AM EST)"),
           
           introBox(
             DT::dataTableOutput("game_id_table"),
             data.step = 2,
             data.intro = "Search for the Official Official Game ID with filters")
           
    )
  ),
  
  fluidRow(
    
    column(3,
           introBox(
             checkboxInput("goal_check", "GOALS ONLY",
                           value = FALSE),
             data.step = 5,
             data.intro = "Check to only see Goals"),
           
           
           
           h4("NHL.COM GAME ID"),
           introBox(
             numericInput("choose_game_id", label = "", 
                          value = 2017030415),
             data.step = 3,
             data.intro = "Input Official Game ID"),
           
           h4("PLAYER NAME"),
           introBox(
             selectizeInput(inputId = "player_shot",
                            label = "",
                            choices = player_name_combined,
                            selected = "ALEX.OVECHKIN"),
             data.step = 4,
             data.intro = "Choose NHL Player")
           
    ),
    column(9,
           
           br(),
           
           tabBox(title = "",
                  tabPanel("Official Recap",
                           htmlOutput("official_recap")),
                  tabPanel("Shot Distance",
                           plotOutput("shot_distance")),
                  tabPanel("Goal Probability",
                           imageOutput("goal_probability_anim") %>% withSpinner(type = 1)),
                  width = NULL, side = "left"
           )
    )
  )
  
)  



## Server----
server <- function(input, output, session) {
  
  # PBP Data by Season
  pbp <- reactive({
    
    if(input$season == 1718) {
      read_csv("https://raw.githubusercontent.com/jasonbaik94/nhl-pbp-processed-data/master/pbp_20172018_processed.csv")
      
    } else if (input$season == 1819) {
      gs_auth(token = "previous_token.rds")
      result_processed <- gs_title("result_processed")
      result_processed %>% gs_read_csv()
      
    } else if (input$season == 0708) {
      read_csv("https://raw.githubusercontent.com/jasonbaik94/nhl-pbp-processed-data/master/pbp_20072008_processed.csv")
      
    } else if (input$season == 0809) {
      read_csv("https://raw.githubusercontent.com/jasonbaik94/nhl-pbp-processed-data/master/pbp_20082009_processed.csv")
      
    } else if (input$season == 0910) {
      read_csv("https://raw.githubusercontent.com/jasonbaik94/nhl-pbp-processed-data/master/pbp_20092010_processed.csv")
      
    } else if (input$season == 1011) {
      read_csv("https://raw.githubusercontent.com/jasonbaik94/nhl-pbp-processed-data/master/pbp_20102011_processed.csv")
      
    } else if (input$season == 1112) {
      read_csv("https://raw.githubusercontent.com/jasonbaik94/nhl-pbp-processed-data/master/pbp_20112012_processed.csv")
      
    } else if (input$season == 1213) {
      read_csv("https://raw.githubusercontent.com/jasonbaik94/nhl-pbp-processed-data/master/pbp_20122013_processed.csv")
      
    } else if (input$season == 1314) {
      read_csv("https://raw.githubusercontent.com/jasonbaik94/nhl-pbp-processed-data/master/pbp_20132014_processed.csv")
      
    } else if (input$season == 1415) {
      read_csv("https://raw.githubusercontent.com/jasonbaik94/nhl-pbp-processed-data/master/pbp_20142015_processed.csv")
      
    } else if (input$season == 1516) {
      read_csv("https://raw.githubusercontent.com/jasonbaik94/nhl-pbp-processed-data/master/pbp_20152016_processed.csv")
      
    } else {
      read_csv("https://raw.githubusercontent.com/jasonbaik94/nhl-pbp-processed-data/master/pbp_20162017_processed.csv")
    }
  })
  
  # Game ID
  game_id <- reactive({input$choose_game_id})
  
  
  # Help
  observeEvent(input$help,
               introjs(session, options = list("nextLabel"="Next",
                                               "prevLabel"="Previous",
                                               "skipLabel"="Skip")
               )
  )
  
  
  # Shot Chart
  output$shot_chart <- renderPlot({
    
    
    
    pbp <- pbp()
    
    # Game Date
    game_date <- pbp %>% 
      filter(game_id == game_id()) %>% 
      select(game_date) %>% 
      unique() %>% 
      pull()
    
    # Specify home/away
    home_team <- pbp %>% 
      filter(game_id == game_id()) %>% 
      select(home_team) %>% 
      unique() %>% 
      pull()
    
    away_team <- pbp %>% 
      filter(game_id == game_id()) %>% 
      select(away_team) %>% 
      unique() %>% 
      pull()
    
    # Home Team logo
    home_team_logo <- case_when(
      home_team == "ANA" ~ team_logo[1],
      home_team == "ARI" ~ team_logo[2],
      home_team == "BOS" ~ team_logo[3],
      home_team == "BUF" ~ team_logo[4],
      home_team == "CAR" ~ team_logo[5],
      home_team == "CBJ" ~ team_logo[6],
      home_team == "CGY" ~ team_logo[7],
      home_team == "CHI" ~ team_logo[8],
      home_team == "COL" ~ team_logo[9],
      home_team == "DAL" ~ team_logo[10],
      home_team == "DET" ~ team_logo[11],
      home_team == "EDM" ~ team_logo[12],
      home_team == "FLA" ~ team_logo[13],
      home_team == "L.A" ~ team_logo[14],
      home_team == "MIN" ~ team_logo[15],
      home_team == "MTL" ~ team_logo[16],
      home_team == "N.J" ~ team_logo[17],
      home_team == "NSH" ~ team_logo[18],
      home_team == "NYI" ~ team_logo[19],
      home_team == "NYR" ~ team_logo[20],
      home_team == "OTT" ~ team_logo[21],
      home_team == "PHI" ~ team_logo[22],
      home_team == "PIT" ~ team_logo[23],
      home_team == "S.J" ~ team_logo[24],
      home_team == "STL" ~ team_logo[25],
      home_team == "T.B" ~ team_logo[26],
      home_team == "TOR" ~ team_logo[27],
      home_team == "VAN" ~ team_logo[28],
      home_team == "VGK" ~ team_logo[29],
      home_team == "WPG" ~ team_logo[30],
      home_team == "WSH" ~ team_logo[31],
      TRUE ~ "ATTENTION"
    )
    
    # Away Team Logo
    away_team_logo <- case_when(
      away_team == "ANA" ~ team_logo[1],
      away_team == "ARI" ~ team_logo[2],
      away_team == "BOS" ~ team_logo[3],
      away_team == "BUF" ~ team_logo[4],
      away_team == "CAR" ~ team_logo[5],
      away_team == "CBJ" ~ team_logo[6],
      away_team == "CGY" ~ team_logo[7],
      away_team == "CHI" ~ team_logo[8],
      away_team == "COL" ~ team_logo[9],
      away_team == "DAL" ~ team_logo[10],
      away_team == "DET" ~ team_logo[11],
      away_team == "EDM" ~ team_logo[12],
      away_team == "FLA" ~ team_logo[13],
      away_team == "L.A" ~ team_logo[14],
      away_team == "MIN" ~ team_logo[15],
      away_team == "MTL" ~ team_logo[16],
      away_team == "N.J" ~ team_logo[17],
      away_team == "NSH" ~ team_logo[18],
      away_team == "NYI" ~ team_logo[19],
      away_team == "NYR" ~ team_logo[20],
      away_team == "OTT" ~ team_logo[21],
      away_team == "PHI" ~ team_logo[22],
      away_team == "PIT" ~ team_logo[23],
      away_team == "S.J" ~ team_logo[24],
      away_team == "STL" ~ team_logo[25],
      away_team == "T.B" ~ team_logo[26],
      away_team == "TOR" ~ team_logo[27],
      away_team == "VAN" ~ team_logo[28],
      away_team == "VGK" ~ team_logo[29],
      away_team == "WPG" ~ team_logo[30],
      away_team == "WSH" ~ team_logo[31],
      TRUE ~ "ATTENTION"
    )
    
    # Read in Home Team PNG logos
    home_team_logo_download <- readPNG(RCurl::getURLContent(home_team_logo))
    home_team_logo_alpha <- matrix(rgb(home_team_logo_download[,,1],home_team_logo_download[,,2],home_team_logo_download[,,3], home_team_logo_download[,,4] * 0.2), nrow=dim(home_team_logo_download)[1])
    
    # Read in Away Team PNG logos
    away_team_logo_download <- readPNG(RCurl::getURLContent(away_team_logo))
    away_team_logo_alpha <- matrix(rgb(away_team_logo_download[,,1],away_team_logo_download[,,2],away_team_logo_download[,,3], away_team_logo_download[,,4] * 0.2), nrow=dim(away_team_logo_download)[1])
    
    
    # Shot Chart
    pbp_processed <- reactive({ 
      # If GOALS ONLY:
      if(input$goal_check == TRUE) {
        
        pbp %>%
          # Check for NAs
          filter(!is.na(coords_x),
                 !is.na(coords_y),
                 !is.na(event_rinkside)) %>% 
          filter(game_id == game_id(),
                 event_type == "GOAL") %>% 
          select(game_date, event_team, event_type, home_team, away_team, coords_x, coords_y) %>% 
          # Y Coordinate: move Home Team shots with positive y coordinate and negative x coordinate to 
          # negative y coordinate and positive x coordinate / 
          # move Home team shots with negative y coordinate and negative x coordinate to positive y coordinate and
          # and positive x coordinate 
          mutate(coords_y = if_else((event_team == home_team) & coords_y > 0 & coords_x < 0,
                                    -1 * coords_y,
                                    if_else((event_team == home_team) & coords_y < 0 & coords_x < 0,
                                            -1 * coords_y, coords_y), coords_y)) %>% 
          mutate(coords_y = if_else((event_team == away_team) & coords_x > 0, -1 * coords_y, coords_y)) %>% 
          #  X coordinate: move Home Team Shots to right & Away Team Shots to left
          mutate(coords_x = if_else(event_team == home_team, 
                                    abs(coords_x),
                                    if_else((event_team == away_team) & coords_x > 0, coords_x * -1, coords_x), coords_x)) %>%
          # Prevent points from running off the rink
          mutate(coords_y = if_else(coords_y < -35, coords_y + 5, coords_y)) %>% 
          mutate(coords_y = if_else(coords_y > 35, coords_y - 5, coords_y)) %>% 
          mutate(coords_x = if_else(coords_x > 80, coords_x - 2.7, coords_x)) %>% 
          mutate(coords_x = if_else(coords_x < -80, coords_x + 2.7, coords_x))
        
        
      } else{
        
        pbp %>%
          # Check for NAs
          filter(!is.na(coords_x),
                 !is.na(coords_y),
                 !is.na(event_rinkside)) %>% 
          filter(game_id == game_id(),
                 event_type %in% c("SHOT", "GOAL")) %>% 
          select(game_date, event_team, event_type, home_team, away_team, coords_x, coords_y) %>% 
          # Y Coordinate: move Home Team shots with positive y coordinate and negative x coordinate to n
          # negative y coordinate and positive x coordinate / 
          # move Home team shots with negative y coordinate and negative x coordinate to positive y coordinate and
          # and positive x coordinate 
          mutate(coords_y = if_else((event_team == home_team) & coords_y > 0 & coords_x < 0,
                                    -1 * coords_y,
                                    if_else((event_team == home_team) & coords_y < 0 & coords_x < 0,
                                            -1 * coords_y, coords_y), coords_y)) %>% 
          mutate(coords_y = if_else((event_team == away_team) & coords_x > 0, -1 * coords_y, coords_y)) %>% 
          #  X coordinate: move Home Team Shots to right & Away Team Shots to left
          mutate(coords_x = if_else(event_team == home_team, 
                                    abs(coords_x),
                                    if_else((event_team == away_team) & coords_x > 0, coords_x * -1, coords_x), coords_x)) %>%
          # Prevent points from running off the rink
          mutate(coords_y = if_else(coords_y < -35, coords_y + 5, coords_y)) %>% 
          mutate(coords_y = if_else(coords_y > 35, coords_y - 5, coords_y)) %>% 
          mutate(coords_x = if_else(coords_x > 80, coords_x - 2.7, coords_x)) %>% 
          mutate(coords_x = if_else(coords_x < -80, coords_x + 2.7, coords_x))
      }
      
    })
    
    # Put Processed data in df
    df <- pbp_processed()
    
    # Plot
    df %>% 
      ggplot(aes(coords_x, coords_y)) +
      annotation_custom(rink, -100, 100, -45, 45) +
      annotation_raster(away_team_logo_alpha, ymin = -20, ymax = 18, xmin = -62, xmax = -25) +
      annotation_raster(home_team_logo_alpha, ymin = -20, ymax = 18, xmin = 27, xmax = 65) +
      geom_point(aes(color = event_team), size = 5, show.legend = FALSE) +
      coord_fixed() +
      xlim(-100, 100) +
      ylim(-45, 45) +
      theme_nothing() +
      theme(text = element_text(size = 15),
            plot.title = element_text(hjust = 0.5)) +
      ggtitle(paste0(game_date, "\n", away_team, " vs ", home_team)) +
      scale_color_manual(values = c("#000000", "slategrey"))
    
    
    
    
  })
  
  
  
  
  
  # Interactive Shot Chart
  output$int_shot_chart <- renderPlotly({
    
    pbp <- pbp()
    
    # Game Date
    game_date <- pbp %>% 
      filter(game_id == game_id()) %>% 
      select(game_date) %>% 
      unique() %>% 
      pull()
    
    # Specify home/away
    home_team <- pbp %>% 
      filter(game_id == game_id()) %>% 
      select(home_team) %>% 
      unique() %>% 
      pull()
    
    away_team <- pbp %>% 
      filter(game_id == game_id()) %>% 
      select(away_team) %>% 
      unique() %>% 
      pull()
    
    
    # Shot Chart
    pbp_processed <- reactive({ 
      # If GOALS ONLY:
      if(input$goal_check == TRUE) {
        
        pbp %>%
          # Check for NAs
          filter(!is.na(coords_x),
                 !is.na(coords_y),
                 !is.na(event_rinkside)) %>% 
          filter(game_id == game_id(),
                 event_type == "GOAL") %>% 
          # Add Goal Probabilities
          mutate(event_description = paste0(event_description, " ", "Goal Prob: ", round(prob_goal * 100, 1), "%")) %>% 
          # Edit event description
          mutate(event_description = gsub(".*-","",event_description),
                 event_description = gsub("Off. Zone,", "", event_description)) %>% 
          select(game_date, event_team, event_description, event_type, home_team, away_team, coords_x, coords_y) %>% 
          # Y Coordinate: move Home Team shots with positive y coordinate and negative x coordinate to n
          # negative y coordinate and positive x coordinate / 
          # move Home team shots with negative y coordinate and negative x coordinate to positive y coordinate and
          # and positive x coordinate 
          mutate(coords_y = if_else((event_team == home_team) & coords_y > 0 & coords_x < 0,
                                    -1 * coords_y,
                                    if_else((event_team == home_team) & coords_y < 0 & coords_x < 0,
                                            -1 * coords_y, coords_y), coords_y)) %>% 
          mutate(coords_y = if_else((event_team == away_team) & coords_x > 0, -1 * coords_y, coords_y)) %>% 
          #  X coordinate: move Home Team Shots to right & Away Team Shots to left
          mutate(coords_x = if_else(event_team == home_team, 
                                    abs(coords_x),
                                    if_else((event_team == away_team) & coords_x > 0, coords_x * -1, coords_x), coords_x)) %>%
          # Prevent points from running off the rink
          mutate(coords_y = if_else(coords_y < -35, coords_y + 5, coords_y)) %>% 
          mutate(coords_y = if_else(coords_y > 35, coords_y - 5, coords_y)) %>% 
          mutate(coords_x = if_else(coords_x > 80, coords_x - 2.7, coords_x)) %>% 
          mutate(coords_x = if_else(coords_x < -80, coords_x + 2.7, coords_x)) %>% 
          # Change event_type column to numeric for plotly
          mutate(event_type = if_else(event_type == "SHOT", 0, 1))
        
        
      } else{
        
        pbp %>%
          # Check for NAs
          filter(!is.na(coords_x),
                 !is.na(coords_y),
                 !is.na(event_rinkside)) %>% 
          filter(game_id == game_id(),
                 event_type %in% c("SHOT", "GOAL")) %>% 
          # Add Goal Probabilities
          mutate(event_description = paste0(event_description, " ", "Goal Prob: ", round(prob_goal * 100, 1), "%")) %>% 
          # Edit event description
          mutate(event_description = gsub(".*-","",event_description),
                 event_description = gsub("Off. Zone,", "", event_description)) %>% 
          select(game_date, event_team, event_description, event_type, home_team, away_team, coords_x, coords_y) %>% 
          # Y Coordinate: move Home Team shots with positive y coordinate and negative x coordinate to n
          # negative y coordinate and positive x coordinate / 
          # move Home team shots with negative y coordinate and negative x coordinate to positive y coordinate and
          # and positive x coordinate 
          mutate(coords_y = if_else((event_team == home_team) & coords_y > 0 & coords_x < 0,
                                    -1 * coords_y,
                                    if_else((event_team == home_team) & coords_y < 0 & coords_x < 0,
                                            -1 * coords_y, coords_y), coords_y)) %>% 
          mutate(coords_y = if_else((event_team == away_team) & coords_x > 0, -1 * coords_y, coords_y)) %>% 
          #  X coordinate: move Home Team Shots to right & Away Team Shots to left
          mutate(coords_x = if_else(event_team == home_team, 
                                    abs(coords_x),
                                    if_else((event_team == away_team) & coords_x > 0, coords_x * -1, coords_x), coords_x)) %>%
          # Prevent points from running off the rink
          mutate(coords_y = if_else(coords_y < -35, coords_y + 5, coords_y)) %>% 
          mutate(coords_y = if_else(coords_y > 35, coords_y - 5, coords_y)) %>% 
          mutate(coords_x = if_else(coords_x > 80, coords_x - 2.7, coords_x)) %>% 
          mutate(coords_x = if_else(coords_x < -80, coords_x + 2.7, coords_x)) %>% 
          # Change event_type column to numeric for plotly
          mutate(event_type = if_else(event_type == "SHOT", 0, 1))
      }
      
    })
    
    # Put Processed data in df
    df <- pbp_processed()
    
    # Plot
    df %>% 
      plot_ly(x = ~coords_x, y=~coords_y, 
              text= ~event_description)  %>%
      add_markers(size = ~event_type,
                  sizes = c(20, 250),
                  alpha = 0.75,
                  color = ~factor(event_team),
                  colors = c("black", "slategrey")
      ) %>%  
      layout(
        title = paste0(game_date, "\n", away_team, " vs ", home_team),
        xaxis = list(range = c(-100,100), title = "", showgrid = FALSE, showticklabels = FALSE), 
        yaxis = list(range = c(-45,45), title = "", showgrid = FALSE, showticklabels = FALSE),
        images= list(
          list(
            source= "https://i.imgur.com/Y2kOUX5.png",
            xref= "container",
            yref= "container",
            x= 0,
            y= 0.5,
            sizex= 1,
            sizey= 0.885,
            sizing = "stretch",
            xanchor = "left",
            yanchor = "middle",
            opacity= 0.9,
            layer = "below")
        )
      )
    
    
  })
  
  
  
  # Shot Animation
  output$anim <- renderImage({
    
    pbp <- pbp()
    
    # Game Date
    game_date <- pbp %>% 
      filter(game_id == game_id()) %>% 
      select(game_date) %>% 
      unique() %>% 
      pull()
    
    # Specify home/away
    home_team <- pbp %>% 
      filter(game_id == game_id()) %>% 
      select(home_team) %>% 
      unique() %>% 
      pull()
    
    away_team <- pbp %>% 
      filter(game_id == game_id()) %>% 
      select(away_team) %>% 
      unique() %>% 
      pull()
    
    # Home Team logo
    home_team_logo <- case_when(
      home_team == "ANA" ~ team_logo[1],
      home_team == "ARI" ~ team_logo[2],
      home_team == "BOS" ~ team_logo[3],
      home_team == "BUF" ~ team_logo[4],
      home_team == "CAR" ~ team_logo[5],
      home_team == "CBJ" ~ team_logo[6],
      home_team == "CGY" ~ team_logo[7],
      home_team == "CHI" ~ team_logo[8],
      home_team == "COL" ~ team_logo[9],
      home_team == "DAL" ~ team_logo[10],
      home_team == "DET" ~ team_logo[11],
      home_team == "EDM" ~ team_logo[12],
      home_team == "FLA" ~ team_logo[13],
      home_team == "L.A" ~ team_logo[14],
      home_team == "MIN" ~ team_logo[15],
      home_team == "MTL" ~ team_logo[16],
      home_team == "N.J" ~ team_logo[17],
      home_team == "NSH" ~ team_logo[18],
      home_team == "NYI" ~ team_logo[19],
      home_team == "NYR" ~ team_logo[20],
      home_team == "OTT" ~ team_logo[21],
      home_team == "PHI" ~ team_logo[22],
      home_team == "PIT" ~ team_logo[23],
      home_team == "S.J" ~ team_logo[24],
      home_team == "STL" ~ team_logo[25],
      home_team == "T.B" ~ team_logo[26],
      home_team == "TOR" ~ team_logo[27],
      home_team == "VAN" ~ team_logo[28],
      home_team == "VGK" ~ team_logo[29],
      home_team == "WPG" ~ team_logo[30],
      home_team == "WSH" ~ team_logo[31],
      TRUE ~ "ATTENTION"
    )
    
    # Away Team Logo
    away_team_logo <- case_when(
      away_team == "ANA" ~ team_logo[1],
      away_team == "ARI" ~ team_logo[2],
      away_team == "BOS" ~ team_logo[3],
      away_team == "BUF" ~ team_logo[4],
      away_team == "CAR" ~ team_logo[5],
      away_team == "CBJ" ~ team_logo[6],
      away_team == "CGY" ~ team_logo[7],
      away_team == "CHI" ~ team_logo[8],
      away_team == "COL" ~ team_logo[9],
      away_team == "DAL" ~ team_logo[10],
      away_team == "DET" ~ team_logo[11],
      away_team == "EDM" ~ team_logo[12],
      away_team == "FLA" ~ team_logo[13],
      away_team == "L.A" ~ team_logo[14],
      away_team == "MIN" ~ team_logo[15],
      away_team == "MTL" ~ team_logo[16],
      away_team == "N.J" ~ team_logo[17],
      away_team == "NSH" ~ team_logo[18],
      away_team == "NYI" ~ team_logo[19],
      away_team == "NYR" ~ team_logo[20],
      away_team == "OTT" ~ team_logo[21],
      away_team == "PHI" ~ team_logo[22],
      away_team == "PIT" ~ team_logo[23],
      away_team == "S.J" ~ team_logo[24],
      away_team == "STL" ~ team_logo[25],
      away_team == "T.B" ~ team_logo[26],
      away_team == "TOR" ~ team_logo[27],
      away_team == "VAN" ~ team_logo[28],
      away_team == "VGK" ~ team_logo[29],
      away_team == "WPG" ~ team_logo[30],
      away_team == "WSH" ~ team_logo[31],
      TRUE ~ "ATTENTION"
    )
    
    # Read in Home Team PNG logos
    home_team_logo_download <- readPNG(RCurl::getURLContent(home_team_logo))
    home_team_logo_alpha <- matrix(rgb(home_team_logo_download[,,1],home_team_logo_download[,,2],home_team_logo_download[,,3], home_team_logo_download[,,4] * 0.2), nrow=dim(home_team_logo_download)[1])
    
    # Read in Away Team PNG logos
    away_team_logo_download <- readPNG(RCurl::getURLContent(away_team_logo))
    away_team_logo_alpha <- matrix(rgb(away_team_logo_download[,,1],away_team_logo_download[,,2],away_team_logo_download[,,3], away_team_logo_download[,,4] * 0.2), nrow=dim(away_team_logo_download)[1])
    
    
    
    # Processed Data
    pbp_processed <- reactive({ 
      # If GOALS ONLY:
      if(input$goal_check == TRUE) {
        
        pbp %>%
          # Check for NAs
          filter(!is.na(coords_x),
                 !is.na(coords_y),
                 !is.na(event_rinkside)) %>% 
          filter(game_id == game_id(),
                 event_type == "GOAL") %>% 
          select(game_date, event_team, home_team, away_team, coords_x, coords_y) %>% 
          # Y Coordinate: move Home Team shots with positive y coordinate and negative x coordinate to n
          # negative y coordinate and positive x coordinate / 
          # move Home team shots with negative y coordinate and negative x coordinate to positive y coordinate and
          # and positive x coordinate 
          mutate(coords_y = if_else((event_team == home_team) & coords_y > 0 & coords_x < 0,
                                    -1 * coords_y,
                                    if_else((event_team == home_team) & coords_y < 0 & coords_x < 0,
                                            -1 * coords_y, coords_y), coords_y)) %>% 
          mutate(coords_y = if_else((event_team == away_team) & coords_x > 0, -1 * coords_y, coords_y)) %>% 
          #  X coordinate: move Home Team Shots to right & Away Team Shots to left
          mutate(coords_x = if_else(event_team == home_team, 
                                    abs(coords_x),
                                    if_else((event_team == away_team) & coords_x > 0, coords_x * -1, coords_x), coords_x)) %>% 
          # Add event_rinkside column
          mutate(event_rinkside = if_else(event_team == home_team, "R", "L")) %>% 
          # Add Event Index
          mutate(event_index = seq_along(event_rinkside)) %>% 
          select(event_index, coords_x, coords_y, event_rinkside, everything()) %>% 
          mutate(coords_x = 82 * ifelse(event_rinkside == "L", -1, 1),
                 coords_y = 0) %>% 
          rbind(pbp %>%
                  # Check for NAs
                  filter(!is.na(coords_x),
                         !is.na(coords_y),
                         !is.na(event_rinkside)) %>% 
                  filter(game_id == game_id(),
                         event_type == "GOAL") %>% 
                  select(game_date, event_team, home_team, away_team, coords_x, coords_y) %>% 
                  # Y Coordinate: move Home Team shots with positive y coordinate and negative x coordinate to n
                  # negative y coordinate and positive x coordinate / 
                  # move Home team shots with negative y coordinate and negative x coordinate to positive y coordinate and
                  # and positive x coordinate 
                  mutate(coords_y = if_else((event_team == home_team) & coords_y > 0 & coords_x < 0,
                                            -1 * coords_y,
                                            if_else((event_team == home_team) & coords_y < 0 & coords_x < 0,
                                                    -1 * coords_y, coords_y), coords_y)) %>% 
                  mutate(coords_y = if_else((event_team == away_team) & coords_x > 0, -1 * coords_y, coords_y)) %>% 
                  #  X coordinate: move Home Team Shots to right & Away Team Shots to left
                  mutate(coords_x = if_else(event_team == home_team, 
                                            abs(coords_x),
                                            if_else((event_team == away_team) & coords_x > 0, coords_x * -1, coords_x), coords_x)) %>% 
                  # Add event_rinkside column
                  mutate(event_rinkside = if_else(event_team == home_team, "R", "L")) %>% 
                  # Add Event Index
                  mutate(event_index = seq_along(event_rinkside)) %>% 
                  select(event_index, coords_x, coords_y, event_rinkside, everything()), .) %>% 
          arrange(event_index)
        
      } else{
        
        pbp %>%
          # Check for NAs
          filter(!is.na(coords_x),
                 !is.na(coords_y),
                 !is.na(event_rinkside)) %>% 
          filter(game_id == game_id(),
                 event_type %in% c("SHOT", "GOAL")) %>% 
          select(game_date, event_team, home_team, away_team, coords_x, coords_y) %>% 
          # Y Coordinate: move Home Team shots with positive y coordinate and negative x coordinate to n
          # negative y coordinate and positive x coordinate / 
          # move Home team shots with negative y coordinate and negative x coordinate to positive y coordinate and
          # and positive x coordinate 
          mutate(coords_y = if_else((event_team == home_team) & coords_y > 0 & coords_x < 0,
                                    -1 * coords_y,
                                    if_else((event_team == home_team) & coords_y < 0 & coords_x < 0,
                                            -1 * coords_y, coords_y), coords_y)) %>% 
          mutate(coords_y = if_else((event_team == away_team) & coords_x > 0, -1 * coords_y, coords_y)) %>% 
          #  X coordinate: move Home Team Shots to right & Away Team Shots to left
          mutate(coords_x = if_else(event_team == home_team, 
                                    abs(coords_x),
                                    if_else((event_team == away_team) & coords_x > 0, coords_x * -1, coords_x), coords_x)) %>% 
          # Add event_rinkside column
          mutate(event_rinkside = if_else(event_team == home_team, "R", "L")) %>% 
          # Add Event Index
          mutate(event_index = seq_along(event_rinkside)) %>% 
          select(event_index, coords_x, coords_y, event_rinkside, everything()) %>% 
          mutate(coords_x = 82 * ifelse(event_rinkside == "L", -1, 1),
                 coords_y = 0) %>% 
          rbind(pbp %>%
                  # Check for NAs
                  filter(!is.na(coords_x),
                         !is.na(coords_y),
                         !is.na(event_rinkside)) %>% 
                  filter(game_id == game_id(),
                         event_type %in% c("SHOT", "GOAL")) %>% 
                  select(game_date, event_team, home_team, away_team, coords_x, coords_y) %>% 
                  # Y Coordinate: move Home Team shots with positive y coordinate and negative x coordinate to n
                  # negative y coordinate and positive x coordinate / 
                  # move Home team shots with negative y coordinate and negative x coordinate to positive y coordinate and
                  # and positive x coordinate 
                  mutate(coords_y = if_else((event_team == home_team) & coords_y > 0 & coords_x < 0,
                                            -1 * coords_y,
                                            if_else((event_team == home_team) & coords_y < 0 & coords_x < 0,
                                                    -1 * coords_y, coords_y), coords_y)) %>% 
                  mutate(coords_y = if_else((event_team == away_team) & coords_x > 0, -1 * coords_y, coords_y)) %>% 
                  #  X coordinate: move Home Team Shots to right & Away Team Shots to left
                  mutate(coords_x = if_else(event_team == home_team, 
                                            abs(coords_x),
                                            if_else((event_team == away_team) & coords_x > 0, coords_x * -1, coords_x), coords_x)) %>% 
                  # Add event_rinkside column
                  mutate(event_rinkside = if_else(event_team == home_team, "R", "L")) %>% 
                  # Add Event Index
                  mutate(event_index = seq_along(event_rinkside)) %>% 
                  select(event_index, coords_x, coords_y, event_rinkside, everything()), .) %>% 
          arrange(event_index)
        
      }
    })  
    
    pbp_processed_test <- pbp_processed()
    
    anim_test <- pbp_processed_test %>%
      mutate(state = seq(1, n())) %>% 
      ggplot(aes(x = coords_x, y = coords_y, group = event_index)) +
      annotation_custom(rink, -100, 100, -45, 45) +
      annotation_raster(away_team_logo_alpha, ymin = -20, ymax = 18, xmin = -62, xmax = -25) +
      annotation_raster(home_team_logo_alpha, ymin = -20, ymax = 18, xmin = 27, xmax = 65) +
      geom_point(size = 5) +
      coord_fixed() +
      xlim(-100, 100) +
      ylim(-45, 45) +
      theme_nothing() +
      theme(plot.title = element_text(hjust = 0.5)) +
      transition_states(state, transition_length = 3) +
      # fade in for each point associated with a new event_index
      enter_fade() + 
      # cubic for smoother transition between states within each event_index
      ease_aes('cubic-in-out') + 
      # fade out for each point associated with an old event_index
      exit_fade() + 
      # add title to keep track of states
      labs(title = paste0(game_date, "\n", away_team, " vs ", home_team, 
                          "\nCount: {closest_state}")) 
    
    
    
    # Need to find number of rows of pbp_processed_test so that I can feed it into nframes
    # anim_test_nrow <- nrow(pbp_processed_test) 
    
    
    anim_save("outfile.gif", animate(anim_test, fps = 2)) # New
    
    # Return a list containing the filename
    list(src = "outfile.gif",
         contentType = 'image/gif',
         width = "450px",
         height = "400px"
    )
    
  })
  
  
  
  
  # Histogram of Shot Distance
  output$shot_distance <- renderPlot({
    
    pbp <- pbp()
    
    # Shot Distances
    event_distance_df <- pbp %>% 
      filter(game_id == game_id(),
             event_type %in% c("SHOT", "GOAL")) %>% 
      select(event_team, event_distance) 
    
    event_distance_df %>% 
      ggplot(aes(event_distance)) +
      geom_histogram(binwidth = 2.5, color = "black", fill = "#999999") +
      # Add Goal line
      geom_vline(xintercept=0.001, color = "red", size = 1) +
      # Add Faceoff dot
      geom_vline(xintercept=29.8, color = "red", size = 1.5) +
      # Add blueline
      geom_vline(xintercept=64, color = "blue3", size = 3) +
      # Add Center line
      geom_vline(xintercept=89, color = "red2", size = 3) +
      scale_x_continuous(breaks = c(0.001, 29.8, 64, 89), labels = c("Goal line", "Faceoff Dot",
                                                                     "Blue line", "Red line")) +
      labs(x = "",
           y = "# of Shots") +
      facet_wrap(~event_team, dir = "v") +
      theme_classic() +
      theme(text = element_text(size=20))
  })
  
  
  # Goal Probability Animation
  output$goal_probability_anim <- renderImage({
    
    pbp <- pbp()
    
    anim_goal_prob <- pbp %>% 
      filter(game_id == game_id(),
             event_type %in% c("GOAL", "SHOT")) %>% 
      select(game_seconds, event_team, prob_goal) %>% 
      mutate(game_time = round(game_seconds / 60, 2)) %>% 
      ggplot(aes(x = game_time, y = prob_goal * 100,
                 group = event_team)) +
      geom_path(aes(linetype = event_team), show.legend = FALSE) +
      geom_text(aes(label = event_team), 
                nudge_x = 1,
                nudge_y = 0.3,
                size = 5) +
      geom_vline(xintercept = 20, color = "slategrey", size = 1.5) +
      geom_vline(xintercept = 40, color = "slategrey", size = 1.5) +
      geom_vline(xintercept = 60, color = "slategrey", size = 1.5, linetype = "dashed") +
      labs(x = NULL,
           y = NULL) +
      scale_x_continuous(breaks = c(0, 20, 40, 60),
                         labels = c("GAME START", "END 1ST", "END 2ND", "END\nREG")) +
      scale_y_continuous(breaks = c(0, 10, 20, 30, 40),
                         labels = c("0%", "10%", "20%", "30%", "40%")) +
      theme_classic() +
      theme(text = element_text(size=20)) +
      transition_reveal(along = game_time) +
      ease_aes('linear')
    
    
    
    anim_save("goal-prob-anim.gif", animate(anim_goal_prob, fps = 5)) # New
    
    # Return a list containing the filename
    list(src = "goal-prob-anim.gif",
         contentType = 'image/gif',
         width = "550px",
         height = "400px"
         # alt = "This is alternate text"
    )
    
    
  })
  
  
  # Game ID Table
  output$game_id_table <- DT::renderDataTable({
    
    
    pbp <- pbp()
    
    # Customize pbp
    pbp_game_id <- pbp %>% 
      distinct(game_id, game_date, away_team, home_team) %>% 
      select(game_id, game_date, away_team, home_team)
    
    
    DT::datatable(pbp_game_id,
                  extensions = "Scroller",
                  filter = "top", options = list(
                    deferRender = TRUE,
                    scrollY = 200,
                    scroller = TRUE
                  ),
                  rownames = FALSE,
                  colnames = c("Game ID",
                               "Game Date",
                               "Away",
                               "Home")
    )
    
  })
  
  
  # NHL.COM Iframe
  output$official_recap <- renderUI({
    
    pbp <- pbp()
    
    # Game Date
    game_date <- pbp %>% 
      filter(game_id == game_id()) %>% 
      select(game_date) %>% 
      unique() %>% 
      pull()
    
    game_date_processed <- gsub(pattern = "-", replacement = "/", game_date)
    
    # Specify home
    home_team <- pbp %>% 
      filter(game_id == game_id()) %>% 
      select(home_team) %>% 
      unique() %>% 
      pull()
    # 2018020016
    
    # Specify away
    away_team <- pbp %>% 
      filter(game_id == game_id()) %>% 
      select(away_team) %>% 
      unique() %>% 
      pull()
    
    # Change S.J to SJS
    home_team <- if_else(home_team == "S.J", "SJS", home_team)
    away_team <- if_else(away_team == "S.J", "SJS", away_team)
    
    # Change L.A to LAK
    home_team <- if_else(home_team == "L.A", "LAK", home_team)
    away_team <- if_else(away_team == "L.A", "LAK", away_team)
    
    # Change N.J to NJD
    home_team <- if_else(home_team == "N.J", "NJD", home_team)
    away_team <- if_else(away_team == "N.J", "NJD", away_team)
    
    # Change T.B to TBL
    home_team <- if_else(home_team == "T.B", "TBL", home_team)
    away_team <- if_else(away_team == "T.B", "TBL", away_team)
    
    
    
    nhl_url <- paste0("https://www.nhl.com/gamecenter/",
                      str_to_lower(away_team), "-vs-", str_to_lower(home_team),
                      "/", game_date_processed, "/", game_id(), "#game=", game_id(), "game_state=final")
    
    tags$iframe(src = nhl_url, height = 380, width = 1200,
                scrolling = "auto")
    
  })
  
  
  
  
  player_name_reactive <- reactive({input$player_shot})
  
  # Player Shot Chart
  output$player_shot_plot <- renderPlot({
    
    # Season
    season <- reactive({
      
      if(input$season == 1718) {
        "2017-2018"
        
      } else if (input$season == 1819) {
        "2018-2019"
        
      } else if (input$season == 0708) {
        "2007-2008"
        
      } else if (input$season == 0809) {
        "2008-2009"
        
      } else if (input$season == 0910) {
        "2009-2010"
        
      } else if (input$season == 1011) {
        "2010-2011"
        
      } else if (input$season == 1112) {
        "2011-2012"
        
      } else if (input$season == 1213) {
        "2012-2013"
        
      } else if (input$season == 1314) {
        "2013-2014"
        
      } else if (input$season == 1415) {
        "2014-2015"
        
      } else if (input$season == 1516) {
        "2015-2016"
        
      } else {
        "2016-2017"
      }
      
    })
    
    season <- season()
    
    
    pbp <- pbp()
    player_name_processed <- player_name_reactive()
    
    
    player_name_title <-  player_name_processed %>%
      str_replace_all("\\.", " ") %>%
      str_to_title()
    
    
    pbp_player <- reactive({
      
      
      # If GOALS ONLY:
      if(input$goal_check == TRUE) {
        
        pbp %>% 
          filter(event_player_1 == player_name_processed,
                 event_type =="GOAL") %>% 
          # Y Coordinate: move Home Team shots with positive y coordinate and negative x coordinate to
          # negative y coordinate and positive x coordinate /
          # move Home team shots with negative y coordinate and negative x coordinate to positive y coordinate and
          # and positive x coordinate
          mutate(coords_y = if_else((event_team == home_team) & coords_y > 0 & coords_x < 0,
                                    -1 * coords_y,
                                    if_else((event_team == home_team) & coords_y < 0 & coords_x < 0,
                                            -1 * coords_y, coords_y), coords_y)) %>%
          mutate(coords_y = if_else((event_team == away_team) & coords_x > 0, -1 * coords_y, coords_y)) %>%
          #  X coordinate: move Home Team Shots to right & Away Team Shots to left
          mutate(coords_x = if_else(event_team == home_team,
                                    abs(coords_x),
                                    if_else((event_team == away_team) & coords_x > 0, coords_x * -1, coords_x), coords_x)) %>%
          # Move all shots taken on road to the right side of rink
          mutate(coords_y = if_else(event_team == away_team, coords_y * -1, coords_y)) %>%
          mutate(coords_x = if_else(event_team == away_team, abs(coords_x), coords_x)) %>%
          # Prevent points from running off the rink
          mutate(coords_y = if_else(coords_y < -35, coords_y + 2.7, coords_y)) %>%
          mutate(coords_y = if_else(coords_y > 35, coords_y - 2, coords_y)) %>%
          mutate(coords_x = if_else(coords_x > 80, coords_x - 2.7, coords_x)) %>%
          mutate(coords_x = if_else(coords_x < -80, coords_x + 2.7, coords_x)) %>% 
          # Take out NA values in X,Y coordinates
          filter(!is.na(coords_x)) %>% 
          filter(!is.na(coords_y))
        
        
        
        
      } else{
        
        pbp %>% 
          filter(event_player_1 == player_name_processed,
                 event_type %in% c("SHOT", "GOAL")) %>% 
          # Y Coordinate: move Home Team shots with positive y coordinate and negative x coordinate to
          # negative y coordinate and positive x coordinate /
          # move Home team shots with negative y coordinate and negative x coordinate to positive y coordinate and
          # and positive x coordinate
          mutate(coords_y = if_else((event_team == home_team) & coords_y > 0 & coords_x < 0,
                                    -1 * coords_y,
                                    if_else((event_team == home_team) & coords_y < 0 & coords_x < 0,
                                            -1 * coords_y, coords_y), coords_y)) %>%
          mutate(coords_y = if_else((event_team == away_team) & coords_x > 0, -1 * coords_y, coords_y)) %>%
          #  X coordinate: move Home Team Shots to right & Away Team Shots to left
          mutate(coords_x = if_else(event_team == home_team,
                                    abs(coords_x),
                                    if_else((event_team == away_team) & coords_x > 0, coords_x * -1, coords_x), coords_x)) %>%
          # Move all shots taken on road to the right side of rink
          mutate(coords_y = if_else(event_team == away_team, coords_y * -1, coords_y)) %>%
          mutate(coords_x = if_else(event_team == away_team, abs(coords_x), coords_x)) %>%
          # Prevent points from running off the rink
          mutate(coords_y = if_else(coords_y < -35, coords_y + 2.7, coords_y)) %>%
          mutate(coords_y = if_else(coords_y > 35, coords_y - 2, coords_y)) %>%
          mutate(coords_x = if_else(coords_x > 80, coords_x - 2.7, coords_x)) %>%
          mutate(coords_x = if_else(coords_x < -80, coords_x + 2.7, coords_x)) %>% 
          # Take out NA values in X,Y coordinates
          filter(!is.na(coords_x)) %>% 
          filter(!is.na(coords_y))
        
        
        
      }
      
    })
    
    
    pbp_player <- pbp_player()
    
    # working with simple features
    # a simple rectangle delimited by the min-max bounds of the court
    rinksf <- sf::st_as_sfc(sf::st_bbox(c(xmin = -100, xmax = 100, ymax = 43, ymin = -43)))
    
    rinkgrid <-
      sf::st_make_grid(rinksf, what = "polygons", cellsize = 10) %>%
      sf::st_sf() %>%
      dplyr::mutate(cellid = dplyr::row_number())
    
    
    # go from data frame to simple feature by declaring the variables with the xy coordinates
    pbp_player_sf <- pbp_player %>% sf::st_as_sf(coords = c("coords_x", "coords_y"))
    
    # calculate n per grid square for points
    shot_richness <- rinkgrid %>%
      sf::st_join(pbp_player_sf) %>% # spatial join of points and grid cells
      dplyr::group_by(cellid) %>% # group because each cell may have matched spatially with many shot points
      dplyr::summarize(num_shots = n()) # count how many matches per grid cell
    
    # Embedding Ice Rink image as background
    rink_url <- "https://i.imgur.com/Y2kOUX5.png"
    
    rink_download <- readPNG(RCurl::getURLContent(rink_url))
    rink_alpha <- matrix(rgb(rink_download[,,1],rink_download[,,2],rink_download[,,3], rink_download[,,4] * 0.45), nrow=dim(rink_download)[1])
    
    
    # together
    ggplot(shot_richness) +
      geom_sf(aes(fill = num_shots)) +
      annotation_raster(rink_alpha, -99.8, 99.8, -44, 47) +
      scale_fill_distiller(palette="YlOrRd", direction=1) +
      theme(text = element_text(size=13),
            panel.background = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      labs(fill = "Number of Shots") +
      ggtitle(paste(season, player_name_title, "Shot Density"))
    
    
  })     
  
}




shinyApp(ui, server)