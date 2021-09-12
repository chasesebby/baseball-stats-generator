library(shiny)
library(shinydashboard)
library(Lahman)
library(dplyr)
library(bslib)
library(thematic)
library(tidyverse)

bat_stats <- Batting %>%
  select(yearID, AB, R, H, X2B, X3B, HR, RBI, SB, BB, SO, HBP, GIDP) %>%
  group_by(yearID) %>%
  summarise(across(.cols = everything(), sum)) %>%
  filter(yearID >= 1939) %>%
  mutate("AVG" = H/AB,
         "R_rate" = R/AB*100,
         "H_rate" = H/AB*100,
         "X2B_rate" = X2B/AB*100,
         "X3B_rate" = X3B/AB*100,
         "HR_rate" = HR/AB*100,
         "RBI_rate" = RBI/AB*100,
         "SB_rate" = SB/AB*100,
         "BB_rate" = BB/AB*100,
         "SO_rate" = SO/AB*100,
         "HBP_rate" = HBP/AB*100,
         "GIDP_rate" = GIDP/AB*100
         )
pitch_stats <- Pitching %>%
  select(yearID, W, L, G, CG, IPouts, H, ER, HR, BB, SO, BAOpp, ERA, HBP) %>%
  group_by(yearID) %>%
  summarise(across(.cols = everything(), sum)) %>%
  filter(yearID >= 1939) %>%
  mutate("BAOpp" = H/(IPouts+H), 
         "ERA" = 9*ER/(IPouts/3), 
         "IP" = IPouts/3,
         "W_rate" = W/G, 
         "L_rate" = L/G, 
         "CG_rate" = CG/G, 
         "HR_rate" = HR/IPouts/3,
         "WHIP" = (BB+H)/IPouts/3,
         "SO_rate" = SO/IPouts/3, 
         "HBP_rate" = HBP/IPouts/3
         )

# Merge People and Batting datasets to have first and last names of players
#   in the same dataset

total_data_bat <- Batting %>%
  group_by(playerID, yearID) %>%
  filter(yearID >= 1939)

total_data_bat <- merge(total_data_bat, People, by = "playerID")
total_data_bat <- total_data_bat %>%
  select(nameFirst, nameLast, yearID, G, AB, R, H, X2B, 
         X3B, HR, RBI, SB, BB, SO, IBB, HBP, SH, SF, GIDP) %>%
  unite("last_first", nameLast:nameFirst, sep = ", ") %>%
  select(last_first, G, AB, R, H, X2B, X3B, HR, RBI, SB, BB, SO, IBB, HBP, SH, SF, GIDP) %>%
  group_by(last_first) %>%
  summarise(across(.cols = c(G, AB, R, H, X2B, X3B, HR, RBI, SB, 
                             BB, SO, IBB, HBP, SH, SF, GIDP), sum)) %>%
  mutate("AVG" = H/AB,
         "R_rate" = R/AB*100,
         "H_rate" = H/AB*100,
         "X2B_rate" = X2B/AB*100,
         "X3B_rate" = X3B/AB*100,
         "HR_rate" = HR/AB*100,
         "RBI_rate" = RBI/AB*100,
         "SB_rate" = SB/AB*100,
         "BB_rate" = BB/AB*100,
         "SO_rate" = SO/AB*100,
         "HBP_rate" = HBP/AB*100,
         "GIDP_rate" = GIDP/AB*100
  )
  
total_data_bat[is.na(total_data_bat)] <- 0


# Merge People and Hitting datasets to have first and last names of players
#   in the same dataset

total_data_pitch <- Pitching %>%
  group_by(playerID, yearID) %>%
  filter(yearID >= 1939)

total_data_pitch <- merge(total_data_pitch, People, by = "playerID")
total_data_pitch <- total_data_pitch %>%
  select(nameFirst, nameLast, yearID, W, L, G, CG, IPouts, H, ER, HR, BB, SO, BAOpp, ERA, HBP) %>%
  unite("last_first", nameLast:nameFirst, sep = ", ") %>%
  select(last_first, W, L, G, CG, IPouts, H, ER, HR, BB, SO, BAOpp, ERA, HBP) %>%
  group_by(last_first) %>%
  summarise(across(.cols = c(W, L, G, CG, IPouts, H, ER, HR, BB, SO, BAOpp, ERA, HBP), sum)) %>%
  mutate("BAOpp" = H/(IPouts+H), 
         "ERA" = 9*ER/(IPouts/3), 
         "IP" = IPouts/3,
         "W_rate" = W/G, 
         "L_rate" = L/G, 
         "CG_rate" = CG/G, 
         "HR_rate" = HR/IPouts/3,
         "WHIP" = (BB+H)/IPouts/3,
         "SO_rate" = SO/IPouts/3, 
         "HBP_rate" = HBP/IPouts/3
  )

total_data_pitch[is.na(total_data_pitch)] <- 0



ui <- navbarPage(
  theme = bs_theme(version = 4, bootswatch = "minty"),
  title = "Baseball Stats Generator",
  tabPanel(
    title = "Home",
    fluidRow(
             column(4, offset = 4,
             tags$h1("About the Data"),
             "You may use this app to visually explore relationships between several
             well-known baseball stats. The data is from the Lahman package in R and 
             the datasets were altered to only contain numbers from 1939 and later. 
             Each stat was grouped by year and the sum of all stats was calculated.
             Each datapoint on a graph will represent a total year's worth 
             of whatever stat you have chosen.",
             tags$br(), tags$br(),
             "I recommend to, when picking two stats to look at, use the _rate variables
             to do the comparing, as the other stats are heavily reliant on how many 
             games that were played.",
             tags$h5("Example:"),
             "A player who plays 162 games is likely to have
             more homeruns than a player who played 10 games. Say the player who played 162 games
             homered 15 times while the 10-game player homered 5 times. The total number
             of homers would not be a good representation of a hitter's power since the 10-game 
             player averaged a homerun every 2 games while the one who played the full
             season averaged a homerun every 10.8 games."
             )
           )
          ),
  
  # Batting Panel
  
  tabPanel(
    title = "Batting",
    fluidRow(
      column(
        4, 
        wellPanel(
        tags$h5("Pick Two Stats to Compare"),
           selectInput(inputId = "bat_stat_1", label = "Stat 1",
                       choices = c("AVG", "R", "H", "X2B", "X3B", "HR", "RBI", "SB", "BB", "SO", "HBP", 
                       "GIDP", "R_rate", "X2B_rate", "X3B_rate", "HR_rate", "RBI_rate",
                       "SB_rate", "BB_rate", "SO_rate", "HBP_rate", "GIDP_rate"), selected = "R_rate"),
           selectInput(inputId = "bat_stat_2", label = "Stat 2",
                       choices = c("AVG", "R", "H", "X2B", "X3B", "HR", "RBI", "SB", "BB", "SO", "HBP", 
                       "GIDP", "R_rate", "X2B_rate", "X3B_rate", "HR_rate", "RBI_rate",
                       "SB_rate", "BB_rate", "SO_rate", "HBP_rate", "GIDP_rate"), selected = "H_rate")),
            tags$h5("Legend:"), 
                    "AVG = Batting Average", tags$br(),
                    "R = Runs", tags$br(),
                    "H = Hits", tags$br(),
                    "X2B = Doubles", tags$br(),
                    "X3B = Triples", tags$br(),
                    "HR = Homeruns", tags$br(),
                    "RBI = Runs Batted In", tags$br(),
                    "SB = Stolen Bases", tags$br(),
                    "BB = Walks", tags$br(),
                    "SO = Strikeouts", tags$br(),
                    "HBP = Hit by Pitches", tags$br(),
                    "GIDP = Ground into Double Plays", tags$br(),
                    "*_rate = * / At Bats * 100"
        ),
      column(
        8, mainPanel(plotOutput("batting"))
        )
      )
  ),
  
  # Pitching panel
  
  tabPanel(
    title = "Pitching",
    fluidRow(
           column(
             4, 
             wellPanel(
             tags$h5("Pick Two Stats to Compare"),
              selectInput(inputId = "pitch_stat_1", label = "Stat 1",
                       choices = c("W", "L", "IP", "H", "ER", "HR", "BB", "SO", "BAOpp", "ERA",
                                   "HBP", "W_rate", "L_rate", "CG_rate", "HR_rate", "WHIP", 
                                   "SO_rate", "HBP_rate"), selected = "ERA"),
              selectInput(inputId = "pitch_stat_2", label = "Stat 2",
                       choices = c("W", "L", "IP", "H", "ER", "HR", "BB", "SO", "BAOpp", "ERA",
                                   "HBP", "W_rate", "L_rate", "CG_rate","HR_rate",
                                   "WHIP", "SO_rate","HBP_rate"), selected = "HR_rate")),
           tags$h5("Legend:"), 
           "W = Wins", tags$br(),
           "L = Losses", tags$br(),
           "IP = Innings Pitched", tags$br(),
           "H = Hits Allowed", tags$br(),
           "ER = Earned Runs", tags$br(),
           "HR = Homeruns Allowed", tags$br(),
           "BB = Walks Allowed", tags$br(),
           "SO = Strikeouts", tags$br(),
           "BAOpp = Opponents' Batting Average", tags$br(),
           "ERA = Earned Run Average", tags$br(),
           "HBP = Hit by Pitches", tags$br(),
           "W_rate = Wins per Game", tags$br(),
           "L_rate = Losses per Game", tags$br(),
           "CG_rate = Complete games per Game", tags$br(),
           "HR_rate = Homeruns Allowed per Inning", tags$br(),
           "WHIP = Walks + Hits per Innings Pitched", tags$br(),
           "SO_rate = Strikeouts per Inning", tags$br(),
           "HBP_rate = Hit by Pitches per Inning"
           ),
      column(
        8, mainPanel(plotOutput("pitching"))
        )
      )
      ),
  
  # Hitter Comparison panel
  
  tabPanel(
    title = "Hitter Comparison",
    fluidRow(
      column(
        4,
        wellPanel(
          tags$h5("Pick Two Hitters to Compare"),
          selectizeInput(inputId = "hitter1", label = "Hitter 1",
                         choices = total_data_bat$last_first, selected = "Bonds, Barry"),
          selectizeInput(inputId = "hitter2", label = "Hitter 2",
                         choices = total_data_bat$last_first, selected = "Aaron, Hank"),
          tags$h5("Pick a Stat to Compare By"),
          selectInput(inputId = "comp_bat_stat", label = "Stat",
                        choices = c("AVG", "R", "H", "X2B", "X3B", "HR", "RBI", "SB", "BB", "SO", "HBP", 
                                    "GIDP", "R_rate", "X2B_rate", "X3B_rate", "HR_rate", "RBI_rate",
                                    "SB_rate", "BB_rate", "SO_rate", "HBP_rate", "GIDP_rate"), selected = "HR")
          ),
        tags$h5("Notes: "),
        "This data is from 1939-present, so anyone who played before 1939 will not appear 
        and those who played before 1939 and past, it will only show the total stats from
        1939 on.",
        tags$br(), tags$br(),
        "Also, players with the same exact name (e.g. Vladimir Guerrero, Ken Griffey) 
        will come up as the same person because of the nature of the dataset."
      ),
      column(
        8,
        plotOutput("bat_barplot")
      )
    )
    ),
  
  # Pitcher Comparison panel
  
  tabPanel(
    title = "Pitcher Comparison",
    fluidRow(
      column(
        4,
        wellPanel(
          tags$h5("Pick Two Pitchers to Compare"),
          selectizeInput(inputId = "pitcher1", label = "Pitcher 1",
                         choices = total_data_pitch$last_first, selected = "Johnson, Randy"),
          selectizeInput(inputId = "pitcher2", label = "Pitcher 2",
                         choices = total_data_pitch$last_first, selected = "Clemens, Roger"),
          tags$h5("Pick a Stat to Compare By"),
          selectInput(inputId = "comp_pitch_stat", label = "Stat 2",
                      choices = c("W", "L", "IP", "H", "ER", "HR", "BB", "SO", "BAOpp", "ERA",
                                  "HBP", "W_rate", "L_rate", "CG_rate","HR_rate",
                                  "WHIP", "SO_rate","HBP_rate"), selected = "SO")
        ),
        tags$h5("Notes: "),
        "This data is from 1939-present, so anyone who played before 1939 will not appear 
        and those who played before 1939 and past, it will only show the total stats from
        1939 on.",
        tags$br(), tags$br(),
        "Also, players with the same exact name (e.g. Vladimir Guerrero, Ken Griffey) 
        will come up as the same person because of the nature of the dataset."
        ),
      column(
        8,
        plotOutput("pitch_barplot")
      )
    )
  )
)
  
server <- function(input, output) {
  
  # Hitting Output
  
  output$batting <- renderPlot({
    points <- data.frame(bat_stats[input$bat_stat_1], bat_stats[input$bat_stat_2])
    plot(points, xlab = input$bat_stat_1, ylab = input$bat_stat_2, 
         main = c(input$bat_stat_1, " vs ", input$bat_stat_2), pch = 16)
    grid(col = "gray", lty = 2)
  })
  
  # Pitching Output
  
  output$pitching <- renderPlot({
    points <- data.frame(pitch_stats[input$pitch_stat_1], pitch_stats[input$pitch_stat_2])
    plot(points, xlab = input$pitch_stat_1, ylab = input$pitch_stat_2, 
         main = c(input$pitch_stat_1, " vs ", input$pitch_stat_2), pch = 16)
    grid(col = "gray", lty = 2)
  })
  
  # Hitter Comparison Output
  
  hitter_row1 <- reactive({which(total_data_bat == input$hitter1, arr.ind = TRUE)[1]})
  hitter_row2 <- reactive({which(total_data_bat == input$hitter2, arr.ind = TRUE)[1]})
  output$bat_barplot <- renderPlot({
    points <- c(as.numeric(total_data_bat[hitter_row1(),input$comp_bat_stat]),
                as.numeric(total_data_bat[hitter_row2(),input$comp_bat_stat]))
    bp <- barplot(points, col = c("gray", "gray"), 
                  main = c(input$comp_bat_stat),
                  names.arg = c(input$hitter1, input$hitter2), ylab = input$comp_bat_stat,
                  width = c(1, 1))
    text(bp, 0, round(points, 3), cex = 2, pos = 3)
  })
  
  # Pitcher Comparison Output
  
  pitcher_row1 <- reactive({which(total_data_pitch == input$pitcher1, arr.ind = TRUE)[1]})
  pitcher_row2 <- reactive({which(total_data_pitch == input$pitcher2, arr.ind = TRUE)[1]})
  output$pitch_barplot <- renderPlot({
    points <- c(as.numeric(total_data_pitch[pitcher_row1(),input$comp_pitch_stat]),
                as.numeric(total_data_pitch[pitcher_row2(),input$comp_pitch_stat]))
    bp <- barplot(points, col = c("gray", "gray"), 
            main = c(input$comp_pitch_stat),
            names.arg = c(input$pitcher1, input$pitcher2), ylab = input$comp_pitch_stat,
            width = c(1, 1))
    text(bp, 0, round(points, 3), cex = 2, pos = 3)
  })
}
shinyApp(ui = ui, server = server)







