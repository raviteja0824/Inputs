library(shiny)
library(shinydashboard)
library(tidyverse)
library(shiny)
library(shinyBS)
library(plotly)
library(ggplot2)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(RSQLite)
library(shinymanager)


players<-read.csv(url("https://www.dropbox.com/s/8gs2iefh14974xs/Players.csv?dl=1"))
external<-players%>% filter(Type != "Internal")
internal<-players%>% filter(Type != "External")
html_caption_str <- as.character(shiny::tags$b(align = 'c', style = "color: black; text-align: center; font-size: 20px;", "Rankings"))

inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"


# data.frame with credentials info
credentials <- data.frame(
  user = c("Analysis@stirlingalbion.info" ),
  password = c("Thebinos123"),
  # comment = c("alsace", "auvergne", "bretagne"), %>% 
  stringsAsFactors = FALSE
)

if (interactive()) {
ui<-secure_app(head_auth = tags$script(inactivity),
               fluidPage( style = "width: 1260; overflow-x: hidden;",
  img(src='Header.png', height = '130px', width = '1250px'),
 br(),
 br(),
  splitLayout( cellWidths =620,fluid = FALSE,style='padding-left:10px;',
               
    tags$div(
             tags$span(style = "color: red ;font-family: juventus palace;  font-style: Bold;font-size: 40px;", "Player A")),
    tags$div(
      tags$span(style = "color: blue ;font-family: juventus palace;  font-style: Bold;font-size: 40px;", "Player B"))),
   dashboardBody(useShinyjs(),
                 
                 br(),
                 
                 div(id = "form",
                     splitLayout( cellWidths = 150, fluid = FALSE,style='padding-left:100px;',
            selectInput("Type_2", "Type", choices = players$Type, width = 250),
           selectInput("Position_2", "Position", choices = NULL, width = 250),
          selectInput("Player_2", "Player Name", choices = NULL, width = 200),tags$head(tags$style(HTML("
                              .shiny-split-layout > div {
                                overflow-y: visible;
                                overflow-x: hidden;
                                font-size: 11px;
                              }
                              "))),
          
          selectInput("Type", "Type", choices = players$Type, width = 250),
          selectInput("Position", "Position", choices = NULL, width = 250),
        
          selectInput("Player", "Player Name", choices = NULL, width = 250),tags$head(tags$style(HTML("
                              .shiny-split-layout > div {
                                overflow: visible;
                                text-align: center;
                                color: Black;
                                border-color: Black;
                                
                              }
                              "))),
          
          tags$style(type='text/css', "#Position{background-color: white; height: 40px; border-color: black; width: 520px; position: relative;left: 3%}"))),
          br(),
          fixedRow(actionButton("resetAll", "Reset", width = 150), style='padding-left:580px;'),
    
  br(),
  br(),
  splitLayout(cellWidths = 630,fluid = FALSE,style='padding-left:40px;overflow-x: hidden;',
            mainPanel(
             plotlyOutput('radar_2', width = 500)
           ),
           mainPanel(
             plotlyOutput('radar', width = 500)
           )),
    splitLayout( cellWidths = 560,fluid = FALSE,style='padding-left:50px;overflow-x: hidden;',
    box(
             column(6, style='border: 1px solid red; width: 500px; height: 500px; text-align: center;', htmlOutput("data_2"), tags$head(tags$style("#data_2{color: Black;
                                 font-size: 10px;
                                 font-style: Bold;
                                 width: 500px; 
                                 max-width: 100%;
                                 padding: 6px 12px; 
                                overflow-x: hidden;
                                 white-space: pre-wrap;
                                 .form-control {
            border-radius: 4px 4px 4px 4px;
        }};
                                 
                                 
                                 }"
             ))), solidHeader = TRUE),
    box(
      column(6, offset = 4,align = "center",  style='border: 1px solid blue; width: 500px; height: 500px; text-align: center;', htmlOutput("data"), tags$head(tags$style("#data{color: Black;
                                 font-size: 10px;
                                 font-style: Bold;
                                 width: 500px; 
                                 max-width: 100%;
                                 padding: 6px 12px; 
                                overflow-x: hidden;
                                 white-space: pre-wrap;
                        
                                 
                                 
                                 }"
      ))), solidHeader = TRUE)),
  br(),
  br(),
  splitLayout(cellWidths = 643,fluid = FALSE,style='padding-left:50px;',
    box(
      column(6, caption = "Rankings",align = "center", style='border: 1px solid red; width: 500px; text-align: center;', tableOutput("ranking_table_2"), tags$head(tags$style("#ranking_table_2{color: Black;
                                 font-size: 10px;
                                 font-style: Bold;
                                 font-colour: Black;
                                 max-width: 100%;
                                 padding: 6px 12px; 
                                 white-space: pre-wrap;
                                 text-align: center;
              
                                 
                                 
                                 }"
      ))), solidHeader = TRUE),
    box(
      column(6, caption = "Rankings", style='border: 1px solid blue; width: 500px; text-align: center;', tableOutput("ranking_table"), tags$head(tags$style("#ranking_table{color: Black;
                                 font-size: 10px;
                                 font-style: Bold;
                                 font-colour: Black;
                                 max-width: 100%;
                                 padding: 6px 12px; 
                                 white-space: pre-wrap;
                                 text-align: center;
                             
                                 
                                 
                                 }"
      ))), solidHeader = TRUE)),
  
  tags$style(HTML("
      body {
        background-color: white;
        color: white;
      }
      h1 {
        font-family: 'Yusei Magic', sans-serif;
      }
      h3 {
        font-family: 'juventus palace', sans-serif; color: black;
                        text-align: center; 
                                 font-style: Bold;
                                 font-colour: Black;
                                 max-width: 100%;
                                
                                 padding: 6px 12px; 
                                 white-space: pre-wrap;
                                 text-align: center;
                                 line-height: 45px;
      }
     .shiny-input-container {
       color: #474747;
      }.shiny-split-layout > div {
                                display: inline-block;
                              }"))
  ))
)
  



server<-shinyServer(function(input,output, session){
  result_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  output$res_auth <- renderPrint({
    reactiveValuesToList(result_auth)
  })
  observeEvent(
    input$Type,
    updateSelectInput(session, "Position", "Position", selected = "", choice = players$Position[players$Type == input$Type])
  )
 
  observeEvent(
    input$Position,
    updateSelectInput(session, "Player", "Player", choice = players$Player.Name[(players$Position == input$Position)&(players$Type == input$Type)])
  )
  observeEvent(input$resetAll, {
    reset("form")
  })
  output$radar <-renderPlotly({
    if(input$Type == "External"){
    if (input$Position=="Full Back"){
      player<-players%>%filter(Player.Name == input$Player )
      r<- player %>%select(Physique,	Speed...Agility,	Heading.Ability,	Passing.Ability,	Work.Rate,	Defensive.Qualities.1.v.1,	Covering,	Getting.Forward...Delivery,	Recovery,	Ball.Control.Confidence,	Set.Play.Roles) 
       nms <- names(r)
    #code to plot the radar
    fig <- plot_ly(
      type = 'scatterpolar',
      r = as.matrix(r),
      theta = nms,
      fill = 'toself',
      fillcolor='rgba(0, 0, 255, 0.7)',
      mode = 'markers'
    )
    fig <- fig %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,10),width = 200, height = 200
          ),
          angularaxis = list(tickfont = list(size = 8))
        ),annotations =list(showarrow = FALSE,
                             text=player$Overall.Score, 
                             colour = "white",
                             xanchor='centre',   
                             yanchor='centre', 
                            bgcolor="rgb(255, 255, 255)",
                             font=list(size=30, color = "rgba(0, 0, 255, 0.7)" )),
        showlegend = F
      )
    } 
    else if (input$Position=="Centre Back"){
      player<-players%>%filter(Player.Name == input$Player)
      r<- player %>%select(Physique,	Speed...Agility,	Heading.Ability,	Passing.Ability,	Work.Rate,	Defensive.Qualities.1.v.1,	Recovery,	Comfort.on.the.Ball,	Organisation,	Set.Play.Roles,	Temperament) 
      nms <- names(r)
      
      #code to plot the radar
      fig <- plot_ly(
        type = 'scatterpolar',
        r = as.matrix(r),
        theta = nms,
        fill = 'toself',
        fillcolor = 'rgba(0, 0, 255, 0.7)',
        mode = 'markers'
      ) 
      fig <- fig %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = T,
              range = c(0,10)
            ),
            angularaxis = list(tickfont = list(size = 8))
          ),annotations =list(showarrow = FALSE,
                                                                            text=player$Overall.Score, 
                                                                            colour = "white",
                                                                            xanchor='centre',   
                                                                            yanchor='centre', 
                                                                            bgcolor="rgb(255, 255, 255)",
                                                                            font=list(size=30, color = "rgba(0, 0, 255, 0.7)" )),
          
          showlegend = F
        )
    }
    else if (input$Position=="Striker"){
    player<-players%>%filter(Player.Name == input$Player)
    r<- player %>%select(Physique, Speed...Agility, Heading.Ability, Passing.Ability, Work.Rate, Ball.Control.Confidence, Shooting..Power.Accuracy., Link.Play, Movement, Game.Understanding) 
    nms <- names(r)
    
    #code to plot the radar
    fig <- plot_ly(
      type = 'scatterpolar',
      r = as.matrix(r),
      theta = nms,
      fill = 'toself',
      fillcolor = 'rgba(0, 0, 255, 0.7)',
      mode = 'markers'
    ) 
    fig <- fig %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,10)
          ),
          angularaxis = list(tickfont = list(size = 8))
        ),annotations =list(showarrow = FALSE,
                             text=player$Overall.Score, 
                             colour = "white",
                             xanchor='centre',   
                             yanchor='centre', 
                             bgcolor="rgb(255, 255, 255)",
                             font=list(size=30, color = "rgba(0, 0, 255, 0.7)" )),
        showlegend = F
      )
    }
    else if (input$Position=="Wide Player"){
     player<-players%>%filter(Player.Name == input$Player)
      r<- player %>%select(Physique, Speed...Agility, Passing.Ability, Work.Rate, Shooting..Power.Accuracy., Crossing.Ability, Defensive.ability, Comfort.on.the.Ball, Aggression, Goal.Threat, Decision.Making) 
      
      nms <- names(r)
      
      #code to plot the radar
      fig <- plot_ly(
        type = 'scatterpolar',
        r = as.matrix(r),
        theta = nms,
        fill = 'toself',
        fillcolor = 'rgba(0, 0, 255, 0.7)',
        mode = 'markers'
      ) 
      fig <- fig %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = T,
              range = c(0,10)
            ),
            angularaxis = list(tickfont = list(size = 8))
          ),annotations =list(showarrow = FALSE,
                               text=player$Overall.Score, 
                               colour = "white",
                               xanchor='centre',   
                               yanchor='centre', 
                               bgcolor="rgb(255, 255, 255)",
                               font=list(size=30, color = "rgba(0, 0, 255, 0.7)" )),
          showlegend = F
        )
    }
    else if (input$Position=="Central Midfield"){
      player<-players%>%filter(Player.Name == input$Player)
      r<- player %>%select(Physique, Speed...Agility, Passing.Ability, Work.Rate, Defensive.Qualities.1.v.1, Movement,Tactical.Awareness, Ball.Control, Attacking.Qualities, Temperament, Heading.Ability) 
      nms <- names(r)
      
      #code to plot the radar
      fig <- plot_ly(
        type = 'scatterpolar',
        r = as.matrix(r),
        theta = nms,
        fill = 'toself',
        fillcolor = 'rgba(0, 0, 255, 0.7)',
        mode = 'markers'
      ) 
      fig <- fig %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = T,
              range = c(0,10)
            ),
            angularaxis = list(tickfont = list(size = 8))
          ),annotations =list(showarrow = FALSE,
                               text=player$Overall.Score, 
                               colour = "white",
                               xanchor='centre',   
                               yanchor='centre', 
                               bgcolor="rgb(255, 255, 255)",
                               font=list(size=30, color = "rgba(0, 0, 255, 0.7)" )),
          showlegend = F
        )
    }
    else if (input$Position=="Goalkeeper"){
     player<-players%>%filter(Player.Name == input$Player)
      r<- player %>%select(Physique, Speed...Agility, Communication, Dealing.with.Crosses, General.Handling, Decision.Making,Distribution, One.vs.One.Blocking, Shape, Positional.Play.on.Shots, Shot.Stopping) 
      nms <- names(r)
      
      #code to plot the radar
      fig <- plot_ly(
        type = 'scatterpolar',
        r = as.matrix(r),
        theta = nms,
        fill = 'toself',
        fillcolor = 'rgba(0, 0, 255, 0.7)',
        mode = 'markers'
      ) 
      fig <- fig %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = T,
              range = c(0,10)
            ),
            annotations = list(text = player$Overall.Score),
            angularaxis = list(tickfont = list(size = 8))
            ),annotations =list(showarrow = FALSE,
                               text=player$Overall.Score, 
                               colour = "white",
                               xanchor='centre',   
                               yanchor='centre', 
                               bgcolor="rgb(255, 255, 255)",
                               font=list(size=30, color = "rgba(0, 0, 255, 0.7)" )),
          showlegend = F
        )
    }
      
    }else if(input$Type == "Internal"){
      
      if (input$Position=="Full Back"){
        player <-filter(players, (Player.Name == input$Player)&(Position=="Full Back"))
        #r <- map_dbl(player[c("Physique",	"Speed...Agility",	"Heading.Ability",	"Passing.Ability",	"Work.Rate",	"Defensive.Qualities.1.v.1",	"Covering",	"Getting.Forward...Delivery",	"Recovery",	"Ball.Control.Confidence",	"Set.Play.Roles")], ~.x)
        #player<-internal%>%filter(Player.Name == input$Player)
        r<- player %>%select(Physique,	Speed...Agility,	Heading.Ability,	Passing.Ability,	Work.Rate,	Defensive.Qualities.1.v.1,	Covering,	Getting.Forward...Delivery,	Recovery,	Ball.Control.Confidence,	Set.Play.Roles) 
        nms <- names(r)
        text<-
          #code to plot the radar
          fig <- plot_ly(
            type = 'scatterpolar',
            r = as.matrix(r),
            theta = nms,
            fill = 'toself',
            fillcolor='rgba(0, 0, 255, 0.7)',
            mode = 'markers'
            
          )
        fig <- fig %>%
          layout(
            polar = list(
              radialaxis = list(
                visible = T,
                range = c(0,10)
              ),
              angularaxis = list(tickfont = list(size = 8))
              ),annotations =list(showarrow = FALSE,
                                 text=player$Overall.Score, 
                                 colour = "white",
                                 xanchor='centre',   
                                 yanchor='centre', 
                                 bgcolor="rgb(255, 255, 255)",
                                 font=list(size=30, color = "rgba(0, 0, 255, 0.7)" )),
            showlegend = F
          )
      } 
      else if (input$Position=="Centre Back"){
        player <-filter(players, (Player.Name == input$Player)&(Position=="Centre Back"))
        #r <- map_dbl(player[c("Physique",	"Speed...Agility",	"Heading.Ability",	"Passing.Ability",	"Work.Rate",	"Defensive.Qualities.1.v.1",	"Recovery",	"Comfort.on.the.Ball",	"Organisation",	"Set.Play.Roles",	"Temperament")], ~.x)
        #player<-internal%>%filter(Player.Name == input$Player)
        r<- player %>%select(Physique,	Speed...Agility,	Heading.Ability,	Passing.Ability,	Work.Rate,	Defensive.Qualities.1.v.1,	Recovery,	Comfort.on.the.Ball,	Organisation,	Set.Play.Roles,	Temperament) 
        nms <- names(r)
        
        #code to plot the radar
        fig <- plot_ly(
          type = 'scatterpolar',
          r = as.matrix(r),
          theta = nms,
          fill = 'toself',
          fillcolor = 'rgba(0, 0, 255, 0.7)',
          mode = 'markers'
        ) 
        fig <- fig %>%
          layout(
            polar = list(
              radialaxis = list(
                visible = T,
                range = c(0,10)
              ),
              angularaxis = list(tickfont = list(size = 8))
              ),annotations =list(showarrow = FALSE,
                                 text=player$Overall.Score, 
                                 colour = "white",
                                 xanchor='centre',   
                                 yanchor='centre', 
                                 bgcolor="rgb(255, 255, 255)",
                                 font=list(size=30, color = "rgba(0, 0, 255, 0.7)" )),
            showlegend = F
          )
      }
      else if (input$Position=="Striker"){
        player <-filter(players, (Player.Name == input$Player)&(Position=="Striker"))
        r<- player %>%select(Physique, Speed...Agility, Heading.Ability, Passing.Ability, Work.Rate, Ball.Control.Confidence, Shooting..Power.Accuracy., Link.Play, Movement, Game.Understanding) 
        nms <- names(r)
        
        #code to plot the radar
        fig <- plot_ly(
          type = 'scatterpolar',
          r = as.matrix(r),
          theta = nms,
          fill = 'toself',
          fillcolor = 'rgba(0, 0, 255, 0.7)',
          mode = 'markers'
        ) 
        fig <- fig %>%
          layout(
            polar = list(
              radialaxis = list(
                visible = T,
                range = c(0,10)
              ),
              angularaxis = list(tickfont = list(size = 8)),
              ),annotations =list(showarrow = FALSE,
                                 text=player$Overall.Score, 
                                 colour = "white",
                                 xanchor='centre',   
                                 yanchor='centre', 
                                 bgcolor="rgb(255, 255, 255)",
                                 font=list(size=30, color = "rgba(0, 0, 255, 0.7)" )),
            showlegend = F
          )
      }
      else if (input$Position=="Wide Player"){
        player <- filter(players, (Player.Name == input$Player)&(Position=="Wide Player"))
        # r <- map_dbl(player[c("Physique", "Speed...Agility", "Passing.Ability", "Work.Rate", "Shooting..Power.Accuracy.", "Crossing.Ability", "Defensive.ability", "Comfort.on.the.Ball", "Aggression", "Goal.Threat", "Decision.Making")], ~.x)
        #player<-internal%>%filter(Player.Name == input$Player)
        r<- player %>%select(Physique, Speed...Agility, Passing.Ability, Work.Rate, Shooting..Power.Accuracy., Crossing.Ability, Defensive.ability, Comfort.on.the.Ball, Aggression, Goal.Threat, Decision.Making) 
        nms <- names(r)
        
        #code to plot the radar
        fig <- plot_ly(
          type = 'scatterpolar',
          r = as.matrix(r),
          theta = nms,
          fill = 'toself',
          fillcolor = 'rgba(0, 0, 255, 0.7)',
          mode = 'markers'
        ) 
        fig <- fig %>%
          layout(
            polar = list(
              radialaxis = list(
                visible = T,
                range = c(0,10)
              ),
              angularaxis = list(tickfont = list(size = 8))
              ),annotations =list(showarrow = FALSE,
                                 text=player$Overall.Score, 
                                 colour = "white",
                                 xanchor='centre',   
                                 yanchor='centre', 
                                 bgcolor="rgb(255, 255, 255)",
                                 font=list(size=30, color = "rgba(0, 0, 255, 0.7)" )),
            showlegend = F
          )
      }
      else if (input$Position=="Central Midfield"){
        player <- filter(players, (Player.Name == input$Player)&(Position=="Central Midfield"))
        #  r <- map_dbl(player[c("Physique", "Speed...Agility", "Passing.Ability", "Work.Rate", "Defensive.Qualities.1.v.1", "Movement","Tactical.Awareness", "Ball.Control", "Attacking.Qualities", "Temperament", "Heading.Ability")], ~.x)
        #player<-internal%>%filter(Player.Name == input$Player)
        r<- player %>%select(Physique, Speed...Agility, Passing.Ability, Work.Rate, Defensive.Qualities.1.v.1, Movement,Tactical.Awareness, Ball.Control, Attacking.Qualities, Temperament, Heading.Ability) 
        nms <- names(r)
        
        #code to plot the radar
        fig <- plot_ly(
          type = 'scatterpolar',
          r = as.matrix(r),
          theta = nms,
          fill = 'toself',
          fillcolor = 'rgba(0, 0, 255, 0.7)',
          mode = 'markers'
        ) 
        fig <- fig %>%
          layout(
            polar = list(
              radialaxis = list(
                visible = T,
                range = c(0,10)
              ),
              angularaxis = list(tickfont = list(size = 8))
              ),annotations =list(showarrow = FALSE,
                                 text=player$Overall.Score, 
                                 colour = "white",
                                 xanchor='centre',   
                                 yanchor='centre', 
                                 bgcolor="rgb(255, 255, 255)",
                                 font=list(size=30, color = "rgba(0, 0, 255, 0.7)" )),
            showlegend = F
          )
      }
      else if (input$Position=="Goalkeeper"){
        player <- filter(players, (Player.Name == input$Player)&(Position=="Goalkeeper"))
        #r <- map_dbl(player[c("Physique", "Speed...Agility", "Communication", "Dealing.with.Crosses", "General.Handling", "Decision.Making","Distribution", "One.vs.One.Blocking", "Shape", "Positional.Play.on.Shots", "Shot.Stopping")], ~.x)
        #player<-internal%>%filter(Player.Name == input$Player)
        r<- player %>%select(Physique, Speed...Agility, Communication, Dealing.with.Crosses, General.Handling, Decision.Making,Distribution, One.vs.One.Blocking, Shape, Positional.Play.on.Shots, Shot.Stopping) 
        nms <- names(r)
        
        #code to plot the radar
        fig <- plot_ly(
          type = 'scatterpolar',
          r = as.matrix(r),
          theta = nms,
          fill = 'toself',
          fillcolor = 'rgba(0, 0, 255, 0.7)',
          mode = 'markers'
        ) 
        fig <- fig %>%
          layout(
            polar = list(
              radialaxis = list(
                visible = T,
                range = c(0,10)
              ),
              angularaxis = list(tickfont = list(size = 8))
              ),annotations =list(showarrow = FALSE,
                                 text=player$Overall.Score, 
                                 colour = "white",
                                 xanchor='centre',   
                                 yanchor='centre', 
                                 bgcolor="rgb(255, 255, 255)",
                                 font=list(size=30, color = "rgba(0, 0, 255, 0.7)" )),
            showlegend = F
          )
      }
    }
  })
  
  
  output$data <-renderText({
    if (input$Position=="Full Back"){
      player <-filter(players, (Player.Name == input$Player)&(Position=="Full Back"))
      paste("<b>","Player Profile" ,"</b>","<br/>",
            "Age: ", player$Age,"<br/>", 
            "Scouted by:", player$Scout,"<br/>",
            "Parent Club: ", player$Parent.Club, "<br/>", "<br/>",
            "<b>","Physique:", "</b>",str_to_sentence(player$Physique_Comments), "<br/>",
            "<b>","Speed & Agility:","</b>",str_to_sentence(player$Speed_Comments), "<br/>",
            "<b>","Heading Ability:","</b>",str_to_sentence(player$Heading_Comments), "<br/>",
            "<b>","Passing Ability S&L:","</b>", str_to_sentence(player$Passing_Comments), "<br/>",
            "<b>","Work Rate:","</b>", str_to_sentence(player$Work_Rate_Comments), "<br/>",
            "<b>","Defending Including 1 v 1:","</b>",str_to_sentence(player$Defensive_1_v_1_Comments), "<br/>",
            "<b>","Covering:","</b>", str_to_sentence(player$Covering_Comments), "<br/>",
            "<b>","Getting forward & Delivery:","</b>", str_to_sentence(player$Getting_forward_Comments), "<br/>",
            "<b>","Recovery:","</b>", str_to_sentence(player$Recovery_Comments), "<br/>",
            "<b>","Ball Control:","</b>", str_to_sentence(player$Ball_Control_Comments), "<br/>",
            "<b>","Set Play Roles:","</b>", str_to_sentence(player$Set_Play_Roles_Comments), "<br/>","<br/>","<br/>",
            "<b>", "Overall Comments & Recommendations","</b>","<br/>",
            str_to_sentence(player$Overall_Comments)
      )
        }
    else if (input$Position=="Centre Back"){
      player <-filter(players, (Player.Name == input$Player)&(Position=="Centre Back"))
      paste("<b>","Player Profile" ,"</b>","<br/>",
            "Age: ", player$Age,"<br/>", 
            "Scouted by:", player$Scout,"<br/>",
            "Parent Club: ", player$Parent.Club, "<br/>", "<br/>",
            "<b>","Physique/Power:", "</b>",str_to_sentence(player$Physique_Comments), "<br/>",
            "<b>","Speed & Agility:","</b>",str_to_sentence(player$Speed_Comments), "<br/>",
            "<b>","Heading Ability:","</b>",str_to_sentence(player$Heading_Comments), "<br/>",
            "<b>","Passing Ability S&L:","</b>", str_to_sentence(player$Passing_Comments), "<br/>",
            "<b>","Work Rate:","</b>", str_to_sentence(player$Work_Rate_Comments), "<br/>",
            "<b>","Defensive Qualities:","</b>", str_to_sentence(player$Defensive_1_v_1_Comments), "<br/>",
            "<b>","Tracking/Recovering:","</b>", str_to_sentence(player$Recovery_Comments), "<br/>",
            "<b>","Comfort on the Ball:","</b>", str_to_sentence(player$Comfort_on_the_Ball_Comments), "<br/>",
            "<b>","Organisation:","</b>", str_to_sentence(player$Organisation_Comments), "<br/>",
            "<b>","Set Play Roles:","</b>", str_to_sentence(player$Set_Play_Roles_Comments), "<br/>",
            "<b>","Temperament:","</b>", str_to_sentence(player$Temperament_Comments), "<br/>","<br/>","<br/>",
            "<b>", "Overall Comments & Recommendations","</b>","<br/>",
            str_to_sentence(player$Overall_Comments)
      )
      }
   else if (input$Position=="Striker"){
     player <-filter(players, (Player.Name == input$Player)&(Position=="Striker"))
     paste("<b>","Player Profile" ,"</b>","<br/>",
           "Age: ", player$Age,"<br/>", 
           "Scouted by:", player$Scout,"<br/>",
           "Parent Club: ", player$Parent.Club, "<br/>", "<br/>",
           "<b>","Physique:", "</b>",str_to_sentence(player$Physique_Comments), "<br/>",
           "<b>","Speed & Agility:","</b>",str_to_sentence(player$Speed_Comments), "<br/>",
           "<b>","Passing Ability S&L:","</b>", str_to_sentence(player$Passing_Comments), "<br/>",
           "<b>","Work Rate:","</b>", str_to_sentence(player$Work_Rate_Comments), "<br/>",
           "<b>","Shooting (Power/Accuracy):","</b>",str_to_sentence(player$Shooting_Comments), "<br/>",
           "<b>","Heading Ability:","</b>", str_to_sentence(player$Heading_Comments), "<br/>",
           "<b>","Ball Control:","</b>", str_to_sentence(player$Ball_Control_Comments), "<br/>",
           "<b>","Link Play:","</b>", str_to_sentence(player$Link_Comments), "<br/>",
           "<b>","Movement:","</b>", str_to_sentence(player$Movement_Comments), "<br/>",
           "<b>","Game Understanding:","</b>", str_to_sentence(player$Game_Understanding_Comments), "<br/>",
           "<b>","Set Play Roles:","</b>", str_to_sentence(player$Set_Play_Roles_Comments), "<br/>","<br/>","<br/>",
           "<b>", "Overall Comments & Recommendations","</b>","<br/>",
           str_to_sentence(player$Overall_Comments)
     )
    }
    else if (input$Position=="Wide Player"){
      player <-filter(players, (Player.Name == input$Player)&(Position=="Wide Player"))
      paste("<b>","Player Profile" ,"</b>","<br/>",
            "Age: ", player$Age,"<br/>", 
            "Scouted by:", player$Scout,"<br/>",
            "Parent Club: ", player$Parent.Club, "<br/>", "<br/>",
            "<b>","Physique:", "</b>",str_to_sentence(player$Physique_Comments), "<br/>",
            "<b>","Speed & Agility:","</b>",str_to_sentence(player$Speed_Comments), "<br/>",
            "<b>","Passing Ability S&L:","</b>", str_to_sentence(player$Passing_Comments), "<br/>",
            "<b>","Work Rate:","</b>", str_to_sentence(player$Work_Rate_Comments), "<br/>",
            "<b>","Crossing Ability:","</b>",str_to_sentence(player$Crossing_Ability_Comments), "<br/>",
            "<b>","Shooting Ability:","</b>", str_to_sentence(player$Shooting_Comments), "<br/>",
            "<b>","Defensive Abilities:","</b>", str_to_sentence(player$Defensive_Ability_Comments), "<br/>",
            "<b>","Comfort on the Ball:","</b>", str_to_sentence(player$Comfort_on_the_Ball_Comments), "<br/>",
            "<b>","Aggression:","</b>", str_to_sentence(player$Aggression_Comments), "<br/>",
            "<b>","Goal Threat:","</b>", str_to_sentence(player$Goal_Threat_Comments), "<br/>",
            "<b>","Decision Making:","</b>", str_to_sentence(player$Decision_Making_Comments), "<br/>","<br/>","<br/>",
            "<b>", "Overall Comments & Recommendations","</b>","<br/>",
            str_to_sentence(player$Overall_Comments)
      )
    }
    else if (input$Position=="Central Midfield"){
      player <-filter(players, (Player.Name == input$Player)&(Position=="Central Midfield"))
            paste("<b>","Player Profile" ,"</b>","<br/>",
                  "Age: ", player$Age,"<br/>", 
                  "Scouted by:", player$Scout,"<br/>",
                  "Parent Club: ", player$Parent.Club, "<br/>", "<br/>",
                  "<b>","Physique:", "</b>",str_to_sentence(player$Physique_Comments), "<br/>",
                  "<b>","Speed & Agility:","</b>",str_to_sentence(player$Speed_Comments), "<br/>",
                  "<b>","Passing Ability S&L:","</b>", str_to_sentence(player$Passing_Comments), "<br/>",
                  "<b>","Work Rate:","</b>", str_to_sentence(player$Work_Rate_Comments), "<br/>",
                  "<b>","Tactical Awareness:","</b>",str_to_sentence(player$Comments_Tactical), "<br/>",
                  "<b>","Heading Ability:","</b>", str_to_sentence(player$Heading_Comments), "<br/>",
                  "<b>","Ball Control:","</b>", str_to_sentence(player$Comments_Ball_Control), "<br/>",
                  "<b>","Defensive Qualities 1 v 1:","</b>", str_to_sentence(player$Defensive_1_v_1_Comments), "<br/>",
                  "<b>","Attacking Qualities:","</b>", str_to_sentence(player$Comments_Attacking), "<br/>",
                  "<b>","Movement:","</b>", str_to_sentence(player$Movement_Comments), "<br/>",
                  "<b>","Temperament:","</b>", str_to_sentence(player$Temperament_Comments), "<br/>","<br/>","<br/>",
                  "<b>", "Overall Comments & Recommendations","</b>","<br/>",
                  str_to_sentence(player$Overall_Comments))
    }
    else if (input$Position=="Goalkeeper"){
      player <-filter(players, (Player.Name == input$Player)&(Position=="Goalkeeper"))
      paste("<b>","Player Profile" ,"</b>","<br/>",
            "Age: ", player$Age,"<br/>", 
            "Scouted by:", player$Scout,"<br/>",
            "Parent Club: ", player$Parent.Club, "<br/>", "<br/>",
            "<b>","Physique:", "</b>",str_to_sentence(player$Physique_Comments), "<br/>",
            "<b>","Speed & Agility:","</b>",str_to_sentence(player$Speed_Comments), "<br/>",
            "<b>","Communication:","</b>", str_to_sentence(player$Communication_Comments), "<br/>",
            "<b>","Dealing with Crosses:","</b>", str_to_sentence(player$Dealing_with_Crosses_Comments), "<br/>",
            "<b>","General Handling:","</b>",str_to_sentence(player$General_Handling_Comments), "<br/>",
            "<b>","Decision Making:","</b>", str_to_sentence(player$Decision_Making_Comments), "<br/>",
            "<b>","Distribution:","</b>", str_to_sentence(player$Distribution_Comments), "<br/>",
            "<b>","One vs One Blocking:","</b>", str_to_sentence(player$One_Vs_One_Comments), "<br/>",
            "<b>","Shape:","</b>", str_to_sentence(player$Shape_Comments), "<br/>",
            "<b>","Positional Play on Shots:","</b>", str_to_sentence(player$Positional_Comments), "<br/>",
            "<b>","Shot Stopping:","</b>", str_to_sentence(player$Shot_Stopping_Comments), "<br/>","<br/>","<br/>",
            "<b>", "Overall Comments & Recommendations","</b>","<br/>",
            str_to_sentence(player$Overall_Comments)
      )
      
    }
  })
  
  observeEvent(
    input$Type_2,
    updateSelectInput(session, "Position_2", "Position",selected = "", choice = players$Position[(players$Type == input$Type_2)])
  )
  
  observeEvent(
    input$Position_2,
    updateSelectInput(session, "Player_2", "Player", choice = players$Player.Name[(players$Position == input$Position_2)&(players$Type == input$Type_2)])
  )
  output$radar_2 <-renderPlotly({
    if(input$Type_2 == "External"){
    if (input$Position_2=="Full Back"){
      player<-players%>%filter(Player.Name == input$Player_2)
      r<- player %>%select(Physique,	Speed...Agility,	Heading.Ability,	Passing.Ability,	Work.Rate,	Defensive.Qualities.1.v.1,	Covering,	Getting.Forward...Delivery,	Recovery,	Ball.Control.Confidence,	Set.Play.Roles) 
      nms <- names(r)
      #code to plot the radar
      fig <- plot_ly(
        type = 'scatterpolar',
        r = as.matrix(r),
        theta = nms,
        fill = 'toself',
        fillcolor='rgba(255, 0, 0, 0.6)',
        mode = 'markers'
      )
      fig <- fig %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = T,
              range = c(0,10),width = 200, height = 200
            ),
            angularaxis = list(tickfont = list(size = 8))
          ),annotations =list(showarrow = FALSE,
                              text=player$Overall.Score, 
                              colour = "white",
                              xanchor='centre',   
                              yanchor='centre', 
                              bgcolor="rgb(255, 255, 255)",
                              font=list(size=30, color = 'rgba(255, 0, 0, 0.6)' )),
          showlegend = F
        )
    } 
    else if (input$Position_2=="Centre Back"){
      player<-players%>%filter(Player.Name == input$Player_2)
      r<- player %>%select(Physique,	Speed...Agility,	Heading.Ability,	Passing.Ability,	Work.Rate,	Defensive.Qualities.1.v.1,	Recovery,	Comfort.on.the.Ball,	Organisation,	Set.Play.Roles,	Temperament) 
      nms <- names(r)
      
      #code to plot the radar
      fig <- plot_ly(
        type = 'scatterpolar',
        r = as.matrix(r),
        theta = nms,
        fill = 'toself',
        fillcolor = 'rgba(255, 0, 0, 0.6)',
        mode = 'markers'
      ) 
      fig <- fig %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = T,
              range = c(0,10)
            ),
            angularaxis = list(tickfont = list(size = 8)),
          ),annotations =list(showarrow = FALSE,
                              text=player$Overall.Score, 
                              colour = "white",
                              xanchor='centre',   
                              yanchor='centre', 
                              bgcolor="rgb(255, 255, 255)",
                              font=list(size=30, color = 'rgba(255, 0, 0, 0.6)' )),
          
          showlegend = F
        )
    }
    else if (input$Position_2=="Striker"){
      player<-players%>%filter(Player.Name == input$Player_2)
      r<- player %>%select(Physique, Speed...Agility, Heading.Ability, Passing.Ability, Work.Rate, Ball.Control.Confidence, Shooting..Power.Accuracy., Link.Play, Movement, Game.Understanding) 
      nms <- names(r)
      
      #code to plot the radar
      fig <- plot_ly(
        type = 'scatterpolar',
        r = as.matrix(r),
        theta = nms,
        fill = 'toself',
        fillcolor = 'rgba(255, 0, 0, 0.6)',
        mode = 'markers'
      ) 
      fig <- fig %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = T,
              range = c(0,10)
            ),
            angularaxis = list(tickfont = list(size = 8))
          ),annotations =list(showarrow = FALSE,
                              text=player$Overall.Score, 
                              colour = "white",
                              xanchor='centre',   
                              yanchor='centre', 
                              bgcolor="rgb(255, 255, 255)",
                              font=list(size=30, color = 'rgba(255, 0, 0, 0.6)' )),
          showlegend = F
        )
    }
    else if (input$Position_2=="Wide Player"){
      player<-players%>%filter(Player.Name == input$Player_2)
      r<- player %>%select(Physique, Speed...Agility, Passing.Ability, Work.Rate, Shooting..Power.Accuracy., Crossing.Ability, Defensive.ability, Comfort.on.the.Ball, Aggression, Goal.Threat, Decision.Making) 
      
      nms <- names(r)
      
      #code to plot the radar
      fig <- plot_ly(
        type = 'scatterpolar',
        r = as.matrix(r),
        theta = nms,
        fill = 'toself',
        fillcolor = 'rgba(255, 0, 0, 0.6)',
        mode = 'markers'
      ) 
      fig <- fig %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = T,
              range = c(0,10)
            ),
            angularaxis = list(tickfont = list(size = 8))
          ),annotations =list(showarrow = FALSE,
                              text=player$Overall.Score, 
                              colour = "white",
                              xanchor='centre',   
                              yanchor='centre', 
                              bgcolor="rgb(255, 255, 255)",
                              font=list(size=30, color = 'rgba(255, 0, 0, 0.6)')),
          showlegend = F
        )
    }
    else if (input$Position_2=="Central Midfield"){
      player<-players%>%filter(Player.Name == input$Player_2)
      r<- player %>%select(Physique, Speed...Agility, Passing.Ability, Work.Rate, Defensive.Qualities.1.v.1, Movement,Tactical.Awareness, Ball.Control, Attacking.Qualities, Temperament, Heading.Ability) 
      nms <- names(r)
      
      #code to plot the radar
      fig <- plot_ly(
        type = 'scatterpolar',
        r = as.matrix(r),
        theta = nms,
        fill = 'toself',
        fillcolor = 'rgba(255, 0, 0, 0.6)',
        mode = 'markers'
      ) 
      fig <- fig %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = T,
              range = c(0,10)
            ),
            angularaxis = list(tickfont = list(size = 8))
          ),annotations =list(showarrow = FALSE,
                              text=player$Overall.Score, 
                              colour = "white",
                              xanchor='centre',   
                              yanchor='centre', 
                              bgcolor="rgb(255, 255, 255)",
                              font=list(size=30, color = 'rgba(255, 0, 0, 0.6)')),
          showlegend = F
        )
    }
    else if (input$Position_2=="Goalkeeper"){
      player<-players%>%filter(Player.Name == input$Player_2)
      r<- player %>%select(Physique, Speed...Agility, Communication, Dealing.with.Crosses, General.Handling, Decision.Making,Distribution, One.vs.One.Blocking, Shape, Positional.Play.on.Shots, Shot.Stopping) 
      nms <- names(r)
      
      #code to plot the radar
      fig <- plot_ly(
        type = 'scatterpolar',
        r = as.matrix(r),
        theta = nms,
        fill = 'toself',
        fillcolor = 'rgba(255, 0, 0, 0.6)',
        mode = 'markers'
      ) 
      fig <- fig %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = T,
              range = c(0,10)
            ),
            annotations = list(text = player$Overall.Score),
            angularaxis = list(tickfont = list(size = 8))
          ),annotations =list(showarrow = FALSE,
                              text=player$Overall.Score, 
                              colour = "white",
                              xanchor='centre',   
                              yanchor='centre', 
                              bgcolor="rgb(255, 255, 255)",
                              font=list(size=30, color = 'rgba(255, 0, 0, 0.6)')),
          showlegend = F
        )
    }
    }else if(input$Type_2 == "Internal"){
      
      if (input$Position_2=="Full Back"){
        player<-players%>%filter(Player.Name == input$Player_2)
        r<- player %>%select(Physique,	Speed...Agility,	Heading.Ability,	Passing.Ability,	Work.Rate,	Defensive.Qualities.1.v.1,	Covering,	Getting.Forward...Delivery,	Recovery,	Ball.Control.Confidence,	Set.Play.Roles) 
        nms <- names(r)
        #code to plot the radar
        fig <- plot_ly(
          type = 'scatterpolar',
          r = as.matrix(r),
          theta = nms,
          fill = 'toself',
          fillcolor='rgba(255, 0, 0, 0.6)',
          mode = 'markers'
        )
        fig <- fig %>%
          layout(
            polar = list(
              radialaxis = list(
                visible = T,
                range = c(0,10),width = 200, height = 200
              ),
              angularaxis = list(tickfont = list(size = 8))
            ),annotations =list(showarrow = FALSE,
                                text=player$Overall.Score, 
                                colour = "white",
                                xanchor='centre',   
                                yanchor='centre', 
                                bgcolor="rgb(255, 255, 255)",
                                font=list(size=30, color = 'rgba(255, 0, 0, 0.6)')),
            showlegend = F
          )
      } 
      else if (input$Position_2=="Centre Back"){
        player <-filter(players, (Player.Name == input$Player_2)&(Position=="Centre Back"))
        #r <- map_dbl(player[c("Physique",	"Speed...Agility",	"Heading.Ability",	"Passing.Ability",	"Work.Rate",	"Defensive.Qualities.1.v.1",	"Recovery",	"Comfort.on.the.Ball",	"Organisation",	"Set.Play.Roles",	"Temperament")], ~.x)
        #player<-internal%>%filter(Player.Name == input$Player)
        r<- player %>%select(Physique,	Speed...Agility,	Heading.Ability,	Passing.Ability,	Work.Rate,	Defensive.Qualities.1.v.1,	Recovery,	Comfort.on.the.Ball,	Organisation,	Set.Play.Roles,	Temperament) 
        nms <- names(r)
        
        #code to plot the radar
        fig <- plot_ly(
          type = 'scatterpolar',
          r = as.matrix(r),
          theta = nms,
          fill = 'toself',
          fillcolor = 'rgba(255, 0, 0, 0.6)',
          mode = 'markers'
        ) 
        fig <- fig %>%
          layout(
            polar = list(
              radialaxis = list(
                visible = T,
                range = c(0,10)
              ),
              angularaxis = list(tickfont = list(size = 8))
            ),annotations =list(showarrow = FALSE,
                                text=player$Overall.Score, 
                                colour = "white",
                                xanchor='centre',   
                                yanchor='centre', 
                                bgcolor="rgb(255, 255, 255)",
                                font=list(size=30, color = 'rgba(255, 0, 0, 0.6)')),
            showlegend = F
          )
      }
      else if (input$Position_2=="Striker"){
        player <-filter(players, (Player.Name == input$Player_2)&(Position=="Striker"))
        #r <- map_dbl(player[c("Physique", "Speed...Agility", "Heading.Ability", "Passing.Ability", "Work.Rate", "Ball.Control.Confidence", "Shooting..Power.Accuracy.", "Link.Play", "Movement", "Game.Understanding")], ~.x)
        #player<-internal%>%filter(Player.Name == input$Player)
        r<- player %>%select(Physique, Speed...Agility, Heading.Ability, Passing.Ability, Work.Rate, Ball.Control.Confidence, Shooting..Power.Accuracy., Link.Play, Movement, Game.Understanding) 
        nms <- names(r)
        
        #code to plot the radar
        fig <- plot_ly(
          type = 'scatterpolar',
          r = as.matrix(r),
          theta = nms,
          fill = 'toself',
          fillcolor = 'rgba(255, 0, 0, 0.6)',
          mode = 'markers'
        ) 
        fig <- fig %>%
          layout(
            polar = list(
              radialaxis = list(
                visible = T,
                range = c(0,10)
              ),
              angularaxis = list(tickfont = list(size = 8))
            ),annotations =list(showarrow = FALSE,
                                text=player$Overall.Score, 
                                colour = "white",
                                xanchor='centre',   
                                yanchor='centre', 
                                bgcolor="rgb(255, 255, 255)",
                                font=list(size=30, color = 'rgba(255, 0, 0, 0.6)' )),
            showlegend = F
          )
      }
      else if (input$Position_2=="Wide Player"){
        player <- filter(players, (Player.Name == input$Player_2)&(Position=="Wide Player"))
        # r <- map_dbl(player[c("Physique", "Speed...Agility", "Passing.Ability", "Work.Rate", "Shooting..Power.Accuracy.", "Crossing.Ability", "Defensive.ability", "Comfort.on.the.Ball", "Aggression", "Goal.Threat", "Decision.Making")], ~.x)
        #player<-internal%>%filter(Player.Name == input$Player)
        r<- player %>%select(Physique, Speed...Agility, Passing.Ability, Work.Rate, Shooting..Power.Accuracy., Crossing.Ability, Defensive.ability, Comfort.on.the.Ball, Aggression, Goal.Threat, Decision.Making) 
        nms <- names(r)
        
        #code to plot the radar
        fig <- plot_ly(
          type = 'scatterpolar',
          r = as.matrix(r),
          theta = nms,
          fill = 'toself',
          fillcolor = 'rgba(255, 0, 0, 0.6)',
          mode = 'markers'
        ) 
        fig <- fig %>%
          layout(
            polar = list(
              radialaxis = list(
                visible = T,
                range = c(0,10)
              ),
              angularaxis = list(tickfont = list(size = 8))
            ),annotations =list(showarrow = FALSE,
                                text=player$Overall.Score, 
                                colour = "white",
                                xanchor='centre',   
                                yanchor='centre', 
                                bgcolor="rgb(255, 255, 255)",
                                font=list(size=30, color = 'rgba(255, 0, 0, 0.6)')),
            showlegend = F
          )
      }
      else if (input$Position_2=="Central Midfield"){
        player <- filter(players, (Player.Name == input$Player_2)&(Position=="Central Midfield"))
        #  r <- map_dbl(player[c("Physique", "Speed...Agility", "Passing.Ability", "Work.Rate", "Defensive.Qualities.1.v.1", "Movement","Tactical.Awareness", "Ball.Control", "Attacking.Qualities", "Temperament", "Heading.Ability")], ~.x)
        #player<-internal%>%filter(Player.Name == input$Player)
        r<- player %>%select(Physique, Speed...Agility, Passing.Ability, Work.Rate, Defensive.Qualities.1.v.1, Movement,Tactical.Awareness, Ball.Control, Attacking.Qualities, Temperament, Heading.Ability) 
        nms <- names(r)
        
        #code to plot the radar
        fig <- plot_ly(
          type = 'scatterpolar',
          r = as.matrix(r),
          theta = nms,
          fill = 'toself',
          fillcolor = 'rgba(255, 0, 0, 0.6)',
          mode = 'markers'
        ) 
        fig <- fig %>%
          layout(
            polar = list(
              radialaxis = list(
                visible = T,
                range = c(0,10)
              ),
              angularaxis = list(tickfont = list(size = 8))
            ),annotations =list(showarrow = FALSE,
                                text=player$Overall.Score, 
                                colour = "white",
                                xanchor='centre',   
                                yanchor='centre', 
                                bgcolor="rgb(255, 255, 255)",
                                font=list(size=30, color = 'rgba(255, 0, 0, 0.6)' )),
            showlegend = F
          )
      }
      else if (input$Position_2=="Goalkeeper"){
        player <- filter(players, (Player.Name == input$Player_2)&(Position=="Goalkeeper"))
        #r <- map_dbl(player[c("Physique", "Speed...Agility", "Communication", "Dealing.with.Crosses", "General.Handling", "Decision.Making","Distribution", "One.vs.One.Blocking", "Shape", "Positional.Play.on.Shots", "Shot.Stopping")], ~.x)
        #player<-internal%>%filter(Player.Name == input$Player)
        r<- player %>%select(Physique, Speed...Agility, Communication, Dealing.with.Crosses, General.Handling, Decision.Making,Distribution, One.vs.One.Blocking, Shape, Positional.Play.on.Shots, Shot.Stopping) 
        nms <- names(r)
        
        #code to plot the radar
        fig <- plot_ly(
          type = 'scatterpolar',
          r = as.matrix(r),
          theta = nms,
          fill = 'toself',
          fillcolor = 'rgba(255, 0, 0, 0.6)',
          mode = 'markers'
        ) 
        fig <- fig %>%
          layout(
            polar = list(
              radialaxis = list(
                visible = T,
                range = c(0,10)
              ),
              angularaxis = list(tickfont = list(size = 8))
            ),annotations =list(showarrow = FALSE,
                                text=player$Overall.Score, 
                                colour = "white",
                                xanchor='centre',   
                                yanchor='centre', 
                                bgcolor="rgb(255, 255, 255)",
                                font=list(size=30, color = 'rgba(255, 0, 0, 0.6)' )),
            showlegend = F
          )
      }
    }
  })
  output$data_2 <-renderText({
    if (input$Position_2=="Full Back"){
      player <-filter(players, (Player.Name == input$Player_2)&(Position=="Full Back"))
      paste("<b>","Player Profile" ,"</b>","<br/>",
            "Age: ", player$Age,"<br/>", 
            "Scouted by:", player$Scout,"<br/>",
            "Parent Club: ", player$Parent.Club, "<br/>", "<br/>",
            "<b>","Physique:", "</b>",str_to_sentence(player$Physique_Comments), "<br/>",
            "<b>","Speed & Agility:","</b>",str_to_sentence(player$Speed_Comments), "<br/>",
            "<b>","Heading Ability:","</b>",str_to_sentence(player$Heading_Comments), "<br/>",
            "<b>","Passing Ability S&L:","</b>", str_to_sentence(player$Passing_Comments), "<br/>",
            "<b>","Work Rate:","</b>", str_to_sentence(player$Work_Rate_Comments), "<br/>",
            "<b>","Defending Including 1 v 1:","</b>",str_to_sentence(player$Defensive_1_v_1_Comments), "<br/>",
            "<b>","Covering:","</b>", str_to_sentence(player$Covering_Comments), "<br/>",
            "<b>","Getting forward & Delivery:","</b>", str_to_sentence(player$Getting_forward_Comments), "<br/>",
            "<b>","Recovery:","</b>", str_to_sentence(player$Recovery_Comments), "<br/>",
            "<b>","Ball Control:","</b>", str_to_sentence(player$Ball_Control_Comments), "<br/>",
            "<b>","Set Play Roles:","</b>", str_to_sentence(player$Set_Play_Roles_Comments), "<br/>","<br/>","<br/>",
            "<b>", "Overall Comments & Recommendations","</b>","<br/>",
            str_to_sentence(player$Overall_Comments)
      )
    }
    else if (input$Position_2=="Centre Back"){
      player <-filter(players, (Player.Name == input$Player_2)&(Position=="Centre Back"))
      paste("<b>","Player Profile" ,"</b>","<br/>",
            "Age: ", player$Age,"<br/>", 
            "Scouted by:", player$Scout,"<br/>",
            "Parent Club: ", player$Parent.Club, "<br/>", "<br/>",
            "<b>","Physique/Power:", "</b>",str_to_sentence(player$Physique_Comments), "<br/>",
            "<b>","Speed & Agility:","</b>",str_to_sentence(player$Speed_Comments), "<br/>",
            "<b>","Heading Ability:","</b>",str_to_sentence(player$Heading_Comments), "<br/>",
            "<b>","Passing Ability S&L:","</b>", str_to_sentence(player$Passing_Comments), "<br/>",
            "<b>","Work Rate:","</b>", str_to_sentence(player$Work_Rate_Comments), "<br/>",
            "<b>","Defensive Qualities:","</b>", str_to_sentence(player$Defensive_1_v_1_Comments), "<br/>",
            "<b>","Tracking/Recovering:","</b>", str_to_sentence(player$Recovery_Comments), "<br/>",
            "<b>","Comfort on the Ball:","</b>", str_to_sentence(player$Comfort_on_the_Ball_Comments), "<br/>",
            "<b>","Organisation:","</b>", str_to_sentence(player$Organisation_Comments), "<br/>",
            "<b>","Set Play Roles:","</b>", str_to_sentence(player$Set_Play_Roles_Comments), "<br/>",
            "<b>","Temperament:","</b>", str_to_sentence(player$Temperament_Comments), "<br/>","<br/>","<br/>",
            "<b>", "Overall Comments & Recommendations","</b>","<br/>",
            str_to_sentence(player$Overall_Comments)
      )
    }
    else if (input$Position_2=="Striker"){
      player <-filter(players, (Player.Name == input$Player_2)&(Position=="Striker"))
      paste("<b>","Player Profile" ,"</b>","<br/>",
            "Age: ", player$Age,"<br/>", 
            "Scouted by:", player$Scout,"<br/>",
            "Parent Club: ", player$Parent.Club, "<br/>", "<br/>",
            "<b>","Physique:", "</b>",str_to_sentence(player$Physique_Comments), "<br/>",
            "<b>","Speed & Agility:","</b>",str_to_sentence(player$Speed_Comments), "<br/>",
            "<b>","Passing Ability S&L:","</b>", str_to_sentence(player$Passing_Comments), "<br/>",
            "<b>","Work Rate:","</b>", str_to_sentence(player$Work_Rate_Comments), "<br/>",
            "<b>","Shooting (Power/Accuracy):","</b>",str_to_sentence(player$Shooting_Comments), "<br/>",
            "<b>","Heading Ability:","</b>", str_to_sentence(player$Heading_Comments), "<br/>",
            "<b>","Ball Control:","</b>", str_to_sentence(player$Ball_Control_Comments), "<br/>",
            "<b>","Link Play:","</b>", str_to_sentence(player$Link_Comments), "<br/>",
            "<b>","Movement:","</b>", str_to_sentence(player$Movement_Comments), "<br/>",
            "<b>","Game Understanding:","</b>", str_to_sentence(player$Game_Understanding_Comments), "<br/>",
            "<b>","Set Play Roles:","</b>", str_to_sentence(player$Set_Play_Roles_Comments), "<br/>","<br/>","<br/>",
            "<b>", "Overall Comments & Recommendations","</b>","<br/>",
            str_to_sentence(player$Overall_Comments)
      )
    }
    else if (input$Position_2=="Wide Player"){
      player <-filter(players, (Player.Name == input$Player_2)&(Position=="Wide Player"))
      paste("<b>","Player Profile" ,"</b>","<br/>",
            "Age: ", player$Age,"<br/>", 
            "Scouted by:", player$Scout,"<br/>",
            "Parent Club: ", player$Parent.Club, "<br/>", "<br/>",
            "<b>","Physique:", "</b>",str_to_sentence(player$Physique_Comments), "<br/>",
            "<b>","Speed & Agility:","</b>",str_to_sentence(player$Speed_Comments), "<br/>",
            "<b>","Passing Ability S&L:","</b>", str_to_sentence(player$Passing_Comments), "<br/>",
            "<b>","Work Rate:","</b>", str_to_sentence(player$Work_Rate_Comments), "<br/>",
            "<b>","Crossing Ability:","</b>",str_to_sentence(player$Crossing_Ability_Comments), "<br/>",
            "<b>","Shooting Ability:","</b>", str_to_sentence(player$Shooting_Comments), "<br/>",
            "<b>","Defensive Abilities:","</b>", str_to_sentence(player$Defensive_Ability_Comments), "<br/>",
            "<b>","Comfort on the Ball:","</b>", str_to_sentence(player$Comfort_on_the_Ball_Comments), "<br/>",
            "<b>","Aggression:","</b>", str_to_sentence(player$Aggression_Comments), "<br/>",
            "<b>","Goal Threat:","</b>", str_to_sentence(player$Goal_Threat_Comments), "<br/>",
            "<b>","Decision Making:","</b>", str_to_sentence(player$Decision_Making_Comments), "<br/>","<br/>","<br/>",
            "<b>", "Overall Comments & Recommendations","</b>","<br/>",
            str_to_sentence(player$Overall_Comments)
      )
    }
    else if (input$Position_2=="Central Midfield"){
      player <-filter(players, (Player.Name == input$Player_2)&(Position=="Central Midfield"))
      paste("<b>","Player Profile" ,"</b>","<br/>",
            "Age: ", player$Age,"<br/>", 
            "Scouted by:", player$Scout,"<br/>",
            "Parent Club: ", player$Parent.Club, "<br/>", "<br/>",
            "<b>","Physique:", "</b>",str_to_sentence(player$Physique_Comments), "<br/>",
            "<b>","Speed & Agility:","</b>",str_to_sentence(player$Speed_Comments), "<br/>",
            "<b>","Passing Ability S&L:","</b>", str_to_sentence(player$Passing_Comments), "<br/>",
            "<b>","Work Rate:","</b>", str_to_sentence(player$Work_Rate_Comments), "<br/>",
            "<b>","Tactical Awareness:","</b>",str_to_sentence(player$Comments_Tactical), "<br/>",
            "<b>","Heading Ability:","</b>", str_to_sentence(player$Heading_Comments), "<br/>",
            "<b>","Ball Control:","</b>", str_to_sentence(player$Comments_Ball_Control), "<br/>",
            "<b>","Defensive Qualities 1 v 1:","</b>", str_to_sentence(player$Defensive_1_v_1_Comments), "<br/>",
            "<b>","Attacking Qualities:","</b>", str_to_sentence(player$Comments_Attacking), "<br/>",
            "<b>","Movement:","</b>", str_to_sentence(player$Movement_Comments), "<br/>",
            "<b>","Temperament:","</b>", str_to_sentence(player$Temperament_Comments), "<br/>","<br/>","<br/>",
            "<b>", "Overall Comments & Recommendations","</b>","<br/>",
            str_to_sentence(player$Overall_Comments)
      )
      
    }
    else if (input$Position_2=="Goalkeeper"){
      player <-filter(players, (Player.Name == input$Player_2)&(Position=="Goalkeeper"))
      paste("<b>","Player Profile" ,"</b>","<br/>",
            "Age: ", player$Age,"<br/>", 
            "Scouted by:", player$Scout,"<br/>",
            "Parent Club: ", player$Parent.Club, "<br/>", "<br/>",
            "<b>","Physique:", "</b>",str_to_sentence(player$Physique_Comments), "<br/>",
            "<b>","Speed & Agility:","</b>",str_to_sentence(player$Speed_Comments), "<br/>",
            "<b>","Communication:","</b>", str_to_sentence(player$Communication_Comments), "<br/>",
            "<b>","Dealing with Crosses:","</b>", str_to_sentence(player$Dealing_with_Crosses_Comments), "<br/>",
            "<b>","General Handling:","</b>",str_to_sentence(player$General_Handling_Comments), "<br/>",
            "<b>","Decision Making:","</b>", str_to_sentence(player$Decision_Making_Comments), "<br/>",
            "<b>","Distribution:","</b>", str_to_sentence(player$Distribution_Comments), "<br/>",
            "<b>","One vs One Blocking:","</b>", str_to_sentence(player$One_Vs_One_Comments), "<br/>",
            "<b>","Shape:","</b>", str_to_sentence(player$Shape_Comments), "<br/>",
            "<b>","Positional Play on Shots:","</b>", str_to_sentence(player$Positional_Comments), "<br/>",
            "<b>","Shot Stopping:","</b>", str_to_sentence(player$Shot_Stopping_Comments), "<br/>","<br/>","<br/>",
            "<b>", "Overall Comments & Recommendations","</b>","<br/>",
            str_to_sentence(player$Overall_Comments)
      )
      
    }
  })
  output$ranking_table <-renderTable({
    if(input$Type=="External"){
    if (input$Position=="Full Back"){
      players%>%select(Player.Name, Age,Type, Position,Overall.Score, Grade)%>%filter(Position == "Full Back")%>%slice_max(Overall.Score, n = 10)
    }
    else if (input$Position=="Centre Back"){
      players%>%select(Player.Name, Age,Type, Position,Overall.Score, Grade)%>%filter(Position == "Centre Back")%>%slice_max(Overall.Score, n = 10)
    }
    else if (input$Position=="Striker"){
      players%>%select(Player.Name, Age,Type, Position,Overall.Score, Grade)%>%filter(Position == "Striker")%>%slice_max(Overall.Score, n = 10)
    }
    else if (input$Position=="Wide Player"){
      players%>%select(Player.Name, Age,Type, Position,Overall.Score, Grade)%>%filter(Position == "Wide Player")%>%slice_max(Overall.Score, n = 10)
    }
    else if (input$Position=="Central Midfield"){
      players%>%select(Player.Name, Age,Type, Position,Overall.Score, Grade)%>%filter(Position == "Central Midfield")%>%slice_max(Overall.Score, n = 10)
    }
    else if (input$Position=="Goalkeeper"){
      players%>%select(Player.Name, Age,Type, Position,Overall.Score, Grade)%>%filter(Position == "Goalkeeper")%>%slice_max(Overall.Score, n = 10)
    }
    }else if(input$Type == "Internal"){
      if (input$Position=="Full Back"){
        players%>%select(Player.Name, Age,Type, Position,Overall.Score, Grade)%>%filter(Position == "Full Back")%>%slice_max(Overall.Score, n = 10)
      }
      else if (input$Position=="Centre Back"){
        players%>%select(Player.Name, Age,Type, Position,Overall.Score, Grade)%>%filter(Position == "Centre Back")%>%slice_max(Overall.Score, n = 10)
      }
      else if (input$Position=="Striker"){
        players%>%select(Player.Name, Age,Type, Position,Overall.Score, Grade)%>%filter(Position == "Striker")%>%slice_max(Overall.Score, n = 10)
      }
      else if (input$Position=="Wide Player"){
        players%>%select(Player.Name, Age,Type, Position,Overall.Score, Grade)%>%filter(Position == "Wide Player")%>%slice_max(Overall.Score, n = 10)
      }
      else if (input$Position=="Central Midfield"){
        players%>%select(Player.Name, Age,Type, Position,Overall.Score, Grade)%>%filter(Position == "Central Midfield")%>%slice_max(Overall.Score, n = 10)
      }
      else if (input$Position=="Goalkeeper"){
        players%>%select(Player.Name, Age,Type, Position,Overall.Score, Grade)%>%filter(Position == "Goalkeeper")%>%slice_max(Overall.Score, n = 10)
      } 
    }
  },caption = html_caption_str, caption.placement = "top")
  
  output$ranking_table_2 <-renderTable({ 
    if(input$Type_2 == "External"){
    if (input$Position_2=="Full Back"){
      players%>%select(Player.Name, Age,Type, Position,Overall.Score, Grade)%>%filter(Position == "Full Back")%>%slice_max(Overall.Score, n = 10)
    }
    else if (input$Position_2=="Centre Back"){
      players%>%select(Player.Name, Age,Type, Position,Overall.Score, Grade)%>%filter(Position == "Centre Back")%>%slice_max(Overall.Score, n = 10)
    }
    else if (input$Position_2=="Striker"){
      players%>%select(Player.Name, Age,Type, Position,Overall.Score, Grade)%>%filter(Position == "Striker")%>%slice_max(Overall.Score, n = 10)
    }
    else if (input$Position_2=="Wide Player"){
      players%>%select(Player.Name, Age,Type, Position,Overall.Score, Grade)%>%filter(Position == "Wide Player")%>%slice_max(Overall.Score, n = 10)
    }
    else if (input$Position_2=="Central Midfield"){
      players%>%select(Player.Name, Age,Type, Position,Overall.Score, Grade)%>%filter(Position == "Central Midfield")%>% slice_max(Overall.Score, n = 10)
    }
    else if (input$Position_2=="Goalkeeper"){
      players%>%select(Player.Name, Age,Type, Position,Overall.Score, Grade)%>%filter(Position == "Goalkeeper")%>%slice_max(Overall.Score, n = 10)
    }
    }else if(input$Type_2 == "Internal"){
      if (input$Position_2=="Full Back"){
        players%>%select(Player.Name, Age,Type, Position,Overall.Score, Grade)%>%filter(Position == "Full Back")%>%slice_max(Overall.Score, n = 10)
      }
      else if (input$Position_2=="Centre Back"){
        players%>%select(Player.Name, Age,Type, Position,Overall.Score, Grade)%>%filter(Position == "Centre Back")%>%slice_max(Overall.Score, n = 10)
      }
      else if (input$Position_2=="Striker"){
        players%>%select(Player.Name, Age,Type, Position,Overall.Score, Grade)%>%filter(Position == "Striker")%>%slice_max(Overall.Score, n = 10)
      }
      else if (input$Position_2=="Wide Player"){
        players%>%select(Player.Name, Age,Type, Position,Overall.Score, Grade)%>%filter(Position == "Wide Player")%>%slice_max(Overall.Score, n = 10)
      }
      else if (input$Position_2=="Central Midfield"){
        players%>%select(Player.Name, Age,Type, Position,Overall.Score, Grade)%>%filter(Position == "Central Midfield")%>%slice_max(Overall.Score, n = 10)
      }
      else if (input$Position_2=="Goalkeeper"){
        players%>%select(Player.Name, Age,Type, Position,Overall.Score, Grade)%>%filter(Position == "Goalkeeper")%>%slice_max(Overall.Score, n = 10)
      }
    }
  }, caption = html_caption_str, caption.placement = "top")
})
shinyApp(ui=ui, server=server)
}