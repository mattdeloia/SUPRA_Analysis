#load libraries
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(readxl)
library(tidyverse)
library(janitor)
library(DT)
library(gt)
library(shinyjs)
library(gghighlight)

df <- read_xlsx("SPARTA Raw Data (All Trials).xlsx") %>%
  clean_names() %>%
  separate(participant_name, into=c("event", "participant", "trial"), sep="_" ) %>%
  bind_rows(
    read_xlsx("72HFS_STX_Raw Data.xlsx") %>% 
      clean_names() %>% 
      mutate(event = "STX", room = "", trial = "Trial1") %>%
      mutate(participant = participant_name)
    ) %>% 
  left_join(read_xlsx("Key.xlsx", sheet= "Key") %>%
              clean_names()
            ) %>%
  filter(result=="PointsBased") %>% 
  select(participant, event, trial, room, measure, category, description, points_scored, grader_name, feature) %>%
  drop_na(category) 

df2 <- df %>% 
  filter(event=="STX") %>% 
  group_by(participant, event,  trial,  measure, category, description) %>% 
  summarise(points_scored = median(points_scored, na.rm = TRUE)) %>% #median scores 
  pivot_wider(names_from = "description", values_from = "points_scored") %>% 
  ungroup() %>% 
  mutate_if(is.numeric, scale) %>% 
  pivot_longer(6:65, names_to="description", values_to = "score") %>%
  rbind(
     df %>% 
      filter(event=="SH") %>% 
      group_by(participant, event, trial, room, measure, category, description) %>%
      summarise(points_scored = median(points_scored, na.rm = TRUE)) %>% #medians of OC scores by room
      group_by(participant, event, trial, measure, category, description) %>%
      summarise(points_scored = mean(points_scored, na.rm = TRUE)) %>% #mean of room scores  by trial
      pivot_wider(names_from = "description", values_from = "points_scored") %>% 
      ungroup() %>% 
      mutate_if(is.numeric, scale) %>% 
      pivot_longer(6:41, names_to="description", values_to = "score")
  ) %>% 
  drop_na(score)
 
squadlist <- c("Squad1", "Squad2", "Squad3", "Squad5", "Squad6", "Squad7", "Squad8")
triallist <- c("Trial1", "Trial2", "Trial3")

figure <- function(x, y, z, q) {
   df2 %>%  
     filter(participant == x, event == y, measure == z) %>% 
     group_by(participant, measure, category, trial) %>%
     mutate (category = factor (category, levels= c(
       "TeamLdr_Post",
       "SquadLdr_Post",
       "Clearing",
       "TeamLdr_Pre",
       "SquadLdr_Pre",
       "Reorganization",
       "Consolidation_Security",
       "Team_SBF",
       "Team_Assault",
       "SquadLdr",
       "Security",
       "Cover and Concealment",
       "Control",
       "Communication",
       "Speed",
       "Surprise",
       "Violence of Action",
       "Fire Effectiveness",
       "Simplicity",
       "Information Exchange",
       "Weapons Handling",
       "Supporting Behavior",
       "Initiative/Leadership",
       "Planning",
       "Rehearsals")
     )) %>%  
     summarise(score = mean(score, na.rm=TRUE)) %>% 
     filter(trial %in% c(q)) %>% 
     ggplot(aes(x=category, y=score, color=trial)) + 
     # if_else(score<(-.5), "low", if_else(score<=.5, "average", "high"))
     geom_jitter(height = 0, width = .05, size=3) + 
     geom_hline(yintercept = c(-2, -1.5, -1, -.5, 0, .5, 1, 1.5, 2), linetype="blank") +
     geom_hline(yintercept = c( -.5, 0, .5), linetype="dashed", color = c("red", "darkgray", "green"), size=1) +
     scale_color_manual(name = y, values=c("Trial1"="darkgray","Trial2"= "skyblue", "Trial3"="blue")) +
     coord_flip() +
     scale_y_continuous(breaks = c(-2, -1.5, -1, -.5, 0, .5, 1, 1.5, 2), 
                        labels=c("","", "below","",  "average","", "above", "", "")) +
     theme(legend.position = "top")+
     theme(axis.text = element_text(size = 10))+
     theme(axis.text.x = element_text(color=c("darkgreen", "darkgray", "red"))) +
     labs(x= "", y="", caption =  "Note: scaled for comparison to all squads and trials")
 }

 figure("Squad1", "SH", "Task", c("Trial1", "Trial2", "Trial3"))
 
 table <- function (x, y, z) {
   df2 %>% filter(participant %in% c(squadlist)) %>% 
     filter(participant==x, event==y, measure==z) %>% 
     pivot_wider(names_from = "trial", values_from = "score") %>% 
     rownames_to_column("id") %>% 
     clean_names() %>% 
     group_by(id) %>% 
     mutate(overall = mean(c(trial1, trial2, trial3), na.rm = TRUE)) %>%
     ungroup() %>% 
     mutate(rank = rank(-overall)) %>%
     ungroup() %>% 
     mutate_at(vars(trial1:overall), ~if_else(.<(-.5), "low", if_else(.<=.5, "ave", "high"))) %>% 
     left_join(read_xlsx("Key.xlsx", sheet="Weightings") %>% 
                 select(description, feature )) %>%  
     arrange(feature, participant, event, category, rank) %>% 
     ungroup() %>% 
     select(participant, event, category:trial3, overall, rank) 
     }
 
 table("Squad1", "STX", "Task")
 
 df_measures <- df2 %>% 
   select(event, measure, category, description) %>% 
   unique() %>% 
   group_by(event, measure, category) %>% 
   summarise(items = n()) %>% 
   ungroup()

#  display of rater scores
 figure2 <- function(x, y, z) {
   df  %>% 
     mutate(points_scored = as.character(points_scored)) %>% 
     group_by(event, measure, trial, grader_name, points_scored) %>% 
     summarise(count = n()) %>% 
     mutate(percent = count/sum(count)) %>% 
     filter(measure==x, event==y, trial%in%c(z)) %>% 
     ggplot(aes(x=points_scored, y=percent, fill=grader_name)) +
     geom_col(position='dodge2') +
     labs(x="point score", y = "proportion of responses") +
     facet_grid(event~trial, scales="free")
 }
 
 df_rank <- function(x) {
   df %>% 
   filter(measure=="Task") %>%
   group_by(participant, event,  trial, room, measure, description) %>% 
   summarise(points_scored = median(points_scored, na.rm = TRUE)) %>%
   group_by(participant, event, trial, measure, description) %>%
   summarise(points_scored = mean(points_scored, na.rm = TRUE)) %>% 
   left_join(read_xlsx("Key.xlsx", sheet="Weightings") %>%
               select(description, weight1, weight2, weight3) %>%
               drop_na(weight1) %>% 
               gather(weight1:weight3, key="weight", value="value") %>% 
               filter(weight == x)
             ) %>%
     mutate(weighted_raw = points_scored*value) %>% 
     group_by(participant, event,  trial, measure) %>% 
     summarise(point_total = sum(weighted_raw)/2) %>%
     rbind(df %>% 
              filter(measure=="Performance") %>% 
              group_by(participant, trial, measure, event, category) %>% 
              summarise(points_scored = median(points_scored, na.rm = TRUE)) %>% 
              group_by(participant, trial, measure, event) %>% 
              summarise(point_total = mean(points_scored, na.rm= TRUE)/10)
            )
   }

 ui <- dashboardPage(
    
    dashboardHeader(title="SUPRA Dashboard"),
    
    dashboardSidebar(
    
      sidebarMenu( br(),
      menuItem("Visualization", tabName = "visualization", icon = icon("bar-chart-o")),
      menuItem("Data Table", tabName = "datatable", icon = icon("table")),
      menuItem("Squad Summary", tabName = "summary", icon = icon ("dashboard")),
      menuItem("OC Review", tabName = "ocreview", icon = icon ("flag"))),
      
     radioButtons("participant", label = "Squad", choices = squadlist),
     
     radioButtons("event", label = "Event", choices = list("STX", "SH")),
     
     checkboxGroupInput("trials", label = "Trials", choices = triallist, selected = "Trial1" ),
     
     radioButtons("measure", label = "Measure", choices = list("Task", "Performance")),
     
     radioButtons("weight", label = "Global weight options", choices = list("weight1", "weight2", "weight3"), selected = "weight3"),
     
     gt_output("itemtable"),
     verbatimTextOutput("participant"),
     verbatimTextOutput("event"),
     verbatimTextOutput("measure")
     ),

    dashboardBody(
      
      tabItems(
        
        tabItem("visualization",
                plotOutput("mainplot", height = "500px" )
                ),
        
        tabItem("datatable",
                downloadButton("downloadResults1", "Download Results"),
                dataTableOutput("maintable")
                ),
        
        tabItem("summary",
                downloadButton("downloadResults2", "Download Results"),
                br(),
                infoBoxOutput("trial1"),
                infoBoxOutput("trial2"), 
                infoBoxOutput("trial3"),
                plotOutput("comparisonplot", height = "400px")),
        
        tabItem("ocreview",
                plotOutput("ocplot", height = "500px"))
        )
      )
    )
server <- function(input, output) {


  df_rank_tr1 <- reactive ({df_rank(input$weight) %>%
      group_by(event, measure, trial) %>% 
      mutate(rank = rank(-point_total)) %>% 
      filter(participant == input$participant, event == input$event, measure == input$measure) %>% 
      filter(trial == "Trial1")})
  
  squad_count_tr1 <- reactive ({df_rank(input$weight) %>%
     filter(event == input$event, measure == input$measure) %>% 
      filter(trial == "Trial1") %>% 
      select(participant) %>% 
      unique() %>% 
      as.data.frame %>% 
      nrow()
      })
  
  df_rank_tr2 <- reactive ({df_rank(input$weight) %>%
      group_by(event, measure, trial) %>% 
      mutate(rank = rank(-point_total)) %>% 
      filter(participant == input$participant, event == input$event, measure == input$measure) %>% 
      filter(trial == "Trial2")})
  
  squad_count_tr2 <- reactive ({df_rank(input$weight) %>%
      filter(event == input$event, measure == input$measure) %>% 
      filter(trial == "Trial2") %>% 
      select(participant) %>% 
      unique() %>% 
      as.data.frame %>% 
      nrow()
  })
  
  df_rank_tr3 <- reactive ({df_rank(input$weight) %>%
      group_by(event, measure, trial) %>% 
      mutate(rank = rank(-point_total)) %>% 
      filter(participant == input$participant, event == input$event, measure == input$measure) %>% 
      filter(trial == "Trial3")})
  
  squad_count_tr3 <- reactive ({df_rank(input$weight) %>%
      filter(event == input$event, measure == input$measure) %>% 
      filter(trial == "Trial3") %>% 
      select(participant) %>% 
      unique() %>% 
      as.data.frame %>% 
      nrow()
  })
  
    output$trial1 <- renderInfoBox({
      infoBox(title = "Trial 1 Rank",
              subtitle = paste("rank of ", squad_count_tr1()),
              value =df_rank_tr1()$rank, 
              color = if_else(df_rank_tr1()$rank == "1", "lime", "light-blue") ) })
    
    output$trial2 <- renderInfoBox({
      infoBox(title = "Trial 2 Rank",
              subtitle = paste("rank of ", squad_count_tr2()),
              value =df_rank_tr2()$rank,
              color = if_else(df_rank_tr2()$rank == "1", "lime", "light-blue") ) })
    
    output$trial3 <- renderInfoBox({
      infoBox(title = "Trial 3 Rank",
              subtitle = paste("rank of ", squad_count_tr3()),
              value =df_rank_tr3()$rank, 
              color = if_else(df_rank_tr3()$rank == "1", "lime", "light-blue") ) })
    
    output$participant <- renderPrint({input$participant})
    
    output$event <- renderPrint({input$event})
    
    output$measure <- renderPrint({input$measure})
    
    output$weight <- renderPrint({input$weighting})
    
    output$mainplot <- renderPlot({
        figure(input$participant, input$event, input$measure, input$trials)
    })
    
    output$ocplot <- renderPlot ({
      figure2(input$measure, input$event, input$trials)
    })
    
    output$maintable <- renderDataTable ({
      table(input$participant, input$event, input$measure) %>% 
        ungroup() %>% 
        arrange(category, rank) %>% 
        datatable(class = "display compact", 
                  rownames= FALSE, 
                  options = list (
                    pageLength = 15 ))  %>%
        formatStyle (c('trial1', 'trial2', 'trial3', 'overall'), 
                     color = styleEqual (c("low", "ave", "high") , c("red","gray", "green")))
    })

    output$itemtable <- render_gt ({
        df_measures %>% 
        filter(event==input$event, measure==input$measure) %>% 
        select(category, items) %>%
        arrange(-items) %>% 
      gt() %>% tab_options(table.font.size = 10, data_row.padding = 1, table.width = pct(80))
    })
    
    df_rank_ave <- reactive ({
      df_rank(input$weight) %>%
        ungroup() %>% 
        group_by(trial, measure, event) %>% 
        summarise(point_total = mean(point_total)) %>%
        mutate(participant ="event_ave")
      })
    
    output$comparisonplot <- renderPlot({
      df_rank(input$weight) %>%
        filter(event==input$event,
               measure==input$measure,
               participant==input$participant) %>%
        bind_rows(
          df_rank_ave() %>% 
            filter(event==input$event,
                            measure==input$measure)
          ) %>% 
        ggplot(aes(x=trial, y=point_total,  group = participant)) +
        geom_point(size=4, color="red") +
        geom_line(linetype = "solid", color="red", size=1)+
        ggrepel::geom_text_repel(aes(label = round(point_total,2)), size=6) +
        gghighlight(participant==input$participant, unhighlighted_colour = "darkgray", unhighlighted_params = list(size=3, linetype = "dotted")) +
        xlab("") +
        ylab("points (proportion of total)") +
        theme(axis.text = element_text(size = 12)) +
        ggtitle(paste("Event: ", input$event)) + 
        ylim(0,1)
    })
    
    output$downloadResults1 <- downloadHandler(
      filename = function(){
        paste("SquadResults_Task_",input$participant, sep = "")
      },
      content=function(filename) {
        write.csv(table(input$participant, input$event, input$measure), filename) 
      })
    
    output$downloadResults2 <- downloadHandler(
      filename = function(){
        paste("SquadResults_Summary", ".csv", sep="")
      },
      content=function(file) {
        write.csv(df_rank(input$weight), file) 
      })
      
}

# Run the application 
shinyApp(ui = ui, server = server)