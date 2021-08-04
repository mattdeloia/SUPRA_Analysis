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

df <- read_xlsx("SPARTA Raw Data (All Trials).xlsx") %>% 
     left_join(read_xlsx("Key.xlsx")) %>% 
     clean_names() %>%
     separate(participant_name, into=c("event", "participant", "trial"), sep="_" ) %>% 
     filter(result=="PointsBased") %>% 
     select(participant, event, trial, measure, category, description, points_scored, grader_name) %>% 
     drop_na(category) 
 
 df2 <- df %>% 
     filter(event=="STX") %>% 
     group_by(participant, event,  trial, measure, category, description) %>% 
     summarise(points_scored = median(points_scored, na.rm = TRUE)) %>% #median scores 
     pivot_wider(names_from = "description", values_from = "points_scored") %>% 
     ungroup() %>% 
     mutate_if(is.numeric, scale) %>% 
     pivot_longer(6:65, names_to="description", values_to = "score") %>%
     rbind(
         df %>% 
             filter(event=="SH") %>% 
             group_by(grader_name, participant, event, trial, measure, category, description) %>%
             summarise(points_scored = mean(points_scored, na.rm = TRUE)) %>% #mean of rooms by OC
             group_by(participant, event, trial, measure, category, description) %>% 
             summarise(points_scored = median(points_scored, na.rm = TRUE)) %>% #median from multiple OCs
             pivot_wider(names_from = "description", values_from = "points_scored") %>% 
             ungroup() %>% 
             mutate_if(is.numeric, scale) %>% 
             pivot_longer(6:41, names_to="description", values_to = "score")
     ) %>% 
     drop_na()
 
 squadlist <- df$participant %>% unique()
 
 #determine squad ranks (x = weighting_value)
 
 figure <- function(x, y, z) {
     df2 %>%  
         filter(participant == x, event == y, measure == z) %>% 
         group_by(participant, measure, category, trial) %>% 
         summarise(score = mean(score)) %>% 
         ggplot(aes(x=reorder(category, score, fun=mean), y=score, color=if_else(score<(-.5), "low", if_else(score<=.5, "average", "high")))) + 
         geom_point(size=3) + 
         geom_hline(yintercept = c(-2, -1.5, -1, -.5, 0, .5, 1, 1.5, 2), linetype="blank") +
         geom_hline(yintercept = c( -.5, 0, .5), linetype="dashed", color = "darkgray") +
         scale_color_manual(values=c("darkgray", "green", "red")) +
         coord_flip() +
         theme(legend.position = "blank")+
         theme(axis.text = element_text(size = 10))+
         theme(strip.background = element_rect(color = "blue", fill="lightblue", size = .5)) +
         scale_y_continuous(breaks = c(-2, -1.5, -1, -.5, 0, .5, 1, 1.5, 2), labels=c("","", "below","",  "average","", "above", "", "")) +
         xlab("") +
         facet_grid(.~trial, scales="free")
 }

 figure("Squad2", "STX", "Task")
 
 table <- function (x, y, z) {
   df2 %>% 
     filter(participant==x, event==y, measure==z) %>% 
     pivot_wider(names_from = "trial", values_from = "score") %>% 
     rownames_to_column("id") %>% 
     clean_names() %>% 
     group_by(id) %>% 
     mutate(ave = mean(c(trial1, trial2, trial3), na.rm = TRUE)) %>%
     ungroup() %>% 
     mutate(rank = rank(-ave)) %>%
     ungroup() %>% 
     mutate_at(vars(trial1:ave), ~if_else(.<(-.5), "low", if_else(.<=.5, "ave", "high"))) %>% 
     arrange(event, category, rank) %>% 
     ungroup() %>% 
     select(category:trial3, rank) %>% 
     datatable(class = "display compact", 
               rownames= FALSE, 
               options = list (
                 pageLength = 15 ))  %>%
     formatStyle (c('trial1', 'trial2', 'trial3'), 
                  color = styleEqual (c("low", "ave", "high") , c("red","lightgray", "green")))
 }
 
 table("Squad1", "STX", "Task")
 
 df_measures <- df2 %>% 
   select(event, measure, category, description) %>% 
   unique() %>% 
   group_by(event, measure, category) %>% 
   summarise(items = n()) %>% 
   ungroup()
 
 ui <- dashboardPage(
    
    dashboardHeader(title="Squad Dashboard"),
    
    dashboardSidebar(
    
      sidebarMenu( br(),
      menuItem("Visualization", tabName = "visualization", icon = icon("bar-chart-o")),
      menuItem("Data Table", tabName = "datatable", icon = icon("table")),
      menuItem("Squad Summary", tabName = "summary", icon = icon ("dashboard"))
      ),
      
     radioButtons("participant", label = "Squad", choices = squadlist),
      radioButtons("event", label = "Event", choices = list("STX", "SH")),
      
      radioButtons("measure", label = "Measure", choices = list("Task", "Performance")),
      sliderInput("weighting", label = "Task weighting factor:", min=1, max=3, value=2, ticks = FALSE ),
     
     #fileInput("file", label = "File input"),
      
      gt_output("itemtable"),
            verbatimTextOutput("participant"),
            verbatimTextOutput("event"),
            verbatimTextOutput("measure")
            
      
            ),

    dashboardBody(
      
      tabItems(
        
          tabItem("visualization",
                  
            infoBoxOutput("trial1"),
            
            infoBoxOutput("trial2"), 
            
            infoBoxOutput("trial3"), 
            
            plotOutput("mainplot", height = "400px" )
        ),
      
        tabItem("datatable",
               
               dataTableOutput("maintable")),
        
        tabItem("summary",
                downloadButton("downloadResults", "Download Results"),
                plotOutput("comparisonplot", height = "500px")
                )
      ) )  
 )
    
server <- function(input, output) {

  df_rank <- reactive ({
    df %>% 
      filter(measure=="Task") %>%
      group_by(participant, event,  trial, measure, description) %>% 
      summarise(points_scored = median(points_scored, na.rm = TRUE)) %>% 
      left_join(read_xlsx("Key.xlsx", sheet="Weightings")) %>%
      mutate(weight = if_else(weight=="x", as.numeric(input$weighting), 1)) %>% 
      mutate(weight = replace_na(weight, 1)) %>% 
      mutate(weighted_raw = points_scored*weight) %>% 
      group_by(participant, event,  trial, measure) %>% 
      summarise(point_total = sum(weighted_raw)) %>%
      mutate(point_total = if_else(event=="SH",  point_total/(12*2+9*2*input$weighting),point_total/(33*2 +12*2*input$weighting))) %>% 
      rbind( df %>% 
               filter(measure=="Performance") %>% 
               group_by(participant, trial, measure, event, category) %>% 
               summarise(points_scored = median(points_scored, na.rm = TRUE)) %>% 
               group_by(participant, trial, measure, event) %>% 
               summarise(point_total = mean(points_scored, na.rm= TRUE)/10) 
      )  
  })
  
  df_rank_tr1 <- reactive ({df_rank() %>%
      group_by(event, measure, trial) %>% 
      mutate(rank = rank(-point_total)) %>% 
      filter(participant == input$participant, event == input$event, measure == input$measure) %>% 
      filter(trial == "Trial1")})
  
  df_rank_tr2 <- reactive ({df_rank() %>%
      group_by(event, measure, trial) %>% 
      mutate(rank = rank(-point_total)) %>% 
      filter(participant == input$participant, event == input$event, measure == input$measure) %>% 
      filter(trial == "Trial2")})
  
  df_rank_tr3 <- reactive ({df_rank() %>%
      group_by(event, measure, trial) %>% 
      mutate(rank = rank(-point_total)) %>% 
      filter(participant == input$participant, event == input$event, measure == input$measure) %>% 
      filter(trial == "Trial3")})
  
    output$trial1 <- renderInfoBox({
      infoBox(title = "Trial 1 Rank",
              subtitle = "rank of 3",
              value =df_rank_tr1()$rank, 
              color = if_else(df_rank_tr1()$rank == "1", "lime", "light-blue") ) })
    
    output$trial2 <- renderInfoBox({
      infoBox(title = "Trial 2 Rank",
              subtitle = "rank of 3",
              value =df_rank_tr2()$rank,
              color = if_else(df_rank_tr2()$rank == "1", "lime", "light-blue") ) })
    
    output$trial3 <- renderInfoBox({
      infoBox(title = "Trial 3 Rank",
              subtitle = "rank of 3",
              value =df_rank_tr3()$rank, 
              color = if_else(df_rank_tr3()$rank == "1", "lime", "light-blue") ) })
    
    output$participant <- renderPrint({input$participant})
    
    output$event <- renderPrint({input$event})
    
    output$measure <- renderPrint({input$measure})
    
    output$weighting <- renderPrint({input$weighting})
    
    
    output$mainplot <- renderPlot({
        figure(input$participant, input$event, input$measure)
    })
    
    output$maintable <- renderDataTable ({
      table(input$participant, input$event, input$measure)
    })

    output$itemtable <- render_gt ({

        df_measures %>% 
        filter(event==input$event, measure==input$measure) %>% 
        select(category, items) %>%
        arrange(-items) %>% 
      gt() %>% tab_options(table.font.size = 10, data_row.padding = 1, table.width = pct(80))
    })
    
    output$comparisonplot <- renderPlot({
      df_rank() %>%
        filter(event==input$event) %>% 
        ggplot(aes(x=trial, y=point_total, color=participant, group = participant)) +
        geom_point(size=3) +
        geom_line(linetype = "dashed")+
        ggrepel::geom_text_repel(aes(label = round(point_total,2)), size=4) +
        xlab("") +
        ylab("points (proportion of total)") +
        theme(axis.text = element_text(size = 12)) +
        facet_grid(measure~.)
    })
    
    output$downloadResults <- downloadHandler(
      filename = function(){
        paste("Results_",Sys.Date(), ".csv")
      },
      content=function(file) {
        write.csv(df_rank(), file) 
      })
      
    
}

# Run the application 
shinyApp(ui = ui, server = server)

