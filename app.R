#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(fs)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(showtext)
library(DT)
library(forcats)
showtext_auto(enable = TRUE)
#i=1
#vector_list <- list(agust$mon,agust$tue,agust$wed,agust$thur,agust$fri,agust$sat,agust$sun)
#students <- c()
#while (i <= 6) {
#  temp <- union(vector_list[[i]],vector_list[[i+1]]) %>% na.omit()
#  student_temp <- c(temp,students) %>% unique()
#  students <- student_temp
#  i = i+1
#}
# 管红林

month_list <- c('jan','feb','mar','apr','may','jun','jul','aug','sept','oct','nov','dec')
normal_students <- c( "王子怡","管红琳","王刘宇","薛雨杭","张萌","葛睿",
                      "周雪榕","郭益欣","付晓研","刘彤","焦宇","王爽","杜宇昂",
                      "刘文晗" ,"丁心","张露丹","孟祥月","郑焱华")
new_cols <- c("type1","mon","tue","wed","thur","fri","sat","sun","type2")
weekday_type_commm <- c("主院连班","浑南连班","骨穿班")
weekend_type_commm <- c("主院白班","层流白班","浑南白班")
universe_type <- c("主院夜班","层流夜班","浑南夜班")

get_hospital_timetalbe <- function(dir,new_cols = new_cols){
  aguddd <- read_excel(dir)
  month_data <- str_sub(dir,1,nchar(dir)-5)
  month_data <- str_remove(month_data,pattern = "data/")
  colnames(aguddd) <- new_cols
  agust_clean <- aguddd %>% 
    filter(!is.na(type1) | !is.na(type2) ) %>% 
    filter(type2 != "日期" ) %>% 
    filter(type2 != "值班") %>% 
    mutate(weekday_type = ifelse(is.na(type1)& !is.na(type2),type2,type1)) %>% 
    mutate(weekend_type = ifelse(is.na(type2)& !is.na(type1),type1,type2)) %>%
    mutate(weekend_type = ifelse(weekend_type == "骨穿班","主院白班",weekend_type)) %>% 
    select(-type1) %>% 
    relocate(weekday_type) %>% 
    select(-type2) %>% 
    mutate(month = month_data)
}

Batch_hospital_table <- function(root,new_cols = new_cols){
  files <- list.files(root)
  dflist <- list()
  for (i in 1:length(files)) {
    dflist[[i]] <- get_hospital_timetalbe(paste0(root,files[i]),
                                          new_cols = new_cols)
  }
  df_total = data.table::rbindlist(dflist,fill = TRUE)
}

get_name_score <- function(df,name = "王子怡",mod = c("weekday","weekend","universe"),type,month_data){
  df <- df %>% 
    filter(month == month_data)
  if (mod == "weekday"){
    df <- df %>% 
      select(-all_of(c("sat","sun","weekend_type"))) %>% 
      filter(weekday_type == type)
    #row_level <- levels(factor(df$weekday_type))
    
  }else if(mod == "weekend"){
    df <- df %>% 
      select(all_of(c("weekend_type","sat","sun"))) %>% 
      filter(weekend_type == type)
  } else {
    df <- df %>% 
      select(-weekend_type) %>% 
      filter(weekday_type == type)
  }
  df <- as.data.frame(df)
  score = 0
  for (i in 1:ncol(df)){
    for (a in 1:nrow(df)) {
      if (is.na(df[a,i])){
        score_temp = score + 0
        score = score_temp
      }else if(str_detect(df[a,i],name) == TRUE){
        score_temp = score + 1
        score = score_temp
      } else {
        score_temp = score + 0
        score = score_temp
      }
    }
  }
  return(score)
}

week_summary <- function(batch_table,name_id,mod,month_data_list,types){
  weekdaysscore <- list()
  for (months_item in month_data_list) {
    temp <- rep(0,length(types))
    for (i in 1:length(types)) {
      temp[i] <- get_name_score(batch_table,name = name_id,mod = mod,
                                type = types[i],month_data = months_item)
    }
    
    weekdaysscore[[months_item]] <- data.frame(types = types,
                           temp = temp) %>% 
      rename(months_item = temp) 
  }
  weekdaysscore <- do.call("cbind",weekdaysscore) %>% 
    select(paste0(month_data_list[1],".types"),ends_with("months_item"))
  names(weekdaysscore) <- c("types",month_data_list)
  weekdaysscore <- weekdaysscore %>% 
    mutate(id =name_id)
  return(weekdaysscore)
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("The KPI Panel in Hematology"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("name_id",
                        "Whoes KPI you want to see(for example):",
                        "王子怡"),
            selectInput("month_id",
                      "Which month you want to know:",
                      month_list,
                      "aug")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel(title = "个人面板",
                     plotOutput("distPlot"),
                     tableOutput("selftable")
            ),
            tabPanel(title = "月排行榜",
                     plotOutput("rankplot")
            ),
            tabPanel(title = "具体数据", 
                     DTOutput("tables")
            )
          )
          #plotOutput("distPlot"),
           #plotOutput("rankplot"),
           #tableOutput("tables")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  batch_table <- reactive({
    Batch_hospital_table("data/",new_cols = new_cols)
  })
  
  month_data_list <- reactive({
    list.files("data/") %>% str_remove(".xlsx")
  })
  
  rank_all <- reactive({
    rank_df <- list()
    for (name_id in normal_students) {
      weekday_summary <- week_summary(batch_table(),name_id = name_id,mod ="weekday" ,
                                      month_data_list = month_data_list() ,types = weekday_type_commm)
      weekend_summary <- week_summary(batch_table(),name_id = name_id,mod ="weekend" ,
                                      month_data_list = month_data_list() ,types = weekend_type_commm)
      weeks_universe <- week_summary(batch_table(),name_id = name_id,mod ="universe" ,
                                     month_data_list = month_data_list() ,types = universe_type)
      df_templist <- list(weeks_universe,
                          weekend_summary,
                          weekday_summary)
      rank_df[[name_id]] <-  data.table::rbindlist(df_templist,fill = TRUE) %>% 
        gather(key = "month", value = "value", -types,-id)
    }
    rank_df <- data.table::rbindlist(rank_df,fill = TRUE)
  })
  
  personal_table <- reactive({
    df <- as.data.frame(rank_all())
    df <- df %>% 
      filter(id == input$name_id) %>% 
      filter(month == input$month_id)
  })

    output$distPlot <- renderPlot({
        df <- personal_table() %>% 
          filter(value != 0)
        
        ggplot(df, aes(x = month, y = value, fill = types)) +
          geom_bar(stat = "identity", position = "dodge") +
          geom_text(aes(label = ifelse(value == 0, "", value)), 
                    position = position_dodge(width = 0.9),vjust = -0.5) +
                      labs(x = "Month", y = "Value", fill = "Types") +
                      theme_bw()
    })
    output$selftable <- renderTable({
      df <- personal_table() %>% 
        select(-id) %>% 
        arrange(-value)
    })
    
    output$rankplot <- renderPlot({
      df <- rank_all()
      data_summary <- df %>%
        group_by(id, month) %>%
        summarize(total_value = sum(value)) %>%
        filter(total_value != 0) %>% 
        arrange(desc(total_value), desc(month))
      
      # 绘制条形图
      ggplot(data_summary, aes(x = total_value, y = month, fill = id)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = ifelse(total_value == 0, "", id)), position = position_dodge(width = 0.9), 
                  vjust = -0.5) +
        labs(x = "Value", y = "Month", fill = "ID") +
        theme_bw()
      
      
    })
    
    output$tables <-renderDT({
      df <- rank_all() %>% 
        arrange(-value)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
