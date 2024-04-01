library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(here)
options(shiny.usecairo = FALSE)

# source: https://stackoverflow.com/questions/48210709/show-content-for-menuitem-when-menusubitems-exist-in-shiny-dashboard/62834634#62834634
convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}

#### Define UI for application ####
ui <- dashboardPage(
  
  skin = "blue",
  
  dashboardHeader(
    title = span(tagList(icon("utensils"), "Restaurant Recommender")),
    titleWidth = 400),
  
  #### Sidebar content ####
  dashboardSidebar(
    width = 400,
    
    sidebarMenu(
      id = "sidebarid",
      
      menuItem("操作說明", tabName = "readme", icon = icon("book")),
      
      convertMenuItem(
        menuItem("餐廳推薦", tabName = "recommender", icon = icon("burger"),
                 
                 #### 哪一天要吃 ####
                 menuSubItem(icon = NULL,
                             dateInput("date", "哪一天要吃") #weekdays(as.Date("2023-05-22")) #"Monday"
                 ),

                 #### 評分 ####
                 menuSubItem(icon = NULL,
                             fluidRow(
                               column(4, checkboxInput("want_score", "評分")),
                               column(8, conditionalPanel("input.want_score==1",
                                                          sliderInput("score", NULL,
                                                                      min = 1, max = 5, value = c(3,4), step = 0.1)))
                             )
                 ),
                 
                 #### 均消 ####
                 menuSubItem(icon = NULL,
                             fluidRow(
                               column(4, checkboxInput("want_price", "均消")),
                               column(8, conditionalPanel("input.want_price==1",
                                                          sliderInput("ave_price", NULL,
                                                                      min = 0, max = 10000, value = c(500,1000))))
                             )
                 ),
                 
                 #### 類型 ####
                 menuSubItem(icon = NULL,
                             fluidRow(
                               column(4, checkboxInput("want_cate", "類型")),
                               column(8, conditionalPanel("input.want_cate==1",
                                                          selectInput("category", NULL,
                                                                      choices = list("早餐" = 1, "中式" = 2, "日式" = 3,
                                                                                     "韓式" = 4, "港式" = 5, "東南亞" = 6,
                                                                                     "美式" = 7, "中東" = 8, "歐式" = 9,
                                                                                     "非洲" = 10, "甜點.咖啡廳" = 11, 
                                                                                     "飲料" = 12, "酒吧" = 13))))
                             )
                 ),

                 #### 地點 ####
                 menuSubItem(icon = NULL,
                             fluidRow(
                               column(4, checkboxInput("want_district", "地點")),
                               column(8, conditionalPanel("input.want_district==1",
                                                          selectInput("district", NULL, # checkboxGroupInput
                                                                             choices = list("台北市信義區" = 1, "台北市大安區" = 2, "台北市文山區" = 3))))
                             )
                 ),
                 
                 #### run ####
                 menuSubItem(icon = NULL,
                             fluidRow(
                               column(7),
                               column(1,actionButton("go", "開始推薦", icon("pizza-slice"), style="color: #fff; background-color: #89B8CC; border-color: #6F8AB7"))
                             )
                 )
        ),"recommender"), # end recommender
      
      menuItem("開發人員", tabName = "developer", icon = icon("people-group"))
      
    ) # end sidebarMenu
  ), # end dashboardSidebar
  
  #### Body content ####
  dashboardBody(
    
    # source: https://stackoverflow.com/questions/45706670/shiny-dashboadpage-lock-dashboardheader-on-top
    # tags$script(HTML("$('body').addClass('fixed');")),
    
    tabItems(
      
      #### 操作說明 ####
      tabItem(tabName = "readme",
              h2("歡迎！"),
              br(),
              p("正在煩惱要吃什麼嗎？", align = "center"),
              p("網路上琳瑯滿目的餐廳評價不知道參考哪個才好嗎？", align = "center"),
              p("那請一定要來試試我們的",strong("餐廳推薦系統！"), align = "center"),
              br(),
              br(),
              p("只要選定用餐日期", align = "center"),
              p("餐廳的",strong("評分、均消、類型、地點"), align = "center"),
              p("皆可按照自身期待來制定篩選依據", align = "center"),
              p("按下「開始推薦」", align = "center"),
              p("即刻從 ",strong("ifoodie, Tripadvisor, Google Maps", style = "color: #3DB1E2")," 三個平台整合的資料庫中", align = "center"),
              p("推薦合適的高分餐廳給您", align = "center"),
              br(),
              br(),
              p("等不及要享用美食了嗎？", align = "center"),
              p("馬上就來試試看吧！", align = "center"), #now
      ),
      
      #### 推薦系統 ####
      tabItem(tabName = "recommender",
              h2("為您推薦..."),
              br(),
              DTOutput("filter_table")
      ),
      
      #### 開發人員 ####
      tabItem(tabName = "developer",
              h2("111-2統計諮詢課程期末報告"),
              br(),
              h4("小組名單"),
              br(),
              tableOutput("name")
      )
    ) # end tabItems
  ) # end dashboardBody
) # end dashboardPage

#### Define server ####
server <- function(input, output) {
  
  #### read dataset ####
  df = read.csv("restaurant_df.csv")
  
  cate = substr(colnames(df)[14:26], start=6, stop=11)
  new_cate = rep(NA, dim(df)[1])
  for (i in 1:dim(df)[1]){
    new_cate[i] = cate[df[i,14:26]==1] %>% str_c(collapse = ",") 
  }
  df$new_cate = new_cate
  
  df = df %>%
    mutate(opening = paste("週一 ", Monday, "\n",
                           "週二 ", Tuesday, "\n",
                           "週三 ", Wednesday, "\n",
                           "週四 ", Thursday, "\n",
                           "週五 ", Friday, "\n",
                           "週六 ", Saturday, "\n",
                           "週日 ", Sunday, sep=""))
  
  df$image_file[df$image_file==""]="transparent.png"
  
  #### filtering ####
  filtered_df = reactive({
    
    if (input$go > 0) {
      
      #### 篩選日期 ####
      wdays_col = which(colnames(df) == weekdays(as.Date(input$date)))
      filter_df = df[df[,wdays_col] != "休息",]
      
      #### 篩選評分 ####
      if (input$want_score == 1) {
        filter_df = filter_df %>%
          filter(score_w >= as.numeric(input$score[1]) & score_w <= as.numeric(input$score[2]))
      }
      
      #### 篩選均消 ####
      if (input$want_price == 1) {
        filter_df = filter_df %>%
          filter(ave_price >= as.numeric(input$ave_price[1]) & ave_price <= as.numeric(input$ave_price[2]))
      }
      
      #### 篩選類型 ####
      if (input$want_cate == 1) {
        cate_col = as.numeric(input$category)+which(colnames(df) == "cate_早餐")-1
        filter_df = filter_df[filter_df[,cate_col] == 1,]
      }
      
      #### 篩選地點 ####
      if (input$want_district == 1) {
        selected_district = c("1"="信義區", "2"="大安區", "3"="文山區")
        filter_df = filter_df %>% filter(區==selected_district[input$district])
      }
      
      #### 重整呈現 ####
      filter_df = filter_df %>%
        mutate(imgpath = map_chr(image_file, ~ as.character(img(src = .x, 
                                                                height = "67px", 
                                                                width = "98px")))) %>%
        mutate(score = round(score_w,2)) %>%
        select("--"=imgpath, "店名"=name_g, "評分"=score, "均消"=ave_price, "類型"=new_cate,
               "地址"=address, "當日營業時間"=weekdays(as.Date(input$date))) %>%
        arrange(desc(評分))
        
      
      return(filter_df)
    }
  })
  
  output$filter_table <- renderDT({
    datatable(filtered_df(),
              escape = FALSE, # source: https://stackoverflow.com/questions/72458575/r-shiny-download-multiple-local-images-in-zip-file
              options = list(scrollX = TRUE,
                             autoWidth = TRUE,
                             columnDefs = list(list(width = "10%", className = "dt-center", targets = c(1)),
                                               list(width = "15%", targets = c(2,5)),
                                               list(width = "5%", targets = c(3,4)),
                                               list(width = "25%", targets = c(6,7)))
                             )
              )
  })

  #### name list and thanks ####
  output$name = renderTable(
    data.frame(`學號` = c("111354011","111354012","111354014","111354020","111354027"),
               `姓名` = c("江瑞濬（組長）","林柏辰","劉貞莉","黃詩涵","黃政嘉")
               )
  )

}


#### Run the application ####
shinyApp(ui = ui, server = server)


