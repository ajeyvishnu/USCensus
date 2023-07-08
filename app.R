library(shiny)
library(shinydashboard)
library(ggplot2)
library(readxl)
library(countrycode)
library(dplyr)
library(scales)
library(ggalt)
library(readr)
library(tidyr)
library(ggforce)
library(DT)
library(leaflet)
library(tidyverse)
library(ggmap)
library(leaflet.extras)
library(htmltools)
library(ggplot2)
library(maps)
library(mapproj)
library(mapdata)
library(plotly)
library(ggiraph)

pop_state <- read_excel("Population_2010_20.xlsx")

gen_age_yr <- read_excel("Gen_AgeGroup.xlsx", col_names = TRUE, sheet = "Sheet1")

ui <- dashboardPage(
  dashboardHeader(title = "US Census Analysis",
                  tags$li(class="dropdown",tags$a(href="https://www.linkedin.com/in/ajay-vishnu/", icon("linkedin"), "Profile", target="_blank")),
                  tags$li(class="dropdown",tags$a(href="https://github.com/ajeyvishnu", icon("github"), "Profile", target="_blank"))
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "Home", icon = icon("house")),
      menuItem("State & Region", tabName = "test101", icon = icon("flag-usa")),
      #selectInput(inputId = "Region", label = "Choose a Region:",
                  #choices = c("All", "West", "Midwest", "South" , "Northeast"), selected = "All"),
     #menuItem("By Gender & Age group", tabName = "test102", icon = icon("venus-mars")),
      menuItem("Age Group, Year, & Gender", tabName = "test103", icon = icon("calendar-days"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Home", 
              tabBox(id="t1", width = 12, 
                     tabPanel("About", icon=icon("address-card"),
                              fluidRow(
                                column(width = 8, 
                                       tags$img(src="uscensus.jpg", width =600 , height = 450),
                                       tags$br() , 
                                       tags$a("Source: https://www.altadenalibrary.org/census2020/"), align = "center"),
                                column(width = 4, tags$br() ,
                                       tags$b(style = "font-size: 30px;", "US Census Data Analysis"),
                                       tags$br() , 
                                       tags$br() , 
                                       tags$p("The US Census data from the official website has been downloaded and used for analysis"),
                                       tags$br() , 
                                       tags$p("The three main factors considered - Age Group, State, and Gender"),
                                       tags$br() , 
                                       tags$p("The data gathered was cleaned, and visualisations were created for better understanding and inferring from the data"),
                                       tags$br() , 
                                       tags$p("The data tables used have been shown in the windows right")
                                )
                              )
                     ), 
                     #tabPanel("Data Table 1", dataTableOutput("data101"), icon = icon("table")),
                     #tabPanel("Data Table 2", dataTableOutput("data102"), icon = icon("table")), 
                     #tabPanel("Data Table 3", dataTableOutput("data103"), icon = icon("table")), 
                     #tabPanel("Structure", verbatimTextOutput("structure"), icon=icon("uncharted")),
                     #tabPanel("Summary Stats", verbatimTextOutput("summary"), icon=icon("chart-pie"))
              )
              
      ),  
      tabItem(tabName = "test101",
              tabBox(id="t1", width = 12, 
                     tabPanel("About", icon=icon("address-card"),
                              fluidRow(
                                column(width = 12, tags$br() ,
                                       tags$b(style = "font-size: 30px;", "Change in Population from 2010 to 2020"),
                                       tags$br() , 
                                       tags$br() , 
                                       tags$p("The statewise population in the years 2010 and 2020 has been considered."),
                                       tags$br() , 
                                       tags$p("The states have been classified into 4 regions - Northeast, Midwest, West, and South."),
                                       tags$br() , 
                                       tags$p("The percentage difference between the years 2010 to 2020 has also been shown."),
                                       tags$br() , 
                                       tags$p("The states have been colour coded based on the percentage change in population from 2010 to 2020.")
                                )
                              )
                     ),
                     tabPanel("Visual",icon = icon("eye"),
                              fluidRow(
                                column(width = 12, 
                                       tags$div(
                                         style="text-align: center;",
                                         h3(style="font-weight:bold;", "Population Change by State & Region (2010-2020)")
                                       ),
                                       ggiraphOutput("map2", height = "600px")
                                )
                              )
                     ),
                     tabPanel("Inferences", icon=icon("lightbulb"),
                              fluidRow(
                                column(width = 12, tags$br() ,
                                       tags$b(style = "font-size: 30px;", "Inferences"),
                                       tags$br() , 
                                       tags$br() , 
                                       tags$p("1. The states in the Northeast and Midwest region have <2% and even negative population growth."),
                                       tags$br() , 
                                       tags$p("2. The West and South region states have >8% population growth."),
                                       tags$br() , 
                                       tags$p("3. Some of the reasons that could have contributed to this bifurcation:"),
                                       tags$p("a. Aging population: The Northeast and Midwest regions have an ageing population, with a higher proportion of people over 65. This means that there are fewer young people to replace the older population, leading to slower population growth.", style = "font-size: 12px;"),
                                       tags$p("b. Outward migration: The Northeast and Midwest regions have experienced outward migration, with people moving to other regions for better job opportunities, a lower cost of living, and a better quality of life.", style = "font-size: 12px;"),
                                       tags$p("c. Slower economic growth: The Northeast and Midwest regions have experienced slower economic growth in recent years, making it more difficult for people to find jobs and support their families.", style = "font-size: 12px;"),
                                       tags$p("d. Job opportunities: The South and West regions have experienced job growth in recent years, attracting people from other regions of the country in search of better job opportunities and higher salaries.", style = "font-size: 12px;"),
                                       tags$p("e. Lower cost of living: The South and West regions generally have a lower cost of living compared to the Northeast and Midwest regions, making it more affordable for people to live there.", style = "font-size: 12px;"),
                                       tags$p("f. Sunbelt migration: The South and West regions have experienced a 'Sunbelt migration' phenomenon, where people move from colder northern regions to warmer southern regions for retirement or to escape harsh winters.", style = "font-size: 12px;"),
                                       tags$p("g. Higher birth rates: The South and West regions generally have higher birth rates than the Northeast and Midwest regions, contributing to population growth.", style = "font-size: 12px;"),
                                       tags$br() , 
                                       tags$p("4. The future scope of this analysis can be to take a deeper dive into these hypotheses and validate them using underlying data and analysis.")
                                )
                              )
                     ),
                                          
                                           
             # tabBox(id="t1", width = 12, 
                     #tabPanel("Population Change (By numbers)", plotOutput("plot1", height = "600px"), icon = icon("arrow-up-right-dots")),
                     #tabPanel("Population Change Map (No.s)", plotlyOutput("map1", height = "600px"), icon = icon("map-location-dot")),
                     #tabPanel("Population Change (By %)", plotOutput("plot2", height = "600px"), icon = icon("arrow-up-right-dots")),
                     #tabPanel("Population Change Map (%)", plotlyOutput("map2", height = "400px"), icon = icon("map-location-dot"))
             )
              
      ),
      tabItem(tabName = "test103",
              tabBox(id="t1", width = 12, 
                     tabPanel("About", icon=icon("address-card"),
                              fluidRow(
                                column(width = 12, tags$br() ,
                                       tags$b(style = "font-size: 30px;", "Change in Population by Age Group, Year, and Gender"),
                                       tags$br() , 
                                       tags$br() , 
                                       tags$p("The statewise population in the years 2010 & 2020, for genders Male & Female, and different Age Groups have been considered."),
                                       tags$br() , 
                                       tags$p("The colour coding is done based on Gender (Male-Blue, Female-Red)"),
                                       tags$br() , 
                                       tags$p("The Age Groups are divided into 6 classes as follows:"),
                                       tags$p("--> Children: <9 years", style = "font-size: 12px;"),
                                       tags$p("--> GenZ: 9-24 years", style = "font-size: 12px;"),
                                       tags$p("--> Millenials: 25-40 years", style = "font-size: 12px;"),
                                       tags$p("--> GenX: 41-56 years", style = "font-size: 12px;"),
                                       tags$p("--> Baby Boomers: 57-75 years", style = "font-size: 12px;"),
                                       tags$p("--> Silent Gen: >75 years", style = "font-size: 12px;"),
                                       tags$br() , 
                                       tags$p("The visual is a grouped stacked bar chart.")
                                )
                              )
                     ),
                     tabPanel("Visual",icon = icon("eye"),
                              fluidRow(
                                column(width = 12, 
                                       tags$div(
                                         style="text-align: center;",
                                         h3(style="font-weight:bold;", "Population Change by Age Group & Year")
                                       ),
                                       ggiraphOutput("plot6", height = "600px")
                                )
                              )
                     ),
                     tabPanel("Inferences", icon=icon("lightbulb"),
                              fluidRow(
                                column(width = 12, tags$br() ,
                                       tags$b(style = "font-size: 30px;", "Inferences"),
                                       tags$br() , 
                                       tags$br() , 
                                       tags$p("1. Considering both the years 2010 and 2020, the following can be observed:"),
                                       tags$p("a. For children and GenZ (ages 0-24), the percentage of the male population is more than that of the female population by minimal margins.", style = "font-size: 12px;"),
                                       tags$p("b. For Millenials and GenX (ages 25-56), the percentage of male and female populations are almost similar.", style = "font-size: 12px;"),
                                       tags$p("c. For Baby Boomers and Silent Gen (ages 57 and above), the percentage of the female population is more than that of the male population by huge margins.", style = "font-size: 12px;"),
                                       tags$br() , 
                                       tags$p("2. Comparing the age group-wise populations for 2010 and 2020:"),
                                       tags$p("a. Children and GenX have seen a decrease in the overall population.", style = "font-size: 12px;"),
                                       tags$p("b. Rest four age groups have seen an increase in the population. Baby Boomers have seen the highest increase in population, followed by Millenials, Silent Gen, and GenZ.", style = "font-size: 12px;"),
                                       tags$br() , 
                                       tags$p("3. Overall, the US has the majority of its population in the GenX age group, followed by GenZ and Millenials, which are almost the same."),
                                       tags$br() , 
                                       tags$p("4. The future scope of this analysis can be to identify the organic growth in the agewise population based on birth and death and due to immigrants to the US.")
                                )
                              )
                     )
              )
      )
    )
  )
)

server <- function(input, output) {
  
  output$data101 <- renderDataTable(genage)
  output$data102 <- renderDataTable(agedata_df)
  output$data103 <- renderDataTable(pop_state)
  
  #mydata <- reactive({
    #pop_state$Code <- countrycode(pop_state$State, "country.name", "iso3c")
    #pop_state <- pop_state %>%
      #mutate(pop_diff = pop_diff, perc_diff = perc_diff) %>%
      #arrange(desc(pop_diff))
    
    #if (input$Region == "All") {
      #return(pop_state)
    #} else {
      #return(pop_state %>% filter(Region == input$Region))
    #}
  #})
  
  # get state data and arrests data
  states <- map_data("state")
  
  # adjust case for matching
  pop_state$region <- tolower(pop_state$State)
  
  # merge and sort (plots in order, sort ensures states filled in)
  pop_state.geo <- merge(states, pop_state, sort = FALSE, by = "region")
  pop_state.geo <- pop_state.geo[order(pop_state.geo$order), ]
  
  #output$map1 <- renderPlotly({
   # ggplot(pop_state.geo, aes(long, lat)) +
    #  geom_polygon(aes(group = group, fill = pop_state$Population_Difference, 
     #                  text = paste("State: ", pop_state.geo$State, "<br>",
      #                              "Population Difference: ", round(pop_state.geo$pop_diff/1000000,2), "M"))) +
      #coord_map() +
      #scale_fill_gradient2(low = "red3", mid = "white", high = "dodgerblue2", midpoint = 1500000,
       #                   guide = "colorbar", 
        #                  limits = c(min(pop_state.geo$pop_diff), max(pop_state.geo$pop_diff)),
         #                 breaks = seq(min(pop_state.geo$pop_diff), max(pop_state.geo$pop_diff), length.out = 5),
          #                labels = label_number(
           #                 accuracy = 0.01,
            #                scale = 1e-6,
             #               suffix = "M", length.out = 5),
              #            name = "Population Difference (in Mil)")+
     # xlab(NULL) + ylab(NULL)
 # })
  
  output$map2 <- renderGirafe({
    gg <- ggplot(pop_state.geo, aes(long, lat, group = group, fill = Percentage_Difference)) +
      geom_polygon_interactive(aes(tooltip = paste("State:", State, "<br>",
                                                   "Region:", Region, "<br>",
                                                   "Population Difference:", paste0(round(Population_Difference / 1000000, 2), "Mil"), "<br>",
                                                   "Percentage Difference:", paste0(round(Percentage_Difference * 100, 2), "%"))),
                                   #data_id = State)
                               stroke = 2, hover = T, 
                               alpha = 0.7, size = 0.1, unselectable = TRUE) +
      scale_fill_gradient2(low = "red3", mid = "white", high = "dodgerblue2", midpoint = 0.08,
                           guide = "colorbar", 
                           limits = c(min(pop_state.geo$Percentage_Difference), max(pop_state.geo$Percentage_Difference)),
                           breaks = seq(min(pop_state.geo$Percentage_Difference), max(pop_state.geo$Percentage_Difference), length.out = 5),
                           labels = label_number(
                             accuracy = 1,
                             scale = 100,
                             suffix = "%", length.out = 5)) +
      xlab(NULL) + ylab(NULL) +
      theme(panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "bottom",
            legend.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(10, 0, 10, 0))) +
      labs(fill = "")
    gg <- gg + borders("state", colour = "grey50", fill = NA)
    ggiraph(code = print(gg),
            hover_css = "fill-opacity: 0.7;")
  })
  
 # output$plot1 <- renderPlot({
  #  ggplot(mydata(), aes(x = reorder(State, pop_diff), y = pop_diff)) + 
   #   geom_col(fill = "#000080") +
    #       coord_flip() +
     #        scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
      #       labs(x = "State", y = "Population Difference")
       #    })
    
#  output$plot2 <- renderPlot({
 #     ggplot(mydata(), aes(x = reorder(State, perc_diff), y = perc_diff)) + 
  #      geom_col(fill = "#000080") +
   #     coord_flip() +
    #    scale_y_continuous(labels = scales::percent_format(scale = 100, accuracy = 1, suffix = "%")) +
     #   labs(x = "State", y = "Percentage Difference in Population")
  #  })
    
#    output$plot3 <- renderPlot({
#      ggplot(agedata_df, aes(x = Male_2010, xend = Male_Estimate, y = Label)) + 
#        geom_dumbbell(aes(color = "Change"), size = 1.5, color = "#4B9CD3", size_x = 4, size_xend = 4, 
#                      colour_x = "#FC4E07", colour_xend = "#FF9900", show.legend = TRUE) +
#        xlab("Population") +
#        ylab(NULL) +
#        scale_x_continuous(limits = c(1000000, 12000000), labels = function(x) format(x, scientific = FALSE)) +
#        scale_color_manual(values = c("#FC4E07", "#FF9900"), name = "Change") +
#        theme(legend.position = "bottom")
#    })
    
#    output$plot4 <- renderPlot({
#      ggplot(agedata_df, aes(x = Female_2010, xend = Female_Estimate, y = Label)) + 
#        geom_dumbbell(aes(color = "Change"), size = 1.5, color = "#4B9CD3", size_x = 4, size_xend = 4, 
#                      colour_x = "#FC4E07", colour_xend = "#FF9900", show.legend = TRUE) +
#        xlab("Population") +
#        ylab(NULL) +
#        scale_x_continuous(limits = c(1000000, 12000000), labels = function(x) format(x, scientific = FALSE)) +
#        scale_color_manual(values = c("#FC4E07", "#FF9900"), name = "Change") +
#        theme(legend.position = "bottom")
#    })
    
#    df_filtered <- reactive({
#      if (input$year == "Both") {
#        genage
#      } else {
#        genage %>% filter(genage$Year == as.numeric(input$year))
#      }
#    })
    
#    output$plot5 <- renderPlot({
#      ggplot(df_filtered(), aes(x = Label)) +
#        geom_bar(aes(y = Male), stat = "identity", fill = "blue", width = 0.4) +
#        geom_bar(aes(y = -Female), stat = "identity", fill = "red", width = 0.4) +
#        scale_y_continuous(expand = c(0, 0), limits = c(-25000000, 25000000), breaks = seq(-25000000, 25000000, by = 5000000),
#                           labels = function(x) format(abs(x), scientific = FALSE)) +
#        labs(x = "", y = "Value") +
#        coord_flip() +
#        theme_bw()
#    })
  
    output$plot6 <- renderGirafe({
      age_levels <- c("Childern(<9)","GenZ(9-24)", "Millenials(25-40)", "GenX(41-56)", "Baby Boomers(57-75)", "Silent Gen(>75)")
      gg1 <- ggplot(gen_age_yr, aes(x = Year, y = Population, fill = Gender, tooltip = paste("Gender: ", Gender, "<br>Year: ", Year, "<br>Age Group: ", AgeGroup, "<br>Population: ", paste0(round(Population/1000000, 2), "Mil")))) +
        geom_bar(stat = "identity",
                 position = "stack") +
        facet_grid(~ factor(AgeGroup, levels = age_levels))+
        theme(strip.text = element_text(size = 5), axis.text.x = element_text(size = 5),axis.text.y = element_text(size = 5))+
        scale_y_continuous(labels = function(x) paste0(round(x/1e6), "M"),
                           breaks = seq(0, 100, by = 10) * 1e6) +
        scale_x_continuous(breaks = c(2010, 2020)) +
        scale_fill_manual(values = c("red4", "dodgerblue4"))+
        geom_text_interactive(aes(label = paste0(round((Percentage*100),2), "%"), y = Population), 
                              position = position_stack(vjust=0.5), color = "white", fontface = "bold", size = 2, show.legend = FALSE)

      ggiraph(code = print(gg1),
              hover_css = "fill-opacity: 0.7; cursor: pointer;")
      })
}

shinyApp(ui = ui, server = server)

