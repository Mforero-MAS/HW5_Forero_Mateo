library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(knitr)
library(kableExtra)
library(sparkline)


raw_data <- read.table('state_market_tracker.tsv',sep ="\t", header = TRUE)
col_kept <- c("period_begin","period_end","state",
              "property_type","median_sale_price",
              "median_list_price", "homes_sold","avg_sale_to_list")
housing_data <- raw_data[col_kept]
housing_data$period_begin <- as.Date(housing_data$period_begin, "%Y-%m-%d")
housing_data$period_end <- as.Date(housing_data$period_end, "%Y-%m-%d")

t <- list(
  family = "Times New Roman",
  size = 19,
  color = "black")


text_summary <- "Below you can see property price information acquired by Redfin,
a real estate brokerage company that collects monthly market data. The motivation
for this dashboard was to give an overview of each states housing market that a novice
user can understand. The user can select the state they are interested in looking
at and use the date slider to zoom in to specific time ranges.  
This trend shows how different residential properties have increased 
in value over the last 10 years. Redfin collects the data in a per month
bases (i.e median sale price for Jan-2019). In the trend the first of the month
is used as a date value, as opposed to binning each month as the date range for
each observation."

plot1_summary <- "The following plot demonstrates selling price trended over the
last 10 years. As expected, property value trends upwards over time,
but residence type has a large impact on the variance of the price."

plot2_summary <- "This ratio shows the selling price over the listing price 
trended over the last 10 years. In simple terms, it can show a sellers market
(ratio > 1) vs a buyers market (ratio < 1). As expected, covid increased this ratio
in larger population states (e.g California) and had a smaller impact in smaller 
population states (e.g Kentucky)."

table1_summary <- "The table below allows for a easy to read table of statistics
for the state and year selected."

ui <- dashboardPage(
  # Format Page
  skin="red",
  
  # Dashboard Title
  dashboardHeader(title="Housing Market"),
  
  # Define Sidebar
  dashboardSidebar(
    # Sidebar Menu  
    sidebarMenu(
      selectInput("selectState","Select State: ",sort(unique(housing_data$state)),
                  selected = "Colorado"),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Summary Table", tabName = 'summary', icon = icon("th"))
    )),
  
  # Body Content
  dashboardBody(htmlwidgets::getDependency('sparkline'),
    # First tab content
    tabItems(
      tabItem("dashboard",
              h3("Mateo Forero - R Shiny Project"),
              h1("Housing Market Data for ",textOutput("state1", inline=TRUE)),
              h4(text_summary),
              h2("Median Sale Price Over the Last 10 Years"),
              h4(plot1_summary),
              box(plotlyOutput('p_timeseries'), width = 500),
              h2("Sale over Listing Price Ratio Over the Last 10 Years"),
              h4(plot2_summary),
              box(plotlyOutput('p_ratio'), width = 500)
      ),
      
      tabItem("summary",
              h2("Yearly Statistics for ",textOutput("state2", inline=TRUE)),
              h4(table1_summary),
              selectInput("selectyear","Select Year: ",
                          seq(2021,2012),
                          selected = 2022),
              box(tableOutput("small_table"),width = 400),
              h2("Difference between ",textOutput("state3", inline=TRUE),
                       "and the rest of the states"),
              box(tableOutput("long_table"),width = 400)
              
      ))))
    
  



server <- function(input, output) {
  
  # --------------------------------------------------
  # define the name for titling
  # --------------------------------------------------
  output$state1 <- renderText({input$selectState})
  output$state2 <- renderText({input$selectState})
  output$state3 <- renderText({input$selectState})
    
  
  # --------------------------------------------------
  # time series of name
  # --------------------------------------------------
  output$p_timeseries <- renderPlotly({
    
    in_state <- input$selectState
    
    p_medianPrice <- ggplot(housing_data %>% filter(state == in_state),
                            aes(x = period_begin, y = median_sale_price,
                                color = property_type, group = 1,
                                text = paste("Type:", property_type,
                                             "<br>Date:", format(period_begin, "%b-%Y"),
                                             "<br>Sale Price:",
                                             scales::dollar(median_sale_price)))) +
      geom_line() +
      scale_y_continuous(labels = scales::dollar_format()) +
      scale_x_date(date_breaks = '1 year', date_labels = "%Y") +
      theme_minimal()
    
    ggplotly(p_medianPrice, tooltip = 'text') %>%
      layout(font = t, legend = list(title=list(text='Property Types')), 
             xaxis = list(title = 'Date',
                          rangeslider = list(visible = T)),
             yaxis = list(title = 'Sale Price'))
    
  })
  
  output$p_ratio <- renderPlotly({
    
    in_state <- input$selectState
    
    
    
    p_ratio <- ggplot(housing_data %>% filter(state == in_state),
                      aes(x = period_begin, y = avg_sale_to_list,
                          color = property_type, group = 1,
                          text = paste("Type:", property_type,
                                       "<br>Date:", format(period_begin, "%b-%Y"),
                                       "<br>Ratio:", round(avg_sale_to_list,2)))) +
      geom_line() +
      geom_segment(aes(x= min(period_begin), xend= max(period_begin), y=1, yend=1),
                   color="black", linetype="dashed") +
      scale_x_date(date_breaks = '1 year', date_labels = "%Y") +
      theme_minimal()
    
    ggplotly(p_ratio, tooltip = 'text') %>%
      layout(font = t, legend = list(title=list(text='Property Types')), 
             xaxis = list(title = 'Date',
                          rangeslider = list(visible = T)),
             yaxis = list(title = 'Price Ratio'))
    
  })
 
  # --------------------------------------------------
  # Tables for Second page
  # --------------------------------------------------
  
  output$small_table <- function() {

    in_state <- input$selectState
    in_year <- as.character(input$selectyear)

    month_tab <- housing_data %>%
      mutate(year = format(period_begin, format = "%Y"),
             month = format(period_begin, format = "%m")) %>%
      filter(state == in_state, year == in_year)

    small_tab <- housing_data %>%
      mutate(year = format(period_begin, format = "%Y"),
             mon = format(period_begin, format = "%m")) %>%
      filter(state == in_state, year == in_year) %>%
      group_by(property_type) %>%
      summarise(sale_price = mean(median_sale_price),
                ratio = mean(avg_sale_to_list),
                sold = sum(homes_sold))


    for(p in month_tab$property_type){
      property_stat <- month_tab %>%
        filter(property_type == p) %>%
        arrange(month)
      
      small_tab[which(small_tab$property_type == p),"plt_price"] <- 
        spk_chr(property_stat$median_sale_price)
      
      small_tab[which(small_tab$property_type == p),"plt_ratio"] <- 
        spk_chr(round(property_stat$avg_sale_to_list,3))
      
      small_tab[which(small_tab$property_type == p),"plt_sum"] <- 
        spk_chr(property_stat$homes_sold) }
    
    
    small_tab$sale_price <- scales::dollar(small_tab$sale_price)
    small_tab$sold <- scales::comma(small_tab$sold)
    
    small_tab %>%
      select(property_type, sale_price,
             plt_price, ratio, plt_ratio,
             sold,plt_sum) %>%
      kbl(escape = FALSE,
          format = 'html',
          col.names = c("Property Type",
                        "Sale Price",
                        "Sale Trend",
                        "Sale/List Ratio",
                        "Ratio Trend",
                        "Homes Sold",
                        "# Sold Trend"),
          align = c("l","r","r","r","r","r","r"),
          digits = 3) %>%
      add_header_above(c("Yearly Average" = 7)) %>%
      kable_material(lightable_options = c("striped", "hover")) %>%
      kable_styling(fixed_thead = TRUE)
    
  }
   
  
  output$long_table <- function() {
    
    in_state <- input$selectState
    in_year <- as.character(input$selectyear)
    
    long_tab <- housing_data %>% 
      mutate(year = format(period_begin, format = "%Y"), 
             mon = format(period_begin, format = "%m")) %>%
      filter(property_type == "All Residential",
             year == in_year) %>%
      group_by(state) %>%
      summarise(sale_price = mean(median_sale_price),
                ratio = round(mean(avg_sale_to_list),3),
                sold = sum(homes_sold))
    sale_diff <- as.integer(long_tab[which(long_tab$state == in_state), 2])
    ratio_diff <- as.double(long_tab[which(long_tab$state == in_state), 3])
    sold_diff <- as.integer(long_tab[which(long_tab$state == in_state), 4])
    
    long_tab$sale_price <- long_tab$sale_price - sale_diff
    long_tab$ratio <- long_tab$ratio - ratio_diff
    long_tab$sold <- long_tab$sold - sold_diff
    
    long_tab <- long_tab[-which(long_tab$state == in_state),]
    
    long_tab$sale_price <- scales::dollar(long_tab$sale_price)
    long_tab$sold <- scales::comma(long_tab$sold)
    
    
    long_tab %>%
      kbl(col.names = c("Property Type",
                        "Sale Price",
                        "Sale/List Ratio",
                        "Homes Sold"),
          align = c("l","r","r","r")) %>%
      kable_material(lightable_options = c("striped", "hover")) %>%
      kable_styling(fixed_thead = TRUE)
    
  }
  
  
}

shinyApp(ui, server)