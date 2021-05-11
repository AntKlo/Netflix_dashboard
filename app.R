library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)

data = read.csv("netflix_titles.csv")
typeof(data)
#View(data)

a = table(data$type)
movies_number = a[1][1]
shows_number = a[2][1]
pr = select(data, 8, 1)
pr[2] = 1
pr$show_id = as.numeric(pr$show_id)
production_number = aggregate(. ~ release_year, data=pr, FUN=sum)
colnames(production_number)[which(names(production_number) == "release_year")] = "Release date"
colnames(production_number)[which(names(production_number) == "show_id")] = "Number of productions"


most_common_movie_director = "Jan Suter"
most_common_movie_directress = "Cathy Garcia-Molina"
most_common_show_director = "Alastair Fothergill"
most_common_show_directress = "Jung-ah Im"
most_common_movie_actor = "Samuel West"
most_common_movie_actress = "Iliza Shlesinger"
most_common_show_actor = "David Attenborough"
most_common_show_actress = "Anna Claire Bartlam"




ui = dashboardPage(
    dashboardHeader(title = tags$a(href = "NetflixDashboard", tags$img(src = "logo.jpg", height = '43', width = '50'))),
    dashboardSidebar(sidebarMenu(
        menuItem("map", tabName = "map", icon = icon("map")),
        menuItem("table", tabName = "table", icon = icon("table")),
        menuItem("Directors", tabName = "Directors", icon = icon("male"))
    )
    ),
    dashboardBody(
        tabItems(
            tabItem("map",
                fluidRow(h1("Number of productions available on Netflix with regards to world release date"), align = "center"),
                fluidRow(column(width = 4, infoBoxOutput("Movies", width = 12)), column(width = 4, infoBoxOutput("Shows", width = 12)),
                         column(width = 4, infoBoxOutput("Sum", width = 12))),
                fluidRow(plotlyOutput("rel_years", height = 400, width = "auto")),
                fluidRow(sidebarLayout(
                            sidebarPanel(sliderInput("Years", 
                                         label = "Release year",
                                         min = min(c(data$release_year)), max = max(c(data$release_year)),
                                         value = c(1925, 2021)), width = 12
                            ),
                    mainPanel(textOutput("Release_dates"))
                        )
                )
            ), 
            
            tabItem("table", fluidPage(dataTableOutput("mytable"))
            ),
            
            tabItem("Directors", fluidPage(
                sidebarLayout(
                    sidebarPanel(
                        selectInput("prod_type", "Production type",
                                    c(movie = "movie", show = "show")
                        )),
                    mainPanel(
                        tabsetPanel(
                            tabPanel("Most popular director", h3(textOutput("dir")), imageOutput("dir_img")),
                            tabPanel("Most popular actor", h3(textOutput("act")), imageOutput("act_img")),
                            tabPanel("Most popular directress", h3(textOutput("dir_w")), imageOutput("dir_img_w")),
                            tabPanel("Most popular actress", h3(textOutput("act_w")), imageOutput("act_img_w"))
                        )
                )),
        )
    )
)))
server = function(input, output){
    
    # Map tab
    output$rel_years = renderPlotly({ggplotly(ggplot(data = production_number, aes(x = `Release date`, y = `Number of productions`)) + theme_bw() +
                                      geom_bar(stat = "identity", fill = '#3182bd') + scale_x_continuous(limits=c(input$Years[1]-1, input$Years[2]+1)) +
                                      theme(axis.text=element_text(size=14), axis.title=element_text(size=16)))},
                                  )
    output$Movies = renderInfoBox({
        infoBox(
            "Movies", length(which(data$type == 'Movie' & data$release_year >= input$Years[1] & data$release_year <= input$Years[2])), icon = icon("film"),
            color = "purple"
        )
    })
    output$Shows = renderInfoBox({
        infoBox(
            "Shows", length(which(data$type == 'TV Show' & data$release_year >= input$Years[1] & data$release_year <= input$Years[2])), icon = icon("tv"),
            color = "blue"
        )
    })
    output$Sum = renderInfoBox({
        infoBox(
            "Sum", length(which(data$release_year >= input$Years[1] & data$release_year <= input$Years[2])), icon = icon("video"),
            color = "black"
        )
    })
    
    
    # Table tab
    output$mytable = renderDataTable({
        datatable(data, filter = 'top')
    })

    
    # Directors tab
    # Men
    output$dir = renderText({
        text_inp = input$prod_type
        if (text_inp == "movie"){
            paste(most_common_movie_director)
        }
        else{
            paste(most_common_show_director)
        }
    })
    
    output$dir_img = renderImage({
        text_inp = input$prod_type
        if (text_inp == "movie"){
            filename <- normalizePath(file.path('./images', paste('movie_dir.jpg')))
            # Return a list containing the filename
            list(src = filename, width = 500, height = 800)
        }
        else{
            filename <- normalizePath(file.path('./images', paste('show_dir.jpg')))
            # Return a list containing the filename
            list(src = filename, width = 500, height = 800)
        }
    }, deleteFile = FALSE)
    
    output$act = renderText({
        text_inp = input$prod_type
        if (text_inp == "movie"){
            paste(most_common_movie_actor)
        }
        else{
            paste(most_common_show_actor)
        }
    })
    
    output$act_img = renderImage({
        text_inp = input$prod_type
        if (text_inp == "movie"){
            filename <- normalizePath(file.path('./images', paste('movie_act.jpg')))
            # Return a list containing the filename
            list(src = filename, width = 500, height = 800)
        }
        else{
            filename <- normalizePath(file.path('./images', paste('show_act.jpg')))
            # Return a list containing the filename
            list(src = filename, width = 500, height = 800)
        }
    }, deleteFile = FALSE)
    
    # Women
    output$dir_w = renderText({
        text_inp = input$prod_type
        if (text_inp == "movie"){
            paste(most_common_movie_directress)
        }
        else{
            paste(most_common_show_directress)
        }
    })
    
    output$dir_img_w = renderImage({
        text_inp = input$prod_type
        if (text_inp == "movie"){
            filename <- normalizePath(file.path('./images', paste('movie_dir_w.jpg')))
            list(src = filename, width = 500, height = 800)
        }
        else{
            filename <- normalizePath(file.path('./images', paste('show_dir_w.jpg')))
            list(src = filename, width = 500, height = 800)
        }
    }, deleteFile = FALSE)
    
    output$act_w = renderText({
        text_inp = input$prod_type
        if (text_inp == "movie"){
            paste(most_common_movie_actress)
        }
        else{
            paste(most_common_show_actress)
        }
    })
    
    output$act_img_w = renderImage({
        text_inp = input$prod_type
        if (text_inp == "movie"){
            filename <- normalizePath(file.path('./images', paste('movie_act_w.jpg')))
            list(src = filename, width = 500, height = 800)
        }
        else{
            filename <- normalizePath(file.path('./images', paste('show_act_w.jpg')))
            list(src = filename, width = 500, height = 800)
        }
    }, deleteFile = FALSE)

}

shinyApp(ui, server)