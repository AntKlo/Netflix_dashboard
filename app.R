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


#-------------EXTRACTING FILM GENRES----------------
df = data.frame(data)

listed_in = df$listed_in
listed_in = lapply(listed_in, function(x){unlist(strsplit(x, ', '))})
df$listed_in = listed_in
rm(listed_in)
get_film_genres = function(df){
  return(unique(unlist(df$listed_in)))
}
get_film_genres_count = function(df){
  film_genres = get_film_genres(df)
  film_genres_count = rep(0, times = length(film_genres))
  names(film_genres_count) = film_genres
  for (i in df$listed_in){
    for (j in i){
      film_genres_count[j] = film_genres_count[j] + 1
    }
  }
  return(film_genres_count)
}
film_genres_count = get_film_genres_count(df)
sort(film_genres_count)
#film_genres_count = na.omit(film_genres_count[film_genres_count >= 100])

equal_names = list( 
  Dramas = c("Dramas", "TV Dramas"),
  Comedies = c("Comedies", "TV Comedies", "Stand-Up Comedy & Talk Shows", "Stand-Up Comedy"),
  Thrillers = c("Thrillers", "TV Thrillers"),
  Documentaries = c("Documentaries", "Docuseries"),
  Anime = c("Anime Features", "Anime Series"),
  Kids = c("Kids' TV", "Children & Family Movies", "Teen TV Shows"),
  Action_and_Adventure = c("TV Action & Adventure", "Action & Adventure"),
  Science_Fiction_and_Fantasy = c("TV Sci-Fi & Fantasy", "Sci-Fi & Fantasy"),
  Romantic = c("Romantic TV Shows", "Romantic Movies"),
  Horror = c("Horror Movies", "TV Horror"),
  Classic_and_Cult = c("Cult Movies","Classic Movies","Classic & Cult TV")
)
####renaming some categories using equal_names------------------------------
df$listed_in = lapply(df$listed_in, function(x){
  for (name in names(equal_names)){
    for (elem in equal_names[[name]]){
      x[match(elem, x)] = name
    }
  }
  return(x)
})
sort(get_film_genres_count(df))
unnecesary_genres = c(
  "TV Shows", "Movies", "International TV Shows", "International Movies"
)
df$listed_in = lapply(df$listed_in, function(x){
  for (genre in unnecesary_genres){
    x = x[x!=genre]
  }
  return(x)
})
sort(get_film_genres_count(df))
film_genres_count = get_film_genres_count(df)
film_genres = names(sort(film_genres_count, decreasing = T))
hist(film_genres_count)
#--------------------------------------------------

#------------BELOW I INCLUDED AGE CATEGORIES----------------
little_kids = c("G", "TV-Y", "TV-G")
older_kids = c("PG", "TV-Y7", "TV-Y7-FV", "TV-PG") #7+
teens = c("PG-13", "TV-14")  #13+
mature = c("R", "NC-17", "TV-MA") #18+
#-------------------------------------------


ui = dashboardPage(
    dashboardHeader(title = tags$a(href = "NetflixDashboard", tags$img(src = "logo.jpg", height = '43', width = '50'))),
    dashboardSidebar(sidebarMenu(
        menuItem("Release years", tabName = "release_years", icon = icon("calendar-alt")),
        menuItem("Table", tabName = "table", icon = icon("table")),
        menuItem("Directors", tabName = "Directors", icon = icon("male")),
        menuItem("Film genres", tabName = "film_genres", icon = icon("video"))
    )
    ),
    dashboardBody(
        tabItems(
            tabItem("release_years",
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
                    )
                  ),
              
                  )),
            tabItem(tabName = "film_genres",
                    sidebarLayout(
                      sidebarPanel(
                        checkboxGroupInput("film_genres", 
                                           "Film genres", 
                                           film_genres,
                                           selected = film_genres
                        )
                      ),
                      mainPanel(
                        fluidRow(h2("Number of films from different genres"), align = 'center'),
                        plotlyOutput("genre_bar")
                      )
                    )
                    
                    )
)))
server = function(input, output){
    
    # Release years tab
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
        datatable(
          data[,c(-1)], filter = 'top',
          options = list(
            pageLength = 5,
            lengthMenu = c(5,10,15,20,25,100),
            scrollX = T,
            #autoWidth = T,
            #scrollY = T,
            #fixedColumns = T,
            columnDefs = list(list(
            #width = '50px',
            targets = "_all",# can be c(4, -1,-2)
            render = JS(
              "function(data, type, row, meta) {",
              "return type === 'display' && data.length > 30 ?",
              "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
              "}")#sets max of 30 characters to be displayed in a datatable columns specified by targets
          )))
        )
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
            filename <- normalizePath(file.path('./www', paste('movie_dir.jpg')))
            list(src = filename, width = 400, height = 650)
        }
        else{
            filename <- normalizePath(file.path('./www', paste('show_dir.jpg')))
            list(src = filename, width = 400, height = 650)
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
            filename <- normalizePath(file.path('./www', paste('movie_act.jpg')))
            list(src = filename, width = 400, height = 650)
        }
        else{
            filename <- normalizePath(file.path('./www', paste('show_act.jpg')))
            list(src = filename, width = 400, height = 650)
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
            filename <- normalizePath(file.path('./www', paste('movie_dir_w.jpg')))
            list(src = filename, width = 400, height = 650)
        }
        else{
            filename <- normalizePath(file.path('./www', paste('show_dir_w.jpg')))
            list(src = filename, width = 400, height = 650)
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
            filename <- normalizePath(file.path('./www', paste('movie_act_w.jpg')))
            list(src = filename, width = 400, height = 650)
        }
        else{
            filename <- normalizePath(file.path('./www', paste('show_act_w.jpg')))
            list(src = filename, width = 400, height = 650)
        }
    }, deleteFile = FALSE)
    
    output$genre_bar = renderPlotly({
        film_genre =input$film_genres
        count = sort(film_genres_count, decreasing = T)
        count = count[names(count) %in% film_genre]
        film_genre = reorder(film_genre, count)
        breaks = film_genre[seq(1,length(film_genre), length.out = 4)]
        if(!is.na(breaks[1])){
          ggplot(NULL) + xlab("Genre") + ylab("Count") + 
            geom_bar(aes(x = film_genre, y = count), stat = "identity", fill = '#3182bd') + 
            theme_bw() + scale_x_discrete( breaks = breaks)
        }
      }
    )
    

}

shinyApp(ui, server)

