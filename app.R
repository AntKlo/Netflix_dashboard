library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)

# dataset
data = read.csv("netflix_titles.csv")


#--------FOR RELEASE YEARS TAB-----------
a = table(data$type)
movies_number = a[1][1]
shows_number = a[2][1]
pr = select(data, 8, 1)
pr[2] = 1
pr$show_id = as.numeric(pr$show_id)
production_number = aggregate(. ~ release_year, data=pr, FUN=sum)
colnames(production_number)[which(names(production_number) == "release_year")] = "Release date"
colnames(production_number)[which(names(production_number) == "show_id")] = "Number of productions"

#---------DIRECTORS AND ACTORS----------------------
most_common_movie_director = "Jan Suter"
most_common_movie_directress = "Cathy Garcia-Molina"
most_common_show_director = "Alastair Fothergill"
most_common_show_directress = "Jung-ah Im"
most_common_movie_actor = "Samuel West"
most_common_movie_actress = "Iliza Shlesinger"
most_common_show_actor = "David Attenborough"
most_common_show_actress = "Anna Claire Bartlam"


#-------------EXTRACTING FILM GENRES----------------

calculate_film_genres_count = function(data){
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
  return(film_genres_count)
}
film_genres_count = calculate_film_genres_count(data)
film_genres = names(sort(film_genres_count, decreasing = T))

#--------------------------------------------------
getChartColor = function(){
  return('#3182bd')
}
#------------BELOW I INCLUDED AGE CATEGORIES----------------
getAvailableAges = function(){
  return(c("little_kids", "older_kids", "teens", "mature"))
}
convertRatingToAgeCategory = function(data){
  age_categories = list(
    little_kids = c("G", "TV-Y", "TV-G"),
    older_kids = c("PG", "TV-Y7", "TV-Y7-FV", "TV-PG"), #7+
    teens = c("PG-13", "TV-14"),  #13+
    mature = c("R", "NC-17", "TV-MA", "NR", "UR", "") #18+ #NR and UR are "not rated"
  )
  ratings = data$rating
  ratings = sapply(ratings, function(x){
    for (name in names(age_categories)){
      if (x %in% age_categories[[name]]){
        return(name)
      }
    }
    return(x)
  })
  data$rating = ratings
  return(data)
}
df = convertRatingToAgeCategory(data)
#-------------------------------------------
convertDurationsToNumeric = function(df){
  durations = df$duration
  durations = unlist(lapply(durations, function(x)as.numeric(unlist(strsplit(x, " "))[1])))
  df$duration = durations
  return(df)
}
df = convertDurationsToNumeric(data)
#TO DO scatter plot: release year vs duration

#-----------------------------MAP DATA------------------------------------------
countries = read.csv("countries.csv")
countries_codes = select(countries, 1, 3)
colnames(countries_codes)[which(names(countries_codes) == "COUNTRY")] = "country"
countries_codes = cbind(countries_codes, Productions = 0)

countries_netflix = select(data, 6, 1)
countries_netflix[2] = 1
countries_netflix$show_id = as.numeric(countries_netflix$show_id)
country_productions = aggregate(. ~ country, data=countries_netflix, FUN=sum)

colnames(country_productions)[which(names(country_productions) == "show_id")] = "Productions"
country_productions = drop_na(country_productions)
country_productions = country_productions[-1,]

for(i in 1:nrow(countries_codes)){
  for(j in 1:nrow(country_productions)){
    if (country_productions$country[j] == countries_codes$country[i]){
      countries_codes$Productions[i] = country_productions$Productions[j]
    }
  }
}
#-------------------------------------------------------------------------------


ui = dashboardPage(
    dashboardHeader(title = tags$a(href = "https://www.netflix.com", tags$img(src = "logo.jpg", height = '43', width = '50'))),
    dashboardSidebar(sidebarMenu(
        menuItem("Release years", tabName = "release_years", icon = icon("calendar-alt")),
        menuItem("Table", tabName = "table", icon = icon("table")),
        menuItem("Directors&Actors", tabName = "Directors", icon = icon("male")),
        menuItem("Film genres", tabName = "film_genres", icon = icon("video")),
        menuItem("Map", tabName = "Map", icon = icon("map")),
        menuItem("Durations in years", tabName = "scatter_plots", icon = icon("chart-line")),
        menuItem("About", tabName = "About", icon = icon("info"))
    )),
    dashboardBody(tags$head(
      # Note the wrapping of the string in HTML()
      tags$style(HTML("
      .skin-blue .main-header .logo {
      background-color: #161a1d;
      }

      .skin-blue .main-header .logo:hover {
      background-color: #161a1d;
      }
      .skin-blue .main-header .navbar {
      background-color: #161a1d;
      }
      .skin-blue .main-sidebar {
      background-color: #161a1d;
      }
      .content-wrapper, .right-side {
      background-color: #e6e6e9;
      }
      .fa {
      color : #e6e6e9;
      }
      * { font-family: 'Arial', sans-serif; }"
        )
      )
    ),
        tabItems(
            tabItem("release_years",
                fluidRow(h2("Number of productions available on Netflix with regards to world release date"), align = "center"),
                fluidRow(column(width = 4, infoBoxOutput("Movies", width = 12)), column(width = 4, infoBoxOutput("Shows", width = 12)),
                         column(width = 4, infoBoxOutput("Sum", width = 12))),
                fluidRow(plotlyOutput("rel_years", height = 400, width = "auto")),
                fluidRow(sidebarLayout(
                            sidebarPanel(tags$style(".well {background-color: #e6e6e9;}"), sliderInput("Years", 
                                         label = "Release year",
                                         min = min(c(data$release_year)), max = max(c(data$release_year)),
                                         value = c(1925, 2021)), width = 12
                            ),
                    mainPanel(textOutput("Release_dates"))
                        )
                ),
                fluidRow(column(7, dataTableOutput("click_table")),
                         column(5, fluidRow(), plotOutput("click_plot")))
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
                  )
                  )),
            
            tabItem(tabName = "film_genres",
                    sidebarLayout(
                      sidebarPanel(width = 2,
                        checkboxGroupInput("film_genres", 
                                           "Film genres", 
                                           film_genres,
                                           selected = film_genres
                        )
                      ),
                      mainPanel(width = 9,
                        fluidRow(h2("Number of films from different genres"), align = 'center'),
                        plotlyOutput("genre_bar", width = "auto", height = 600)
                      )
                    )
                    ),
            
            tabItem(tabName = "scatter_plots", 
                    sidebarLayout(
                      sidebarPanel(width = 2,
                        selectInput(
                          "select_type","Select type:", choices = c("TV Show", "Movie"), selected = "Movies"
                          ),
                        checkboxGroupInput("choices_of_ages", "Chose ages:", choices = getAvailableAges(), selected = getAvailableAges())
                      ),
                      mainPanel(width = 9, tags$style(type="text/css",
                                                      ".shiny-output-error { visibility: hidden; }",
                                                      ".shiny-output-error:before { visibility: hidden; }"),
                        fluidRow(h2("Durations of films/shows in different years.", align = "center")),
                        plotlyOutput("scatter_plot", width = "auto", height = 600)
                      )
                      )
                    ),
            
            tabItem("Map",fluidRow(plotlyOutput("map", width = "auto", height = 600)),
                    fluidRow(dataTableOutput("click_table_map"))
            ),
            
            tabItem("About", fluidPage(htmlOutput("text")))
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
    output$click_table = renderDataTable({
      d <- event_data("plotly_click")
      if (!is.null(d)){
        datatable(
          filter(data[,c(-1)],release_year == d$x) , filter = 'top',
          options = list(
            pageLength = 3,
            lengthMenu = c(5,10,15,20,25,100),
            scrollX = T,
            columnDefs = list(list(
              targets = "_all",
              render = JS(
                "function(data, type, row, meta) {",
                "return type === 'display' && data.length > 30 ?",
                "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
                "}")
            )))
        )
      }
    })
    output$click_plot = renderPlot({
      d <- event_data("plotly_click")
      if (!is.null(d)){
        dat = filter(data, release_year == d$x)
        da = select(dat, 2, 1)
        da[2] = 1
        da = aggregate(. ~ type, data=da, FUN=sum)
        da = da %>% 
          arrange(desc(show_id)) %>%
          mutate(prop = round(show_id / sum(da$show_id) *100)) %>%
          mutate(ypos = cumsum(prop)- 0.5*prop )
        ggplot(da, aes(x="", y=prop, fill=type)) +
          geom_bar(stat="identity", width=1, color="white") +
          coord_polar("y", start=0) +
          theme_minimal() + labs(title = d$x, x = "", y = "") + 
          theme(legend.background = element_rect(fill="#e6e6e9"), plot.title = element_text(hjust = 0.5, size = 17),
                legend.title = element_text(size = 15), legend.text = element_text(size = 12),
                plot.background = element_rect(fill = "#e6e6e9", margin(0))) +

          scale_fill_brewer(palette="Set1")

      }
    }, bg="transparent")
    
    
    # Table tab
    output$mytable = renderDataTable({
        datatable(
          data[,c(-1)], filter = 'top',
          options = list(
            pageLength = 10,
            lengthMenu = c(5,10,15,20,25,100),
            scrollX = T,
            columnDefs = list(list(
            targets = "_all",
            render = JS(
              "function(data, type, row, meta) {",
              "return type === 'display' && data.length > 30 ?",
              "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
              "}")
          )))
        )
    })

    
    # Directors&Actors tab
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
    
    
    #Film genres
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
    
    # Map
    output$map = renderPlotly({
      plot_geo(countries_codes) %>% add_trace(
        z = ~Productions, color = ~Productions, colors = 'Blues', zmin = 0, zmax = 250,
        text = ~country, locations = ~CODE, marker = list(line = list(color = toRGB("grey"), width = 0.5)), showscale = FALSE) %>%
        layout(title = "Number of productions")
    })
    output$click_table_map = renderDataTable({
      d <- event_data("plotly_click")
      if (!is.null(d)){
        datatable(
          filter(data[,c(-1)], country == countries_codes$country[as.numeric(d[2])+1]) , filter = 'top',
          options = list(
            pageLength = 3,
            lengthMenu = c(5,10,15,20,25,100),
            scrollX = T,
            columnDefs = list(list(
              targets = "_all",
              render = JS(
                "function(data, type, row, meta) {",
                "return type === 'display' && data.length > 30 ?",
                "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
                "}")
            )))
        )
      }
    })
    
    # Durations in years
    output$scatter_plot = renderPlotly({
      df = convertDurationsToNumeric(data)
      type = input$select_type
      df = df[df$type == type,]
      chosen_ages = input$choices_of_ages
      df = convertRatingToAgeCategory(df)
      df = df[df$rating == chosen_ages,]
      if(type == "TV Show"){
        ylabel = paste("duration", "(seasons)")
      }
      else if (type == "Movie"){
        ylabel = paste("duration", "(minutes)")
      }
      ggplot(df, mapping = aes(release_year, duration)) + 
        geom_point(alpha = 3/10, color = getChartColor()) + 
        ylab(ylabel) + geom_jitter(alpha = 0.3, color = getChartColor()) +
        geom_smooth( se = T, color = getChartColor() ,method = "loess", alpha = 0.3, size = 0.3) +
        theme_bw()
      })
    
    # About
    output$text = renderUI({
      str1 = paste(h2("About"))
      str2 = paste("NetflixDashboard is a tool for analyzing productions available on Netflix.")
      str3 = paste("-> Release years tab presents productions with regards to release date. You can use slider to change range;
                   after clicking on a column table and pie plot appears below with data for choosen year.")
      str4 = paste("-> Table tab contains a table with whole dataset. Searching and sorting are available.")
      str5 = paste("-> In Directors&Actors tab you can see the most popular directors and actors. You can choose movie or TV show
                   and switch between differnet people.")
      str6 = paste("-> Film genres tab allows to see how many movies and TV-shows are from each genre. You can choose genres that
                   you are interested in.")
      str7 = paste("-> Map tab shows the world map with number of productions from each coutry. After clicking on a country
                   table appears below with productions from that country.")
      str8 = paste("-> In Durations in years tab user can choose his age range and see how many movies/TV-shows from each year
                   are available and what is their lengths. You can swap between movie and TV show. Ranges are as follows:")
      str8.1 = paste("little_kids > for everyone, older kids > 7+ years, tens > 13+ years, mature > 18+ years")
      str9 = paste("")
      str10 = paste("Source files and code available on github: ", tags$a(href="https://github.com/AntKlo/Netflix_dashboard", icon("github", "fa-2x")))
      str11 = paste("")
      str12 = paste("Dataset taken from kaggle: ", tags$a(href="https://www.kaggle.com/shivamb/netflix-shows", icon("kaggle", "fa-2x")))
      HTML(paste(str1, str2, str3, str4, str5, str6, str7, str8, str8.1, str9, str10, str11, str12, sep = '<br/>'))
    })
    

}

shinyApp(ui, server)