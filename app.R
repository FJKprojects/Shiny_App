library(shiny)
library(ggplot2)
library(scales)
library(bslib)
library(dplyr)
library(gridExtra)
library(grid)
library(countrycode)
library(tidyr)

ui <- fluidPage(
    theme = bs_theme(version = 4, bootswatch = "minty"),
    titlePanel(HTML("<div><center>Anime Data Dashboard</center></div>")),
    # Sidebar Layout 
    sidebarLayout(position = 'left',
        sidebarPanel(width = 3,
            # Sidebar For Tab 2
            conditionalPanel(condition="input.tabselected==2",h5("Customize your plot"),
                selectInput('plot_type', 'Select Plot Type:', choices=c('Barplot','Scatterplot', 'Piechart')),
                selectInput('fill', 'Select Fill Color:', choices=colors(), selected='darkorange'),
                selectInput('border', 'Select Border Color:', choices=colors(), selected='darkgreen'),
                selectInput('x_aes', 'Select X-axise Variable:', choices=names(anime_cleaned[-c(1:4,6,8,10:14,20)]), selected='genre'),
                selectInput('y_aes', 'Select Y-axise Variable(ScatterPlot):', choices=names(anime_cleaned[-c(1:6,8,16,17,20)]), selected='score')
            ),
            # Sidebar For Tab 1
            conditionalPanel(condition="input.tabselected==1", h5("Filter Anime Here"),
                                selectInput("genre", "Select Genre Type:", choices = unique(anime_cleaned$genre)),
                                selectInput("rating","Select Rating:", choices = unique(anime_cleaned$rating)),
                                selectInput("type","Select Boardcast Type:", choices = unique(anime_cleaned$type)),
                                selectInput("aired","Select Aired Status:", choices = unique(anime_cleaned$status)),
                             sliderInput("slider", label = h5("Year released"), min = 1942, max = 2018, value = c(1970,2018))),
            # Sidebar For Tab 6
            conditionalPanel(condition="input.tabselected==6", h5("Filter Data Here"),
                         selectInput("genre1", "Select Genre Type:", choices = c('All',unique(anime_cleaned$genre))),
                         selectInput("rating1","Select Rating:", choices = c('All',unique(anime_cleaned$rating))),
                         selectInput("type1","Select Boardcast Type:", choices = c('All',unique(anime_cleaned$type))),
                         selectInput("aired1","Select Aired Status:", choices = c('All',unique(anime_cleaned$status))),
                         sliderInput("slider1", label = h5("Year released"), min = 1942, max = 2018, value = c(1970, 2018))),
            # Sidebar For Tab 3
            conditionalPanel(condition="input.tabselected==3", h5("Customize User Plot Here"),
                             selectInput('fill1', 'Select Fill Color:', choices=colors(), selected='darkorange'),
                             selectInput('border1', 'Select Border Color:', choices=colors(), selected='darkgreen'),
                             selectInput('x_var','Select x variable:', choices=names(user[9,17:21]), selected='gender')),
            # Sidebar For Tab 4
            conditionalPanel(condition="input.tabselected==4", h5("User Plot of Behavior"),
                             selectInput('mean', 'Select a mean',choices = c('user_watching','user_completed', 'user_onhold','user_dropped','user_plantowatch',
                                                     'user_days_spent_watching'))),
            conditionalPanel(condition="input.tabselected==5", h4("World Map"))
            ),

        # Mainpanel
        mainPanel(
            tabsetPanel(type = "tabs", id = "tabselected", selected = 1, 
                        tabPanel("PostPreview", textOutput("text"), htmlOutput("image"),value = 1),
                        tabPanel("AnimePlot", plotOutput("barPlot"), value = 2),
                        tabPanel('UserPlot', plotOutput("user_plot"), value = 3),
                        tabPanel("UserAnylysis", plotOutput('plot'), value =4),
                        tabPanel("World Map", HTML('<h6>Map may take a while to show up...</h6>'),plotOutput('world_map'), value= 5),
                        tabPanel('Data', dataTableOutput("theData"), value =6)
           )
        )
    
)
)



# ============================Define server logic==============================
server <- function(input, output) {
    #==========================Ploting for anime data=======================
    output$barPlot = renderPlot({
        if(input$plot_type == 'Barplot'){    
            anime_cleaned%>%
                group_by_at(input$x_aes)%>%
                summarise(
                    count = n()
                )%>%
                filter(
                    count > 30
                )%>% 
                ggplot(aes_string(x = input$x_aes, y = 'count'))+
                geom_col(fill = input$fill, color = input$border, size = 1)+
                scale_y_continuous(labels = comma)+
                geom_text(aes(label = count), vjust = -0.2, size = 4,
                          position = position_dodge(0.9))+
                ggtitle(paste("Barplot for", input$x_aes))+
                theme(plot.title = element_text(size=22, hjust = 0.5, family = 'Verdana'))+
                labs(caption = "Data Source:Kaggle.com")}
        
        else if(input$plot_type == 'Scatterplot'){    
            anime_cleaned%>%
                ggplot(aes_string(x = input$x_aes, y = input$y_aes))+
                geom_point(fill = input$fill, color = input$border, size = 1)+
                scale_y_continuous(labels = comma)+
                ggtitle(paste("ScatterPlot for", input$x_aes,"&", input$y_aes))+
                theme(plot.title = element_text(size=22, hjust = 0.5, family = 'Verdana'))+
                labs(caption = "Data Source:Kaggle.com")}
        
        else if(input$plot_type == 'Piechart'){
            anime_cleaned%>%
                group_by_at(input$x_aes)%>%
                summarise(
                    count = n()
                )%>%
                filter(
                    count > 30
                )%>%
                ggplot( aes_string(x = factor(1), y = 'count', fill = input$x_aes))+
                geom_bar(stat = 'identity', width = 1, position = position_fill())+
                coord_polar('y')+
                theme_void() +
                theme(axis.text.x = element_blank(),
                      axis.ticks = element_blank(),
                      panel.grid = element_blank(), plot.title = element_text(size=22, hjust = 0.5, family = 'Verdana')
                )+
                ggtitle(paste("Piechart for", input$x_aes))+
                labs(caption = "Data Source:Kaggle.com")}
        
    })
    
    #============================DataTable output=================================
    output$theData = renderDataTable(
        plotData()%>%
            select(
                -actual_url,
                -image_url,
                -anime_id
            )%>%
            filter(aired_from_year %in% input$slider1[1]:input$slider1[2]) #sidebar user input to filter by a silder
    )
    
    
   #===============================Post Poster output==================================
    output$image = renderText({
        test = anime_cleaned%>%
            filter(genre == input$genre &
                    rating == input$rating &
                    type == input$type &
                    status == input$aired)%>%
                    arrange(desc(score))%>%
                    filter(aired_from_year %in% input$slider[1]:input$slider[2])%>%  #sidebar user input to filter by a silder
            slice(1:50)
        #get rows
        med_row = anime_cleaned%>%
            filter(genre == input$genre &
                       rating == input$rating &
                       type == input$type &
                       status == input$aired
                        )%>%
                       filter(aired_from_year %in% input$slider[1]:input$slider[2])%>%
                        arrange(desc(score))
        #define the row number variable to use in html h1 tag here.
        row_num = nrow(med_row)
        #html style coding here to get TOP30 scoreed anime's post by it's image_url(clended in cleaning file) and put their title under poster.
        p = paste0("<figure><a href='", test$actual_url,"'>", "<img src='",test$image_url,"' width='200' height='300'", "title='", test$title,"' alt='We did not not find poster'/></a><figcaption><a href='",test$actual_url,"'>", substr(test$title,1,20), "</a></figcaption></figure>", collapse = '')
        p = paste("<h2>Found",row_num,"Results. Showing Top50</h2><div>Click poster or title to open the anime website.Filter at left.Check full name by mousehover</div>","<style>img{margin: 5px;} figure{float:left;}</style><div>", p,"</div>")
    })
    # =============================adding all choice===========================
    plotData = reactive({
        rows = T  
        if(input$genre1 != 'All'){rows=rows & anime_cleaned$genre == input$genre1}
        if(input$rating1 != 'All'){rows=rows & anime_cleaned$rating == input$rating1} 
        if(input$type1 != 'All'){rows=rows & anime_cleaned$type == input$type1}
        if(input$aired1 != 'All'){rows=rows & anime_cleaned$status == input$aired1}
        anime_cleaned[rows, ]%>%
            select(-title)
        }) # same as above for Rating
    # =============================User plot===========================
    output$user_plot <- renderPlot({
        user %>%
            group_by_at(input$x_var) %>%
            summarise(
                count = n()
            ) %>%
            ggplot(aes_string(x= input$x_var, y= 'count')) +
            geom_col(fill = input$fill1, color = input$border1, size = 1)+
            scale_y_continuous(labels = comma)+
            geom_text(aes(label = count), vjust = -0.2, size = 4,
                      position = position_dodge(0.9))+
            ggtitle(paste("Barplot for", input$x_var))+
            theme(plot.title = element_text(size=22, hjust = 0.5, family = 'Verdana'))+
            labs(caption = "Data Source:Kaggle.com")
    })
    # ===============User anlysis plot======================
    output$plot = renderPlot(
        user%>%
            group_by(age_group,gender,continent)%>%
            summarise(
                user_watching = mean(user_watching),
                user_completed = mean(user_completed),
                user_onhold = mean(user_onhold),
                user_dropped= mean(user_dropped),
                user_plantowatch = mean(user_plantowatch),
                user_days_spent_watching = mean(user_days_spent_watching)
            )%>%
            ggplot(aes_string(x = 'continent',y = input$mean, color = 'age_group',group = 'age_group', fill = 'age_group',shape = 'age_group')) +
            geom_line(size = 1.5)+geom_point(size = 4)+
            facet_grid(gender~.)+
            scale_y_continuous(labels = comma)+
            ggtitle(paste("Linechart for", substr(input$mean, 6, 30)))+
            labs(caption = "Data Source:Kaggle.com")+
            theme(
                strip.background = element_rect(fill = "#fedcbd"),plot.title = element_text(size=22, hjust = 0.5, family = 'Verdana'),
                strip.text.y = element_text(size = 14, angle = -90, face = "bold")
            ),height = 600
    )
    
    # ===============Heat map code =====================
    output$world_map = renderPlot(
        grid.arrange(
            ggplot(user3) +
                geom_map(
                    dat = world_map, map = world_map, aes(map_id = region),
                    fill = "white", color = "#7f7f7f", size = 0.25
                ) +
                geom_map(map = world_map, aes(map_id =country, fill = number), size = 0.25) +
                scale_fill_gradient(low = "#fff7bc", high = "#cc4c02", name = "Total Cases") +
                expand_limits(x = world_map$long, y = world_map$lat)+
                facet_grid(. ~ title)+
                labs(x = 'Latitude', y = 'longitude')+
                theme(plot.title = element_text(hjust = 0.5)),
            ggplot(user2[user2$age_group %in% c('<18'),]) +
                geom_map(
                    dat = world_map, map = world_map, aes(map_id = region),
                    fill = "white", color = "#7f7f7f", size = 0.25
                ) +
                facet_grid(.~ age_group)+
                geom_map(map = world_map, aes(map_id =country, fill = number), size = 0.25) +
                scale_fill_gradient(low = "#fff7bc", high = "#cc4c02", name = "Total Cases") +
                expand_limits(x = world_map$long, y = world_map$lat)+
                labs(x = 'Latitude', y = 'longitude')
            ,
            ggplot(user2[user2$age_group %in% c('18-30'),]) +
                geom_map(
                    dat = world_map, map = world_map, aes(map_id = region),
                    fill = "white", color = "#7f7f7f", size = 0.25
                ) +
                facet_grid(.~ age_group)+
                geom_map(map = world_map, aes(map_id =country, fill = number), size = 0.25) +
                scale_fill_gradient(low = "#fff7bc", high = "#cc4c02", name = "Total Cases") +
                expand_limits(x = world_map$long, y = world_map$lat)+
                labs(x = 'Latitude', y = 'longitude'),
            ggplot(user2[user2$age_group %in% c('>30'),]) +
                geom_map(
                    dat = world_map, map = world_map, aes(map_id = region),
                    fill = "white", color = "#7f7f7f", size = 0.25
                ) +
                facet_grid(.~ age_group)+
                geom_map(map = world_map, aes(map_id =country, fill = number), size = 0.25) +
                scale_fill_gradient(low = "#fff7bc", high = "#cc4c02", name = "Total Cases") +
                expand_limits(x = world_map$long, y = world_map$lat)+
                labs(x = 'Latitude', y = 'longitude'),
            heights = c(1,1),top = textGrob('World Heat Map for Users Distribution', gp = gpar(fontsize = 40,font=3))),height = 600
    )
}# end line for server code==========================
    
# Run the application 
shinyApp(ui = ui, server = server)
