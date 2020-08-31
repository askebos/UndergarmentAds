#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(zoo)
library(knitr)
library(htmltools)
library(forcats)
library(captioner)
library(bsplus)
library(markdown)

# Dashboard
library(shinydashboard)

#Interactive libraries
library(DT)
library(plotly)
library(viridis)

#Set up data
source("data/setupData.R")
source("data/setupFunctions.R")

#Prepare captabase

allAds <- allAds %>%
    droplevels()

publication_levels <- allAds %>%
    select(Publication) %>%
    distinct(Publication) %>%
    dplyr::transmute(title = Publication,
                     pubnumber = 1:n()) %>%
    dplyr::mutate(Publication_Color = viridis(n())) %>%
    mutate(Publication_Color = substring(Publication_Color, 1, 7))


collection_dates <- publications %>%
    filter(!is.na(start)) %>%
    select(Publication, start, end) %>%
    dplyr::mutate(set = 1:n()) %>%
    pivot_longer(cols = c(start, end), names_to = "datstat") %>%
    group_by(Publication, set) %>%
    complete(value = seq.Date(min(value), max(value), by = "month")) %>%
    left_join(., publication_levels, by = c("Publication" = "title"))

#Set colours for the brands
brand_colors <- allAds %>%
    dplyr::count(Brand) %>%
    dplyr::mutate(Brand_Color = case_when(n > 250 ~ viridis(n()),
                                          TRUE ~ "#BEBEBE")) %>%
    mutate(Brand_Color = substring(Brand_Color, 1, 7)) %>%
    select(-n) %>%
    mutate(Brand_Legend = case_when(Brand_Color == "#BEBEBE" ~ "Other",
                                    TRUE ~ Brand)) %>%
    mutate(Brand_Color = paste0(Brand_Color, 'BF'))

captabase <- allAds  %>%
    mutate(Volume.Issue = case_when(
        Volume.Issue == NA ~ paste0(Volume, Issue, sep = "."),
        TRUE ~ Volume.Issue
    )) %>%
    mutate(
        Publication = factor(Publication),
        Archive = factor(Archive),
        Brand = factor(Brand),
        Tagline = factor(Tagline),
        Product = factor(Product),
        Campaign = factor(Campaign),
        Department.Store = factor(Department.Store),
        Collection = factor(Collection),
    ) %>%
    left_join(., publication_levels, by = c("Publication" = "title")) %>%
    left_join(., brand_colors, by = c("Brand" = "Brand")) %>%
    mutate(Publication_Color = case_when(Brand == "maidenform" ~ paste0(Publication_Color,'D9'),
                                         TRUE ~ paste0(Publication_Color,'40')))

publications <- publications %>%
    filter(Publication %in% unique(captabase$Publication))


shinyServer(function(input, output) {

# MENU ========================
    
output$filter_publications <- renderMenu({
    selectizeInput(
        inputId = "Publications",
        label = "Filter publications",
        choices = list(Magazines = publications[which(publications$Type== "Magazine"),'Publication'],Newspapers = publications[which(publications$Type== "Newspaper"),'Publication']),
        selected = c("Mademoiselle", "Vogue","Harper's Bazaar","Ladies' Home Journal", "McCall's"),
        multiple = TRUE,
        options = list(loadThrottle = 1000)
    )
})  


# REACTIVE CAPTABASE   =====================================

filtered <- reactiveValues(captabase = captabase,
                           collection_dates= collection_dates)

    observeEvent(input$Publications, {
        filtered$captabase <- captabase %>%
            filter(Publication %in% input$Publications)
        
        filtered$collection_dates <- collection_dates %>%
            filter(Publication %in% input$Publications)
        
    })
    

# CAPTABASE (interface section) =====================================
    
    
    output$captabase <- renderDT(
        filtered$captabase %>%
            select(Publication, Date, Brand, Archive, URL, Campaign, Tagline, Product, Volume.Issue, Page) %>% # Select and reorder for easier display 
            mutate(URL = case_when(URL != '' ~ paste0("<a href='",URL, "' target = '_blank'><i class='fa fa-link'></i></a>"), 
                                   TRUE ~ ''),
                   Brand = gsub(',', ', ', Brand)) %>% #Add space after comma for brand lists to allow the column to wrap
            DT::datatable(
                rownames = FALSE,
                #editable = TRUE,
                class = 'stripe hover order-column',
                escape = FALSE,
                style = 'bootstrap',
                selection = 'none',
                filter = 'top',
                options = list(lengthChange = TRUE, pageLength = 5, scrollX = T),
                caption = 'Each line is one observation in my collection of bra and girdle advertisements. You can filter, reorder, and search this table.'
            )
    )
    

# ARCHIVES =====================================
    
    shapes <-   c(16,17,8,3,12)
    names(shapes) <- c("Digital manual","Digital search","Machine learning","Manual","ProQuest")
    
    collection_dates <- publications %>% 
        filter(!is.na(start))%>%
        select(Publication,start,end) %>%
        dplyr::mutate(set = 1:n()) %>%
        pivot_longer(cols = c(start,end), names_to = "datstat")%>%
        group_by(Publication, set) %>%
        tidyr::complete(value = seq.Date(min(value), max(value), by="month")) %>%
        left_join(., publication_levels, by = c("Publication" = "title"))
    
    #NOTE: This plot is much more convoluted than necessary, because there is a strange bug with the color and opacity variables if plotted without looping through each trace (which then means we also need to create a separate 'empty/hidden' trace to unify the legend)


    # Dataframe for a trace that will only be used to show the Collection legend
    legend <- captabase %>%
        group_by(Collection) %>% sample_n(size = 1) %>%
        mutate(pubnumber = 1000) # To hide the marks far above the plot
    
output$Archive <- renderPlotly({  
    P <- plot_ly()%>%
        add_lines( # Collection dates lines
            data = filtered$collection_dates,
            x = ~ value,
            y = ~ pubnumber,
            mode = "lines",
            name = "Dates collected",
            line = list(color = "#bac7db", width = 2), 
            hoverinfo= '',
            showlegend= F
        ) %>%
        add_trace(data = legend,     # 'Empty' legend trace
                  x = ~ Date,
                  y = ~ pubnumber,
                  type = "scatter",
                  mode = "markers",
                  marker = list(size = 6,     
                                color = 'grey'),
                  symbol = ~Collection,
                  symbols = shapes,
                  legendgroup = ~Collection) %>%
        rangeslider() %>%
        layout(
            xaxis = list(title = ''),
            yaxis = list(
                title = '',
                tickmode = 'array',
                range = c(0, max(filtered$captabase$pubnumber)+1), #Set the y axis so that the graph doesn't move + marks on the legend trace remain hidden
                tickvals = filtered$captabase$pubnumber %>% unique,
                ticktext = filtered$captabase$Publication %>% unique
            )
        ) 

    
    #Loop to add traces for each publication, for all brands except maidenform (for opacity)
    for(k in unique(filtered$captabase$Publication)) {
        P <- add_trace(P, data = filtered$captabase %>% filter(Publication == k & Brand != "maidenform"),     
                       x = ~ Date,
                       y = ~ jitter(as.numeric(pubnumber),factor = 1),
                       type = "scatter",
                       mode = "markers",
                       marker = list(size = 6, 
                                     color = ~Publication_Color),
                       symbol = ~Collection,
                       symbols = shapes,
                       hoverinfo = "text",
                       text = ~ paste0(Publication,", ", format(Date, "%B %Y"),
                                       "<br>Brand: ", Brand,
                                       "<br>Collection mode: ", Collection),
                       legendgroup = ~Collection, 
                       showlegend = F)
    
        #And add traces for each publication, for maidenform only
        P <- add_trace(P, data = filtered$captabase %>% filter(Publication == k & Brand == "maidenform"),     
                       x = ~ Date,
                       y = ~ jitter(as.numeric(pubnumber),factor = 1),
                       type = "scatter",
                       mode = "markers",
                       marker = list(size = 6, 
                                     color = ~Publication_Color),
                       symbol = ~Collection,
                       symbols = shapes,
                       hoverinfo = "text",
                       text = ~ paste0(Publication,", ", format(Date, "%B %Y"),
                                       "<br>Brand: ", Brand,
                                       "<br>Collection mode: ", Collection),
                       legendgroup = ~Collection, 
                       showlegend = F)
    }

    P
    
})  


# DREAMS  =====================================

    Dreams <-
        allAds %>% #use of all publications to get a more complete picture of the campaign itself
        filter(Campaign == "dreams" & is.na(Exclude)) %>%
        select(Tagline, Date, Publication) %>%
        group_by(Tagline) %>%
        plyr::count() %>%
        group_by(Tagline) %>%
        transmute(
            start = min(Date),
            end = max(Date),
            pub_num = n_distinct(Publication),
            freq = sum(freq)
        ) %>%
        unique() %>%
        mutate(duration = as.numeric(round(difftime(
            end, start, units = "weeks"
        )))) %>%
        mutate(duration = case_when(duration == 0 ~ 1,
                                    TRUE ~ duration)) %>%
        left_join(., dreams.master[, c('tagline', 'slug', 'format', 'pun')], by = c("Tagline" = "slug")) %>%
    mutate(Year= format(start,"%Y"))
    
    max_pub_num <- Dreams$pub_num %>% max
    
    
    output$Timeline <- renderPlotly({
       dreams <-  plot_ly(
           Dreams,
            x = ~ start,
            y = ~ freq,
            type = 'scatter',
            mode = 'markers',
            color = ~ format,
            colors = 'viridis',
            marker = list(sizemode = 'diameter', opacity = 0.5),
            size = ~ duration,
            sizes = c(10,50),
            hovertemplate =  ~ paste(
                "Tagline:",
                tagline,
                "<br>First published:",
                start,
                "<br>Last published:",
                end
            )
        ) %>%
            layout (yaxis = list(title = "Frequency"),
                    xaxis = list(title = ""),
                    legend=list(title=list(text='<b>Format</b>')))

       if(input$showLabels){
        dreams <- dreams %>%
            add_text(
                text = ~Tagline,
                textfont = list(size = 9, color = "grey"), 
                textposition = "top"
            ) }
       
       dreams
        
    })


g <- list(
    showland = TRUE,
    landcolor = toRGB("gray95"),
    subunitcolor = toRGB("white"),
    countrycolor = toRGB("gray85"),
    countrywidth = 0.5,
    subunitwidth = 0.5,
    showcountries = TRUE,
    showcoastlines = FALSE,
    showframe = F
)


markersFiltered <- reactive({
    w <- filter(windows,
                Start >= input$Windows.Year[1],
                End <= input$Windows.Year[2])
    return(w)
})

window_Colors <- viridis(length(unique(windows$Type)))
names(window_Colors) <- unique(windows$Type)

window_Shapes <- c("star", "circle","square","triangle-up","diamond","asterisk-open")
names(window_Shapes) <- unique(windows$Type)

# Server
output$Map <- renderPlotly({
    map <- plot_geo(
        markersFiltered(),
            lat = ~ jitter(Lat, factor = 10),
            lon = ~ jitter(Long, factor = 10)
        ) %>% # Introduction jitter to separate points that have the same coordinates (e.g., in the same city)
        add_markers(
            marker = list(opacity = 0.7, size= 6),
            symbol = ~ Type,
            symbols = window_Shapes,
            color = ~ Type,
            colors = window_Colors,
            hoverinfo = "text",
            text = ~ paste0(
                "<br>Location: ",
                Location,
                "<br>Date: ",
                Date,
                "<br>Store: ",
                Place,
                "<br>Type: ",
                Type
            ),
            legendgroup = ~ Type
        )
    
    map %>%
        layout(geo = g,
               margin = list(
                   r = 0, 
                   t = 0, 
                   b = 0, 
                   l = 0),
               legend = list(title = list(text = '<b>Type of display</b>'), 
                             y = 0.5))
})


# PUBLICATIONS =====================================

output$PubPatterns <- renderPlotly({
    capta_freq <- filtered$captabase %>%
        select(Year, Publication) %>%
        table %>%
        as.data.frame() %>%
        left_join(., publication_levels, by = c("Publication" = "title"))
    
    ymax <-
        max(capta_freq$Freq, na.rm = T) %>% plyr::round_any(., 10, f = ceiling) # Max for y axis,to keep it fixed
    
    industry <- capta_freq %>%
        plot_ly(x = ~ Year, y = ~ Freq) %>%
        add_trace(
            hoverinfo = "text",
            type = "scatter",
            mode = "markers",
            color = ~ Publication,
            marker = (list(color = ~Publication_Color)),
            showlegend = F,
            legendgroup = ~ Publication
        ) %>%
        add_trace(
            hoverinfo = "text",
            type = "scatter",
            mode = "lines",
            color = ~ Publication,
            line = (list(color = ~Publication_Color)),
            legendgroup = ~ Publication
        ) %>%
        layout(yaxis = list(title = "Number of Ads",
                            range = c(0, ymax)),
               xaxis = list(title = ''))
    
    smoother <- capta_freq %>%
        split(.$Publication) %>%
        purrr::map(
            .f = function(data) {
                data %>% mutate(smoothed = fitted(loess(Freq ~ as.numeric(Year))))
            }
        ) %>%
        bind_rows() %>%
        plot_ly(.) %>%
        add_lines(
            x = ~ Year,
            y = ~ smoothed,
            color =  ~ Publication,
            line = (list(color = ~Publication_Color)),
            showlegend = F,
            hoverinfo = "none",
            legendgroup = ~ Publication
        ) %>%
        layout(yaxis = list(title = "LOESS"))
    
    subplot(industry, smoother, nrows = 2, shareX = TRUE,titleY = T) %>%
        rangeslider()
    
})


output$Cyclical <- renderPlotly({
    cycl_data <- filtered$captabase %>%
        select(Publication, Date) %>%
        mutate(Date = format(Date, "%B")) %>%
        group_by(Publication, Date) %>%
        dplyr::summarise(freq = n()) %>%
        ungroup  %>%
        left_join(., publication_levels, by = c("Publication" = "title")) %>%
        mutate(relfreq = freq / sum(freq)) %>%
        mutate(
            Date = fct_relevel(
                Date,
                "January",
                "February",
                "March",
                "April",
                "May",
                "June",
                "July",
                "August",
                "September",
                "October",
                "November",
                "December"
            )
        ) %>%
        plot_ly(
            x = ~ Date,
            y = ~ relfreq,
            type = "bar",
            color = ~ Publication,
            marker = (list(color = ~Publication_Color))
        ) %>%
        layout(
            legend = list(orientation = 'h'),
            xaxis = list(title = ''),
            yaxis = list(title = 'Relative frequency')
        )
    
    if (input$separate) {
        cycl <- cycl_data %>%
            layout(barmode = 'group')
    }
    
    else{
        cycl <- cycl_data %>%
            layout(barmode = 'stack')
    }
    
    cycl })


# INDUSTRY  =====================================

    output$competitors <- renderPlotly({
        
       competitors_data <- filtered$captabase %>%
            group_by(Brand, Year) %>%
            dplyr::count() %>%
            ungroup %>%
            left_join(., brand_colors, by = "Brand")
       
            plot_ly(
                data = subset(competitors_data, Brand_Legend == "Other"),
                x = ~ Year,
                y = ~ n,
                type = "scatter",
                color = ~ Brand_Legend,
                mode = "lines",
                line= list(color = ~Brand_Color),
                hoverinfo = "text",
                text = ~ Brand,
                legendgroup = ~Brand_Legend
            ) %>%
            add_lines(data = subset(competitors_data, Brand_Legend != "Other"),
                                      x = ~ Year,
                                      y = ~ n,
                                      type = "scatter",
                                      color = ~ Brand_Legend,
                                      mode = "lines",
                                      line= list(color = ~Brand_Color),
                                      hoverinfo = "text",
                                      text = ~ Brand,
                                      legendgroup = ~Brand_Legend) %>%
            layout(yaxis = list(title = "Number of ads"),
                   xaxis = list(title = "")) %>%
                rangeslider()
        
    })
    
output$compareBrands <- renderMenu({
    selectizeInput(
        'compBrands',
        label =NULL,
        choices = unique(captabase$Brand),
        selected = c("maidenform", "flexees", "playtex", "olga"),
        multiple = TRUE,
        options = list(maxItems = 4)
    )
})


compareBrandsData <- reactive({
    filtered$captabase %>%
    filter(Brand %in% input$compBrands) %>%
    group_by(Publication, Brand, format(Date,"%Y")) %>%
    dplyr::count() %>%
    rename(Date = 3)
})

output$compareBrandsPlot <- renderPlotly({

colors <- publication_levels$Publication_Color
names(colors)<- publication_levels$title

t <- ggplot(compareBrandsData(), aes(x=Date, y=n, color=Publication, group = Publication))+
    geom_line()+
     facet_wrap(~Brand, ncol = 2) +
     xlab('') +
     ylab('Number of Ads')+
     theme_bw()+
     theme(axis.text.x = element_text(angle = 90,size = 7))+
     scale_color_manual(values=colors)

ggplotly(t)

})


# ABOUT  =====================================
    output$`about-text`<-renderText(includeMarkdown("www/about.md"))
    output$`updates-text` <-renderText(includeMarkdown("www/updates.md"))
    output$`tracker-text` <-renderText(includeMarkdown("www/tracker.md"))
    
})