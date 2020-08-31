library(shinydashboard)
library(DT)
library(plotly)

dashboardPage(
    
    skin = "black",
    dashboardHeader(disable = T),
    dashboardSidebar(dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "home", icon = icon("home")),
            menuItem("Captabase", tabName = "captabase", icon = icon("th")),
            menuItem("Dreams", tabName = "dreams", icon = icon("cloud")),
            menuItem("Publications", tabName = "publications", icon = icon("book")),
            menuItem("Industry", tabName = "industry", icon = icon("industry")),
            hr(),
            menuItemOutput("filter_publications"),
            hr(),
            menuItem("About", tabName = "about", icon = icon("info")),
            menuItem("Source code", icon = icon("code"), href = "https://github.com/askebos/UndergarmentAds"),
            menuItem("Companion", icon = icon("feather"), href = "https://www.jmmnews.com/presenting-marketing-differently/")
            
        )
    )),
    dashboardBody(
      tags$script('window.onload = function() {
      function fixBodyHeight() {
        var el = $(document.getElementsByClassName("wrapper")[0]);
        var h = el.height();
        el.css("min-height", h + 50 + "px");
      };
      window.addEventListener("resize", fixBodyHeight);
      fixBodyHeight();
    };'),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
        
      tabItems(

      # HOME
            tabItem(tabName = "home",
                    tags$div(class = "title","Undergarment ads in the United States, 1947- 1970")),             
                        
        # CAPTABASE
        tabItem(tabName = "captabase",
                fluidRow(
                    box(width = 12,
                        collapsible = T,
                        title = "Captabase",
                        DTOutput("captabase")
                    ),
                    box(width = 12,
                        collapsible = T,
                        title = "Sources, archives, and collection method",
                        plotlyOutput("Archive", height='700px'))
                )), 
        
        # DREAMS
        tabItem(tabName = "dreams",
               fluidRow(
                    box(width = 12, 
                        collapsible = T,
                        title = "Timeline",
                        column(10,
                               plotlyOutput("Timeline")),
                        column(
                          2,
                          radioButtons("showLabels", 
                                       label = tags$p('', "Show labels?"),
                                       choices = c(
                                         "Yes" = T,
                                         "No" = F),
                                       selected = F))),
                   box(
                        width = 12,
                        collapsible = T,
                        title = "Window displays",
                        column(10,
                               plotlyOutput("Map")),
                        column(2,
                            sliderInput(
                                "Windows.Year",
                                label = h5("Dates"),
                                min = as.Date("1947-01-01"),
                                max = as.Date("1970-01-01"),
                                value = c(as.Date("1947-01-01"), as.Date("1970-01-01")),
                                step = 365,
                                timeFormat = "%Y",
                                animate = TRUE)))
                )), 
        
        # PUBLICATIONS
        tabItem(tabName = "publications",
                fluidRow(
                box(width = 12, 
                    title = "Publication frequencies",
                    collapsible = T,
                             plotlyOutput("PubPatterns"))),
                fluidRow(
                box(width = 12,
                    title = "Yearly cycles",
                    collapsible = T,
                    column(10,
                             plotlyOutput("Cyclical")),
                    column(2, 
                           radioButtons(
                               "separate",
                               label = tags$p('', "Separate out publications"),
                               choices = c(
                                   "Yes" = T,
                                   "No" = F),
                               selected = F))
                    ))), 
        
        # INDUSTRY
        tabItem(tabName = "industry",
                fluidRow(
                    box(
                        width = 12,
                        title = "Competitors",
                        collapsible = T,
                        plotlyOutput("competitors")
                    )
                ),
                fluidRow(
                    box(
                        width = 12,
                        title = "Compare brands (max. 4)",
                        collapsible = T,
                        menuItemOutput("compareBrands"),
                        plotlyOutput("compareBrandsPlot")
                    )
                )), 

        # ABOUT
        tabItem(tabName = "about",
                fluidRow(box(title = "About",
                                htmlOutput("about-text")),
                box(title = "Updates",
                    htmlOutput("updates-text"))),
                fluidRow(box(title = "Contact",
                            tags$p("Contact details will be added here")),
                         box(title = "Issue/Request tracker",
                             htmlOutput("tracker-text")))
    ))
))
