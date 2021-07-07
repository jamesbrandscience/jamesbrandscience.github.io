library(tidyverse)
library(DT)
library(plotly)
library(shiny)
library(reshape2)
library(shinyalert)
library(shinydashboard)
library(shinythemes)

###################
#data
###################

ONZE_summary <- readRDS("ONZE_summary.rds")

###################
#ui
###################

ui <- navbarPage(
  title = "Systematic co-variation",
  
  ################
  #bottom matter
  ################
  
  footer = 

  #add in various information about the data and app
  HTML(paste(
    #creative commons
    a(img(src="by.png", width=120, height=42), href="https://creativecommons.org/licenses/by/4.0/", target="_blank"),
    
    #Lancaster University
    a(img(src="NZILBB2.png", width=300, height=42), href="https://www.canterbury.ac.nz/nzilbb/", target="_blank"),
    
    #reuse declaration
    h5("You can distribute, remix, tweak, and build upon this work as long as you credit us for the original creation. Please use the following citation: \"CITATION\""))),
                 tabPanel("About",
                          dashboardPage(
                            dashboardHeader(disable = TRUE),
                            dashboardSidebar(disable = TRUE
                          ),
                          dashboardBody(
                            # useShinyalert(),  # Set up shinyalert
                            fluidRow(
                              column(6,
                                     includeMarkdown("www/about.md")
                              )
                            ),
                            #horizontal rule
                            hr()
                          ))),
                 tabPanel("PCA explorer",
                          dashboardPage(
  
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE
    # sidebarMenu(
    #   menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    #   menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    # )
  ),
  
  
  dashboardBody(
    
    tags$head(tags$style(HTML(
                              'body {
                              -moz-transform: scale(0.8, 0.8); /* Moz-browsers */
                              zoom: 0.8; /* Other non-webkit browsers */
                              zoom: 80%; /* Webkit browsers */
                              }

                              /* body */
                              .content-wrapper, .right-side {
                              background-color: #FFFFFF;
                              }

                              .navbar { background-color: #BFCB6F;
                              font-size: 18px;
                              color: #6A6A6A; }

                             #  .navbar-default .navbar-brand {
                             # color: #6A6A6A;
                             #  }

                              .navbar-default .navbar-brand { color: #F5F4F4; 
                                      font-size: 18px; 
                              background-color: #6A6A6A ;}
                              
                              '))),
  
  # fluidRow(useShinyalert(),  # Set up shinyalert
    box(collapsible = TRUE,
        title = "PC1",
        width = 1200,
    column(width = 6,
           plotlyOutput("PC1_plot", height = 300)
           )
    ,
    column(width = 3,
           plotlyOutput("PC1_vowels", height = 300, width = 300))
    ,
    column(width = 2, align="right",
           img(src = 'PC1_animation.gif', width = 300, height = 300,
               style='padding:20px;')
    )
  )
  ,
    box(collapsible = TRUE,
        title = "PC2",
        width = 1200,
    column(width = 6,
           plotlyOutput("PC2_plot", height = 300)
    )
    ,
    column(width = 3,
           plotlyOutput("PC2_vowels", height = 300, width = 300))
    ,
    column(width = 2,
           img(src = 'PC2_animation.gif', width = 300, height = 300,
               style='padding:20px;')
    )
  )
  ,
    box(collapsible = TRUE,
        title = "PC3",
        width = 1200,
    column(width = 6,
           plotlyOutput("PC3_plot", height = 300)
    )
    ,
    column(width = 3,
           plotlyOutput("PC3_vowels", height = 300, width = 300))
    ,
    column(width = 2,
           img(src = 'PC3_animation.gif', width = 300, height = 300,
               style='padding:20px;')
  )
  ),
  
  #horizontal rule
  hr()
)
)
),

tabPanel("More",
         dashboardPage(
           dashboardHeader(disable = TRUE),
           dashboardSidebar(disable = TRUE
           ),
           dashboardBody(
             # useShinyalert(),  # Set up shinyalert
             fluidRow(
               column(6,
                      includeMarkdown("www/more.md")
               )
             )
           )
         )
)
)

###################
#server
###################

server <- function(input, output, session) {
  
  output$PC1_plot <- renderPlotly({
    print(ggplotly(source = "PC1_plot",
                   
                   ggplot(ONZE_summary %>% select(Speaker, Gender, participant_year_of_birth, Comp.1) %>% distinct(), aes(x = participant_year_of_birth, y = -Comp.1, label = Gender, color = Gender, group = Speaker, key = Speaker)) +
                     geom_text(show.legend = FALSE) + #add text label i.e. F/M
                     scale_y_continuous(breaks = seq(-6,6,2), limits = c(-7,7)) +
                     scale_color_manual(breaks = c("F", "M"), labels = c("Female", "Male"), values = c("black", "green")) +
                     geom_hline(yintercept = 2, color = "red", linetype = "dashed") + #add red dashed line at 2
                     geom_hline(yintercept = -2, colour = "red", linetype = "dashed") + #add red dashed line at -2
                     xlab(NULL) + #label x axis
                     ylab("PC1 speaker loading") + #label y axis
                     theme_bw() + #general aesthetics
                     theme(axis.text = element_text(face = "bold", size = 14), axis.title = element_text(face = "bold", size = 14), legend.position="none") #make the text bold and larger on the axes
                   
      , tooltip = c("x", "y", "label", "key")
    )
    )
  })
  
  output$PC2_plot <- renderPlotly({
    print(
      ggplotly(
     
      ggplot(ONZE_summary %>% select(Speaker, Gender, participant_year_of_birth, Comp.2) %>% distinct(), aes(x = participant_year_of_birth, y = -Comp.2, label = Gender, color = Gender, group = Speaker, key = Speaker)) +
        geom_text(show.legend = FALSE) + #add text label i.e. F/M
        scale_y_continuous(breaks = seq(-6,6,2), limits = c(-7,7)) +
        scale_color_manual(breaks = c("F", "M"), labels = c("Female", "Male"), values = c("black", "green")) +
        geom_hline(yintercept = 2, color = "red", linetype = "dashed") + #add red dashed line at 2
        geom_hline(yintercept = -2, colour = "red", linetype = "dashed") + #add red dashed line at -2
        xlab(NULL) + #label x axis
        ylab("PC2 speaker loading") + #label y axis
        theme_bw() + #general aesthetics
        theme(axis.text = element_text(face = "bold", size = 14), axis.title = element_text(face = "bold", size = 14), legend.position="none") #make the text bold and larger on the axes

      , source = "PC2_plot", tooltip = c("x", "y", "label", "key")
    )
    )
  })
  
  output$PC3_plot <- renderPlotly({
    print(
      ggplotly(
      
      ggplot(ONZE_summary %>% select(Speaker, Gender, participant_year_of_birth, Comp.3) %>% distinct(), aes(x = participant_year_of_birth, y = -Comp.3, label = Gender, color = Gender, group = Speaker, key = Speaker)) +
        geom_text(show.legend = FALSE) + #add text label i.e. F/M
        scale_y_continuous(breaks = seq(-6,6,2), limits = c(-7,7)) +
        scale_color_manual(breaks = c("F", "M"), labels = c("Female", "Male"), values = c("black", "green")) +
        geom_hline(yintercept = 2, color = "red", linetype = "dashed") + #add red dashed line at 2
        geom_hline(yintercept = -2, colour = "red", linetype = "dashed") + #add red dashed line at -2
        xlab(NULL) + #label x axis
        ylab("PC3 speaker loading") + #label y axis
        theme_bw() + #general aesthetics
        theme(axis.text = element_text(face = "bold", size = 14), axis.title = element_text(face = "bold", size = 14), legend.position="none") #make the text bold and larger on the axes

      , source = "PC3_plot" , tooltip = c("x", "y", "label", "key")
    )
    )
  })
  
  # ONZE_summary <- ONZE_summary %>%
  #   mutate(colours1 = fct_recode(Vowel, "#9590FF" = "DRESS",
  #                                    "#D89000" = "FLEECE",
  #                                    "#A3A500" = "GOOSE",
  #                                    "#39B600" = "KIT",
  #                                    "#00BF7D" = "LOT",
  #                                    "#00BFC4" = "NURSE",
  #                                    "#00B0F6" = "START",
  #                                    "#F8766D" = "STRUT",
  #                                    "#E76BF3" = "THOUGHT",
  #                                    "#FF62BC" = "TRAP"))
  # 
  # vowels.PC1 <- c("START", "STRUT", "THOUGHT")
  # vowels.PC2 <- c("TRAP", "DRESS", "KIT", "NURSE", "FLEECE", "GOOSE", "LOT")
  # vowels.PC3 <- c("START", "DRESS","GOOSE", "LOT")
  
  
  output$PC1_vowels <- renderPlotly({
    s1 <- event_data("plotly_click", source = "PC1_plot")
    
    vowel_means_click <- ONZE_summary[ONZE_summary$Speaker == event_data("plotly_click", source = "PC1_plot")$key, ]
    
    ggplotly(
      p <- ggplot(data = vowel_means_click, aes(x = F2_mean, y = F1_mean, label = Vowel, colour = Vowel, size = PC1_loadings_abs)) +
        geom_text(show.legend = FALSE) +
        # scale_color_manual(values = c("grey", "grey", "grey", "grey", "grey",
        #                               "grey", "#00B0F6", "#F8766D", "#E76BF3", "grey")) +
        scale_color_manual(values = c("#9590FF", "#D89000", "#A3A500", "#39B600", "#00BF7D",
                                      "#00BFC4", "#00B0F6", "#F8766D", "#E76BF3", "#FF62BC")) +
        scale_size_continuous(range = c(1,3)) +
        scale_x_reverse(position = "top", name = "F2", limits = c(max(ONZE_summary$F2_mean), min(ONZE_summary$F2_mean))) +
        scale_y_reverse(position = "right", name = "F1", limits = c(max(ONZE_summary$F1_mean), min(ONZE_summary$F1_mean))) +
        ggtitle(event_data("plotly_click", source = "PC1_plot")$key) +
        theme_bw() +
        theme(legend.position="none", plot.title = element_text(size = 6)),
        tooltip = c("x", "y", "label")
    )
  })
  
  output$PC2_vowels <- renderPlotly({
    s2 <- event_data("plotly_click", source = "PC2_plot")
    
    vowel_means_click <- ONZE_summary[ONZE_summary$Speaker == event_data("plotly_click", source = "PC2_plot")$key, ]
    
    ggplotly(
      p <- ggplot(data = vowel_means_click, aes(x = F2_mean, y = F1_mean, label = Vowel, colour = Vowel, size = PC2_loadings_abs)) +
        geom_text(show.legend = FALSE) +
        # scale_color_manual(values = c("#9590FF", "#D89000", "#A3A500", "#39B600", "#00BF7D",
        #                               "#00BFC4", "grey", "grey", "grey", "#FF62BC")) +
        scale_color_manual(values = c("#9590FF", "#D89000", "#A3A500", "#39B600", "#00BF7D",
                                      "#00BFC4", "#00B0F6", "#F8766D", "#E76BF3", "#FF62BC")) +
        scale_size_continuous(range = c(1,3)) +
        scale_x_reverse(position = "top", name = "F2", limits = c(max(ONZE_summary$F2_mean), min(ONZE_summary$F2_mean))) +
        scale_y_reverse(position = "right", name = "F1", limits = c(max(ONZE_summary$F1_mean), min(ONZE_summary$F1_mean))) +
        ggtitle(event_data("plotly_click", source = "PC2_plot")$key) +
        theme_bw() +
        theme(legend.position="none", plot.title = element_text(size = 6)),
      tooltip = c("x", "y", "label")
    )
  })
  
  output$PC3_vowels <- renderPlotly({
    s3 <- event_data("plotly_click", source = "PC3_plot")
    
    vowel_means_click <- ONZE_summary[ONZE_summary$Speaker == event_data("plotly_click", source = "PC3_plot")$key, ]
    
    ggplotly(
      p <- ggplot(data = vowel_means_click, aes(x = F2_mean, y = F1_mean, label = Vowel, colour = Vowel, size = PC3_loadings_abs)) +
        geom_text(show.legend = FALSE) +
        # scale_color_manual(values = c("#9590FF", "grey", "#A3A500", "grey", "#00BF7D",
        #                               "grey", "#00B0F6", "grey", "grey", "grey")) +
        scale_color_manual(values = c("#9590FF", "#D89000", "#A3A500", "#39B600", "#00BF7D",
                                      "#00BFC4", "#00B0F6", "#F8766D", "#E76BF3", "#FF62BC")) +
        scale_size_continuous(range = c(1,3)) +
        scale_x_reverse(position = "top", name = "F2", limits = c(max(ONZE_summary$F2_mean), min(ONZE_summary$F2_mean))) +
        scale_y_reverse(position = "right", name = "F1", limits = c(max(ONZE_summary$F1_mean), min(ONZE_summary$F1_mean))) +
        ggtitle(event_data("plotly_click", source = "PC3_plot")$key) +
        theme_bw() +
        theme(legend.position="none", plot.title = element_text(size = 6)),
      tooltip = c("x", "y", "label")
    )
  })
}

shinyApp(ui, server)

# library(rsconnect)
# deployApp(appDir = "~/Desktop/Covariation_shiny/")
