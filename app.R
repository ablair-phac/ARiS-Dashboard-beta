#DESCRIPTION=========================

#This shiny app is designed to depict data from the PSES 2022 and 2024
# On the PHAC Science Workforce
# for Indicators that are relevant to Anti-racism in Science (ARiS) Monitoring

#Code by: Alexandra Blair

# Project start: Jan 2026.

#Guidance/Resources: CoPilot &  https://library.virginia.edu/data/articles/getting-started-with-shiny
#===============================================#

##=--- Icon bank (for copying) ======= -----

# 🚩 ✅  🚨  ⭐

#Install packages ------------------

#install.packages("shinyWidgets")
#install.packages("dslabs")
#install.packages("plotly")
#install.packages("ggpattern")

#Load the libraries------------
library(shiny)
library(shinyWidgets)
library(dslabs)
library(tidyverse)
library(plotly)
library(openxlsx)
library(readxl)

library(shiny)
library(ggplot2)
library(dplyr)
library(forcats)
library(ggpattern)

# ----------------------------------------------------------------#
# EXPECTATION: A data frame named `data` exists with these columns:
# Indicator, Stratifier, Domain, Sex, Year (numeric), Proportion

#Load DATA------------------

data <-read_excel("C:/Users/ablair/OneDrive - HC-SC PHAC-ASPC/ARiS/ARiS_by_Sex_EE_SciGrp.xlsx") # 🚨 Need to udpate this df with Q65 data


# 🚩 Issues to be fixed------
# 🚩 Missing Q65 in data


#Graph aesthetics set-up--------------

# Optional pattern support (install.packages("ggpattern") to enable textures)
have_ggpattern <- requireNamespace("ggpattern", quietly = TRUE)

#Defensive checks----------------
# Defensive checks (helpful to fail fast if a column is missing)
req_cols <- c("Indicator", "Stratifier", "Group", "Domain", "Gender", "Year", "Proportion")
stopifnot(all(req_cols %in% names(data)))

# Prepare UI choices-------------
indicator_choices  <- sort(unique(data$Indicator))
stratifier_choices <- sort(unique(data$Stratifier))
group_choices      <- c("(All)", sort(unique(data$Group)))
gender_choices     <- c("(All)", sort(unique(data$Gender)))
year_vals <- sort(unique(data$Year))
min_year <- min(year_vals, na.rm = TRUE)
max_year <- max(year_vals, na.rm = TRUE)
year_step <- if (length(year_vals) >= 2) min(diff(year_vals)) else 1

#UI starts HERE--------------------
ui <- fluidPage(
  titlePanel("PSES ARiS Indicators: PHAC Science Workforce"),
  sidebarLayout(
    sidebarPanel(
      width = 3,  # Could go to 2 
      
      selectInput(
        inputId = "indicator",
        label   = "Indicator",
        choices = indicator_choices,
        selected = indicator_choices[1]
      ),
      selectInput(
        inputId = "stratifier",
        label   = "Stratifier (Employment Equity Groups)",
        choices = stratifier_choices,
        selected = stratifier_choices[1]
      ),
      selectInput(
        inputId = "group",
        label   = "Filter Employment Group (optional)",
        choices = group_choices,
        selected = group_choices[1]  # default "(All)"
      ),
      radioButtons(
        inputId = "gender",
        label   = "Gender",
        choices = gender_choices,
        selected = gender_choices[1],  # default "(All)"
        inline  = FALSE
      ),
      sliderInput(
        inputId = "year",
        label   = "Year range",
        min     = min_year,
        max     = max_year,
        value   = c(min_year, max_year),
        step    = year_step,
        sep     = ""
      ),
      
      # Left-hand side help text ------------------------
      helpText(HTML(
        if (have_ggpattern) {
          paste0(
            "Bars show Gender × EE Stratifier Group × Employment Group. Color = Gender. Texture = Employment Group. Facets = Year.",
            
            "<br><br><strong>TECHNICAL NOTES:</strong>",
            
            "<br><br>All data come from the Public Service Employee Survey (PSES) 2022 and 2024.",
            
            "<br><br>Results are presented for the Public Health Agency of Canada (PHAC) science workforce.
            These include EC, BI, EG, NU, SE, MD, VM, CH Groups.",
            
            "<br><br>Dashboard prepared by the Health Equity Division (HED)'s Equity Analysis and Policy Research Team (EAPR).",
            "<br><br>Contact: <a href='mailto:alexandra.blair@phac-aspc.gc.ca'>alexandra.blair@phac-aspc.gc.ca</a> for questions."
          )
        } else {
          paste0(
            "Bars show Gender × EE Stratifier Group × Employment Group. Color = Gender. ",
            "(Install ggpattern for textures.) Facets = Year."
          )
        }
      ))
      
      
    ),
    #Panel height adjustment -----------
    mainPanel(
      plotOutput("combo_plot", height = "800px")
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    req(input$indicator, input$stratifier, input$group, input$gender, input$year)
    
    df <- data %>%
      filter(
        Indicator  == input$indicator,
        Stratifier == input$stratifier,
        Year >= input$year[1],
        Year <= input$year[2]
      )
    
    # Apply Group filter unless "(All)"
    if (!is.null(input$group) && input$group != "(All)") {
      df <- df %>% filter(Group == input$group)
    }
    
    # Apply Gender filter unless "(All)"
    if (!is.null(input$gender) && input$gender != "(All)") {
      df <- df %>% filter(Gender == input$gender)
    }
    
    df <- df %>%
      filter(!is.na(Domain), !is.na(Gender), !is.na(Group),
             !is.na(Proportion), !is.na(Year))
    
    # Convert proportions to 0–100 if they appear in 0–1 scale
    if (nrow(df) > 0) {
      max_prop <- suppressWarnings(max(df$Proportion, na.rm = TRUE))
      df <- df %>%
        mutate(ProportionPct = if (is.finite(max_prop) && max_prop <= 1) Proportion * 100 else Proportion)
    } else {
      df$ProportionPct <- numeric(0)
    }
    
    # Ensure a single row per combo to avoid duplicate labels
    df <- df %>%
      group_by(Indicator, Stratifier, Group, Gender, Domain, Year) %>%
      summarise(ProportionPct = mean(ProportionPct, na.rm = TRUE), .groups = "drop")
    
    # Combined x-axis label for readability---------------
    df <- df %>%
      mutate(
        Combo = paste0(Gender, " – ", Domain, " – ", Group),
        #Combo = paste0(Gender, " – ", Domain),
        Combo = fct_inorder(Combo)
      )
    
    df
  })
  
  #Cells with missing data---------
  output$combo_plot <- renderPlot({
    df <- filtered_data()
    validate(need(nrow(df) > 0, "No data for the selected Indicator / Stratifier / Group / Gender / Year range."))
    
    # Color palette for Gender
    gender_levels <- sort(unique(df$Gender))
    gender_palette <- scales::hue_pal()(length(gender_levels))
    
    #added but did not work:
    ncol_gender <- max(1, length(na.omit(gender_levels)))
    
    #Plot title-----------------
    # Title = Indicator value only 
    plot_title <- input$indicator
    
    #Pattern/texture setting --------------------------------
    if (have_ggpattern) {
      
      # Map Groups to distinct textures (recycles if many groups)
      group_levels <- sort(unique(df$Group))
      
      #NEW------
      ncol_group  <- max(1, length(na.omit(group_levels)))
      
      #base_patterns <- c("stripe", "crosshatch", "circle", "square", "polygon", "magick")
      base_patterns <- c("wave", "stripe", "circle",  "crosshatch", "square", "polygon", "magick")
    
      pattern_values <- setNames(rep(base_patterns, length.out = length(group_levels)), group_levels)
      
      #MOST COMPLEX GRAPHS (ALL VISIBLE)-----------------
      
           p <- ggplot(df, aes(x = Combo, y = ProportionPct)) +
        ggpattern::geom_col_pattern(
          aes(fill = Gender, pattern = Group), #Fill = colour, Pattern = texture
          width = 0.72,
          color = "grey35",
          pattern_density = 0.4,
          pattern_spacing = 0.03,
          pattern_key_scale_factor = 0.8
        ) +
        geom_text(
          aes(label = paste0(round(ProportionPct, 1), "%")),
          vjust = -0.2, size = 4 #Was 3
        ) +
        #scale_fill_manual(values = setNames(gender_palette, gender_levels)) +
        
        #Removes the fill from the Gender legend ------
        scale_fill_manual(values = setNames(gender_palette, gender_levels),
                          guide  = guide_legend(
                            ncol = ncol_gender,
                            override.aes = list(pattern = "none", pattern_density = 0, pattern_spacing = 0)
                          )
        ) +
        ggpattern::scale_pattern_manual(values = pattern_values) +
        scale_y_continuous(
          limits = c(0, 100),
          breaks = seq(0, 100, 10),
          labels = function(x) paste0(x, "%")
        ) +
        labs(
          x = "Gender × EE Stratifier Group × Employment Group", 
          y = "Proportion (%)",
          fill = "Gender",
          pattern = "Group",
          title = plot_title
        ) +
        facet_wrap(vars(Year), nrow = 1, scales = "free_x") +
        theme_minimal(base_size = 13) +
        theme(
          plot.title = element_text(face = "bold"),
          #X-axis angle----------------
          axis.text.x = element_text(size = 13, angle = 50, hjust = 1), #Adjust size using size = 
          
          #Legend aesthetics---------------
          legend.position = "top",
          legend.text = element_text(size = 14),       # ✅ Bigger legend text
          legend.title = element_text(size = 16),      # ✅ Bigger legend title
          legend.key.size = unit(1, "cm"),           # ✅ Bigger legend keys
          
          panel.spacing = unit(1, "lines"),
          
          panel.grid.major.x = element_blank(),  # ✅ Remove vertical grid lines
          panel.grid.minor.x = element_blank(),   # ✅ Remove vertical minor grid lines
          panel.grid.minor.y = element_blank(),  # ✅ Remove  horr minor grid lines
          panel.grid.major.y = element_blank()
          ) 
      
    } else {
    
      #FOR LESS COMPLEX GRAPHS (FEWER SELECTED)-----------------
      
      # Fallback without textures; facet by Group to keep distinctions visible
      p <- ggplot(df, aes(x = Combo, y = ProportionPct, fill = Gender)) +
        geom_col(width = 0.72, color = "grey35") +
        geom_text(
          aes(label = paste0(round(ProportionPct, 1), "%")),
          vjust = -0.2, size = 4 #Was 3
        ) +
        #scale_fill_manual(values = setNames(gender_palette, gender_levels)) +
        #NEW-----
        scale_fill_manual(values = setNames(gender_palette, gender_levels),
                          guide  = guide_legend(
                            ncol = ncol_gender,
                            override.aes = list(pattern = "none", pattern_density = 0, pattern_spacing = 0)
                          )
                          ) +
        scale_y_continuous(
          limits = c(0, 100),
          breaks = seq(0, 100, 10),
          labels = function(x) paste0(x, "%")
        ) +
        labs(
          #X-axis label  -----------------------
          #x = "Gender × Domain × Group",
          x = "Gender × EE Stratifier Group × Employment Group", 
          y = "Proportion (%)",
          fill = "Gender",
          title = plot_title
        ) +
        facet_grid(rows = vars(Year), cols = vars(Group), scales = "free_x") +
        theme_minimal(base_size = 13) +
        theme(
          plot.title = element_text(face = "bold"),
          #X-axis edits again------
          axis.text.x = element_text(size = 13, angle = 25, hjust = 1),
          legend.position = "top",
          
           legend.text = element_text(size = 14),       # ✅ Bigger legend text
           legend.title = element_text(size = 16),      # ✅ Bigger legend title
           legend.key.size = unit(1, "cm"),           # ✅ Bigger legend keys
           
          panel.spacing = unit(1, "lines") 
        )
      
    }
    
    p
  })
}

shinyApp(ui = ui, server = server)

