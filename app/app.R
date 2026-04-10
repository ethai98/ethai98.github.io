library(here)
library(tidyverse)
library(shiny)
# library(tidyverse)
library(plotly)
# library(concordance)
# library(readxl)

# concordance_base <- read_excel(here("research_data/USITC_commodity_translation_wizard_20240707.xlsx"), 
#                                sheet = "Import Concordance", 
#                                col_types = c("text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric", "text"))
# 
# hts8_desc <- concordance_base %>%
#   mutate(hts8 = substr(hts10, 1,8)) %>%
#   distinct(hts8, hts10, description_short) %>%
#   group_by(hts8) %>%
#   mutate(description_hts10 = paste0(description_short, " (", hts10, ")")) %>%
#   summarise(description_hts10 = paste0(description_hts10, collapse = ", "))
# 
# cafta_members <- c("CAFTA" = "CRI, SLV, GTM, HND, NIC")
# 
# ptariff <- readRDS(here("research_data", "paper2_data_8digits_april2025.rds")) %>%
#   filter(!is.na(HS02)) %>%
#   mutate(desta_year = paste0(desta_id, "_", agr_year)) %>%
#   filter(desta_year != "551_2007") %>%
#   mutate(partner_iso3c = str_replace_all(partner_iso3c, cafta_members)) %>%
#   mutate(tariff_treatment_year = ifelse(tariff_treatment == "Phaseout", paste0("Phaseout: ", round(years_delay), " Years"), tariff_treatment)) %>%
#   as_tibble() %>%
#   filter(reporter_iso3c == "USA") %>%
#   select(reporter_iso3c, partner_iso3c, eif_year, code, baserate, unctad_baserate_average, category, years_delay, tariff_treatment) %>%
#   left_join(hts8_desc, by = c("code" = "hts8")) %>%
#   mutate(baserate_modified = ifelse(baserate == "PR", NA, baserate),
#          baserate_modified = as.numeric(baserate_modified),
#          # this process may introduce some 0 baserates, yet the 8-digit products had a tariff treatment attached to it.
#          baserate_supplemented = ifelse(is.na(baserate_modified), unctad_baserate_average, baserate_modified))  %>%
#   separate_rows(partner_iso3c, sep = ",\\s*") %>%
#   mutate(country_name = countrycode::countrycode(partner_iso3c, origin = "iso3c", destination = "country.name")) %>%
#   mutate(
#     clean_desc = str_trunc(description_hts10, 60), 
#     search_label = paste0(clean_desc, " (", code, ")")
#   ) %>%
#   drop_na(eif_year) %>%
#   mutate(years_delay = round(years_delay)) 
# 
# write_csv(ptariff, here("app", "ptariff.csv"))


# Shiny App
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "minty"),
  
  titlePanel("Trade Agreement Tariff Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      # SEARCH INPUT
      selectizeInput("product_input", "Search Product (Keyword or Code):", 
                     choices = NULL, 
                     multiple = FALSE,
                     options = list(
                       placeholder = 'Type "Vehicle", "8704", etc...',
                       maxOptions = 1000, 
                       dropdownParent = 'body'
                     )),
      
      # PARTNER INPUT
      selectInput("partner_input", "Compare Trade Partners:", 
                  choices = NULL, 
                  multiple = TRUE),
      
      hr(),
      helpText("Graph shows tariff rates 3 years prior to agreement through the phaseout period."),
      helpText("Vertical dotted line indicates Entry Into Force (EIF) year.")
    ),
    
    mainPanel(
      plotlyOutput("tariffPlot", height = "500px"),
      br(),
      h4("Tariff Details"),
      tableOutput("rawTable")
    )
  )
)

# --- 3. SERVER LOGIC ---
server <- function(input, output, session) {
  
  # A. SERVER-SIDE SEARCH
  updateSelectizeInput(session, "product_input", 
                       choices = unique(ptariff$search_label), 
                       selected = "TRUCKS, NESOI (8704900000) (87049000)", # <--- SET DEFAULT HERE
                       server = TRUE)
  
  # B. UPDATE PARTNERS (Sorted by Duration)
  observeEvent(input$product_input, {
    req(input$product_input)
    
    # 1. Filter AND Sort by phaseout duration (longest to shortest)
    available_partners <- ptariff %>% 
      filter(search_label == input$product_input) %>%
      # Treat NA delays as 0 so they go to the bottom
      arrange(desc(replace_na(years_delay, 0))) 
    
    # 2. Create named list (Preserves the sorted order)
    partner_choices <- setNames(available_partners$partner_iso3c, available_partners$country_name)
    
    # 3. Update Input, selecting the top 8 longest phaseouts by default
    updateSelectInput(session, "partner_input", 
                      choices = partner_choices,
                      selected = head(available_partners$partner_iso3c, 8))
  })
  
  # C. DATA CALCULATION (Calendar Years)
  selected_data <- reactive({
    req(input$product_input, input$partner_input)
    
    df_subset <- ptariff %>%
      filter(search_label == input$product_input,
             partner_iso3c %in% input$partner_input)
    
    # EXPAND LOGIC: Create timeline relative to EIF Year
    df_expanded <- df_subset %>%
      rowwise() %>%
      mutate(schedule = list({
        
        # Define Timeline: 3 years before EIF -> 20 years after EIF
        start_year <- eif_year - 3
        end_year   <- eif_year + 20 
        calendar_years <- start_year:end_year
        
        # Clean numeric values
        delay <- replace_na(years_delay, 0)
        base  <- replace_na(as.numeric(baserate_supplemented), 0)
        
        # Calculate Rates
        rates <- map_dbl(calendar_years, function(y) {
          if (y < eif_year) {
            # Before agreement: Rate is the base rate
            return(base)
          } else {
            # After agreement: Apply phaseout
            years_passed <- y - eif_year
            if (delay == 0) {
              return(0) # Immediate 0
            } else {
              cut <- base / delay
              return(max(0, base - (cut * years_passed)))
            }
          }
        })
        
        tibble(year = calendar_years, rate = rates)
      })) %>%
      unnest(schedule)
    
    return(df_expanded)
  })
  
  # D. PLOT (With EIF Marker)
  output$tariffPlot <- renderPlotly({
    req(selected_data())
    
    p <- ggplot(selected_data(), aes(x = year, y = rate, color = country_name)) +
      geom_step(size = 1.2) + # Step chart looks cleaner for annual rates
      geom_point(size = 1.5, alpha = 0.8) +
      # Add vertical lines for EIF years (dashed)
      geom_vline(aes(xintercept = eif_year, color = country_name), 
                 linetype = "dashed", alpha = 0.5) +
      labs(
        title = str_wrap(paste("Schedule:", input$product_input), width = 60),
        y = "Tariff Rate (%)",
        x = "Calendar Year",
        color = "Partner"
      ) +
      theme_minimal() +
      scale_color_viridis_d() + # Ensure you have 'viridis' installed or use scale_color_brewer
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  # E. TABLE
  output$rawTable <- renderTable({
    req(input$product_input)
    ptariff %>% 
      filter(search_label == input$product_input,
             partner_iso3c %in% input$partner_input) %>% 
      # Sort table by delay too, for consistency
      arrange(desc(replace_na(years_delay, 0))) %>%
      select(Country = country_name, 
             `EIF Year` = eif_year,
             Code = code, 
             `Base Rate` = baserate_supplemented, 
             `Delay (Yrs)` = years_delay, 
             Treatment = tariff_treatment)
  })
}
shinyApp(ui, server)
