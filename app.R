library(shiny)
library(data.table)
library(tidyverse)
library(tidylog)
library(DT)
library(gghighlight)
library(scales)
library(ggrepel)
library(patchwork)
library(here)

# Load data
hospital_overview_long <- readRDS("rds/hospital_overview_long.rds")
setDT(hospital_overview_long)

disease_list <- readRDS("rds/disease_list.rds")

min_year <- min(hospital_overview_long$年度, na.rm = TRUE)
max_year <- max(hospital_overview_long$年度, na.rm = TRUE)  

prefectures <- readRDS("rds/prefectures.rds")
municipalities <- readRDS("rds/municipalities.rds")
hospitals <- readRDS("rds/hospitals.rds")
hospitals_municipalities <- readRDS("rds/hospitals_municipalities.rds")

# Load RDS files
rds_files <- list.files(path = "data/rds", pattern = "\\.rds$", full.names = TRUE)

# Read all RDS files into a list and convert to data.tables
data_list <- lapply(rds_files, function(x) {
  dt <- setDT(readRDS(x))
  name <- gsub("\\.rds$", "", basename(x))  # Extract filename without extension
  return(dt)
})
names(data_list) <- gsub("\\.rds$", "", basename(rds_files)) # Assign names to list elements



# UI
ui <- dashboardPage(
  dashboardHeader(title = "Survey of Discharged Patients"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("基本情報", tabName = "basic_info", icon = icon("dashboard")),
      menuItem("ダウンロード", tabName = "download", icon = icon("download")),
      
      # Sidebar inputs
      selectInput("prefecture", "都道府県", 
                 choices = prefectures, 
                 multiple = FALSE, 
                 selectize = TRUE),
      
      uiOutput("municipality_selector"),
      uiOutput("hospital_main_selector"),
      uiOutput("hospital_comparison_selector"),
      
      # MDC analysis inputs
      sliderInput("year_selector", "年度", 
                 min = 2012, 
                 max = as.integer(max_year), 
                 value = as.integer(max_year), 
                 step = 1),
      
      selectInput("operation_selector", "手術の有無", 
                 choices = c("手術あり", "手術なし", "合計"),
                 selected = "合計"),
      
      selectInput("mdc_selector", "MDC", 
                 choices = c("MDC01", "MDC02", "MDC03", "MDC04", "MDC05", 
                           "MDC06", "MDC07", "MDC08", "MDC09", "MDC10", 
                           "MDC11", "MDC12", "MDC13", "MDC14", "MDC15", 
                           "MDC16", "MDC17", "MDC18"),
                 selected = "MDC01"),
      
      uiOutput("disease_selector")
    )
  ),
  
  dashboardBody(
    tabItems(
      # Basic Info tab
      tabItem(tabName = "basic_info",
        fluidRow(
          tabBox(
            width = 12, 
            height = "1000px",
            # MDC Overview
            tabPanel("主要MDC", plotOutput("plot_data1_12", height = "600px")),
            tabPanel("主要MDC(手術別)", plotOutput("plot_data2_2", height = "600px")),
            tabPanel("主要病名", tableOutput("plot_data2_2_text")),
            
            # Hospital Performance
            tabPanel("平均在院日数", plotOutput("plot_data1_3")),
            tabPanel("救急医療入院", plotOutput("plot_data1_6")),
            tabPanel("他院からの紹介の有無", plotOutput("plot_data1_7")),
            tabPanel("入院経路と退院先", plotOutput("plot_data1_8_patchwork")),
            tabPanel("再入院", plotOutput("plot_data1_10")),
            tabPanel("退院時転帰", plotOutput("plot_data1_9")),
            tabPanel("在院日数分析", plotOutput("plot_data1_14")),
            
            # MDC Analysis
            tabPanel("手術別MDC(市区町村)", plotOutput("plot_data2_2_municipalities")),
            tabPanel("緊急入院・救急車による搬送の有無", plotOutput("plot_data2_3_4")),
            tabPanel("主要MDC詳細", plotOutput("plot_data2_8_main_hospital")),
            tabPanel("病名別 エリア分析", plotOutput("plot_data2_8_municipalities"))
          )
        )
      ),
      
      # Download tab
      tabItem(tabName = "download",
        fluidRow(
          box(
            width = 12,
            downloadButton("download_plot_all", "グラフをダウンロード")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Dynamic UI elements
  output$municipality_selector <- renderUI({
    select_prefecture <- input$prefecture
    municipalities_for_prefecture <- municipalities[[select_prefecture]]
    selectInput("dynamicMunicipality", "市区町村", 
                choices = municipalities_for_prefecture, 
                multiple = FALSE, 
                selectize = TRUE)
  })
  
  output$hospital_main_selector <- renderUI({
    select_municipality <- input$dynamicMunicipality
    if (is.null(select_municipality) || !select_municipality %in% names(hospitals_municipalities)) {
      return(selectInput("dynamicHospital_main", "主病院", choices = NULL))
    }
    hospitals_for_municipality <- hospitals_municipalities[[select_municipality]]
    if (is.null(hospitals_for_municipality) || length(hospitals_for_municipality) == 0) {
      return(selectInput("dynamicHospital_main", "主病院", choices = NULL))
    }
    selectInput("dynamicHospital_main", "主病院", 
                choices = hospitals_for_municipality, 
                multiple = FALSE, 
                selectize = TRUE)
  })
  
  output$hospital_comparison_selector <- renderUI({
    select_prefecture <- input$prefecture
    hospitals_for_prefecture <- hospitals[[select_prefecture]]
    selectInput("dynamicHospital_comparison", "比較病院", 
                choices = hospitals_for_prefecture, 
                multiple = TRUE, 
                selectize = TRUE)
  })
  
  output$disease_selector <- renderUI({
    mdc_selected <- input$mdc_selector
    disease_list_selected <- filter(disease_list, MDC_main == mdc_selected)
    selectInput("dynamicDisease", "病名", 
                choices = disease_list_selected$label, 
                multiple = FALSE)
  })
  
  # Create a reactive expression for filtered data_list
  data_list_filtered <- reactive({
    lapply(data_list, function(dt) {
      if ("都道府県名" %in% names(dt)) {
        dt <- dt[都道府県名 == input$prefecture]
      }
      return(dt)
    })
  })
  
  # Plot outputs
  output$plot_data1_12 <- renderPlot({
    req(input$dynamicHospital_main)
    
    plot_data <- data_list_filtered()[["data1_12"]]
    if(is.null(plot_data)) return(NULL)
    
    highlight_hospitals <- input$dynamicHospital_main
    if (!is.null(input$dynamicHospital_comparison) && length(input$dynamicHospital_comparison) > 0) {
      highlight_hospitals <- c(input$dynamicHospital_main, input$dynamicHospital_comparison)
    }
    
    plot_data |>
      filter(施設名_最新 %in% highlight_hospitals) |>
      mutate(年度 = as.integer(年度)) |>
      group_by(施設名_最新, MDC) |>
      mutate(has_any_high_割合 = any(割合 >= 0.1)) |>
      ungroup() |>
      mutate(label_value = ifelse(割合 >= 0.1, paste0(round(割合 * 100, 1), "%"), "")) |>
      mutate(label_MDC = if_else((has_any_high_割合 & 年度 == max(年度)), paste0(MDC), "")) |>
      ggplot(aes(x = 年度, y = 割合, fill = MDC)) +
      geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
      geom_text(aes(label = label_value), 
                position = position_stack(vjust = 0.5, reverse = TRUE), 
                color = "grey20") +
      geom_text_repel(aes(x = 年度 + 1, label = label_MDC), 
                      position = position_stack(vjust = 0.5, reverse = TRUE), 
                      color = "grey20", direction = "y") +
      facet_wrap(~ 施設名_最新) +
      theme_minimal() +
      scale_x_continuous(breaks = seq(min_year, max_year, by = 1)) +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "", y = "割合", fill = "", 
           caption = "出典: 厚生労働省DPC導入の影響評価に係る調査「退院患者調査」の結果報告\n『（12）施設別MDC比率』をもとに作成\n比較病院は同じ都道府県内の病院\n割合が10%以上のMDCは表示している") +
      theme(legend.position = "none") +
      coord_flip()
  })
  
  output$plot_data1_3 <- renderPlot({
    req(input$dynamicHospital_main)
    
    plot_data <- data_list_filtered()[["data1_3"]]
    if(is.null(plot_data)) return(NULL)
    
    highlight_hospitals <- input$dynamicHospital_main
    if (!is.null(input$dynamicHospital_comparison) && length(input$dynamicHospital_comparison) > 0) {
      highlight_hospitals <- c(input$dynamicHospital_main, input$dynamicHospital_comparison)
    }
    
    plot_data |>
      ggplot(aes(x = 年度, y = 平均値, group = 施設名_最新, color = 施設名_最新)) +
      geom_line(linewidth = 3) +
      gghighlight(
        施設名_最新 %in% highlight_hospitals,
        use_direct_label = FALSE,
        unhighlighted_params = list(linewidth = 0.5)
      ) +
      theme_minimal() +
      scale_x_continuous(breaks = seq(min_year, max_year, by = 1)) +
      labs(x = "", y = "平均在院日数", color = "",
           caption = "出典: 厚生労働省DPC導入の影響評価に係る調査「退院患者調査」の結果報告\n『（３）在院日数の状況』をもとに作成\n比較病院は同じ都道府県内の病院") +
      theme(legend.position = "top")
  })
  
  output$plot_data1_6 <- renderPlot({
    req(input$dynamicHospital_main)
    
    plot_data <- data_list_filtered()[["data1_6"]]
    if(is.null(plot_data)) return(NULL)
    
    highlight_hospitals <- input$dynamicHospital_main
    if (!is.null(input$dynamicHospital_comparison) && length(input$dynamicHospital_comparison) > 0) {
      highlight_hospitals <- c(input$dynamicHospital_main, input$dynamicHospital_comparison)
    }
    
    plot_data |>
      ggplot(aes(x = 年度, y = 率, group = 施設名_最新, color = 施設名_最新)) +
      geom_line(linewidth = 3) +
      gghighlight(
        施設名_最新 %in% highlight_hospitals,
        use_direct_label = FALSE,
        unhighlighted_params = list(linewidth = 0.5)
      ) +
      facet_wrap(~ key) +
      theme_minimal() +
      scale_x_continuous(breaks = seq(min_year, max_year, by = 1)) +
      labs(x = "", y = "率", color = "",
           caption = "出典: 厚生労働省DPC導入の影響評価に係る調査「退院患者調査」の結果報告\n『（６）救急医療入院』をもとに作成\n比較病院は同じ都道府県内の病院") +
      theme(legend.position = "top")
  })
  
  output$plot_data1_7 <- renderPlot({
    req(input$dynamicHospital_main)
    
    plot_data <- data_list_filtered()[["data1_7"]]
    if(is.null(plot_data)) return(NULL)
    
    highlight_hospitals <- input$dynamicHospital_main
    if (!is.null(input$dynamicHospital_comparison) && length(input$dynamicHospital_comparison) > 0) {
      highlight_hospitals <- c(input$dynamicHospital_main, input$dynamicHospital_comparison)
    }
    
    plot_data |>
      ggplot(aes(x = 年度, y = 率, group = 施設名_最新, color = 施設名_最新)) +
      geom_line(linewidth = 3) +
      gghighlight(
        施設名_最新 %in% highlight_hospitals,
        use_direct_label = FALSE,
        unhighlighted_params = list(linewidth = 0.5)
      ) +
      theme_minimal() +
      scale_x_continuous(breaks = seq(min_year, max_year, by = 1)) +
      labs(x = "", y = "他院からの紹介（率）", color = "",
           caption = "出典: 厚生労働省DPC導入の影響評価に係る調査「退院患者調査」の結果報告\n『（７）他院よりの紹介の有無』をもとに作成\n比較病院は同じ都道府県内の病院") +
      theme(legend.position = "top")
  })
  
  output$plot_data1_8_patchwork <- renderPlot({
    req(input$dynamicHospital_main)
    
    highlight_hospitals <- input$dynamicHospital_main
    if (!is.null(input$dynamicHospital_comparison) && length(input$dynamicHospital_comparison) > 0) {
      highlight_hospitals <- c(input$dynamicHospital_main, input$dynamicHospital_comparison)
    }
    
    # 入院経路のプロット
    plot_hospitalization <- {
      plot_data_hospitalization <- data_list_filtered()[["data1_8_hospitalization"]]
      if(is.null(plot_data_hospitalization)) return(NULL)
      
      plot_data_hospitalization |>
        filter(施設名_最新 %in% highlight_hospitals) |>
        filter(name != "家庭からの入院") |>
        mutate(label_value = ifelse(value >= 0.01, paste0(round(value * 100, 1), "%"), "")) |>
        ggplot(aes(x = 年度, y = value, fill = name)) +
        geom_bar(stat = "identity", position = "stack") +
        geom_text(aes(label = label_value), position = position_stack(vjust = 0.5)) +
        facet_wrap(~ 施設名_最新) +
        theme_minimal() +
        scale_x_continuous(breaks = seq(min_year, max_year, by = 1)) +
        scale_y_continuous(labels = scales::percent) +
        labs(x = "", y = "", fill = "", title = "入院経路") +
        theme(legend.position = "top", 
              legend.text = element_text(size = 10), 
              axis.text.x = element_text(angle = 90))
    }
    
    # 退院先のプロット
    plot_discharge <- {
      plot_data_discharge <- data_list_filtered()[["data1_8_discharge"]]
      if(is.null(plot_data_discharge)) return(NULL)
      
      plot_data_discharge |>
        filter(施設名_最新 %in% highlight_hospitals) |>
        filter(!grepl("家庭への退院", name)) |>
        mutate(label_value = ifelse(value >= 0.01, paste0(round(value * 100, 1), "%"), "")) |>
        ggplot(aes(x = 年度, y = value, fill = name)) +
        geom_bar(stat = "identity", position = "stack") +
        geom_text(aes(label = label_value), position = position_stack(vjust = 0.5)) +
        facet_wrap(~ 施設名_最新) +
        theme_minimal() +
        scale_x_continuous(breaks = seq(min_year, max_year, by = 1)) +
        scale_y_continuous(labels = scales::percent) +
        labs(x = "", y = "", fill = "", title = "退院先") +
        theme(legend.position = "top", 
              legend.text = element_text(size = 10), 
              axis.text.x = element_text(angle = 90))
    }
    
    # patchworkで結合し、共通のannotationを付与
    plot_hospitalization + plot_discharge +
      plot_annotation(
        caption = "出典: 厚生労働省DPC導入の影響評価に係る調査「退院患者調査」の結果報告\n『（８）入院経路及び退院先の状況』をもとに作成\n比較病院は同じ都道府県内の病院\n入院経路は家庭からの入院/退院は家庭への退院を除いている",
        theme = theme(plot.caption = element_text(size = 12))
      )
  })
  
  output$plot_data1_9 <- renderPlot({
    req(input$dynamicHospital_main)
    
    plot_data_disposition <- data_list_filtered()[["data1_9"]]
    if(is.null(plot_data_disposition)) return(NULL)
    
    highlight_hospitals <- input$dynamicHospital_main
    if (!is.null(input$dynamicHospital_comparison) && length(input$dynamicHospital_comparison) > 0) {
      highlight_hospitals <- c(input$dynamicHospital_main, input$dynamicHospital_comparison)
    }
    
    plot_data_disposition |>
      mutate(name = case_when(
        name %in% c("治癒", "軽快") ~ "治癒・軽快",
        TRUE ~ name
      )) |>
      mutate(name = factor(name)) |>
      filter(施設名_最新 %in% highlight_hospitals) |>
      mutate(label_value = ifelse(value >= 0.01, paste0(round(value * 100, 1), "%"), "")) |>
      ggplot(aes(x = 年度, y = value, fill = name)) +
      geom_bar(stat = "identity", position = "stack") +
      geom_text(aes(label = label_value), position = position_stack(vjust = 0.5)) +
      facet_wrap(~ 施設名_最新) +
      theme_minimal() +
      scale_x_continuous(breaks = seq(min_year, max_year, by = 1)) +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "", y = "", fill = "", title = "退院時転帰",
           caption = "出典: 厚生労働省DPC導入の影響評価に係る調査「退院患者調査」の結果報告\n『（９）退院時転帰の状況』をもとに作成\n比較病院は同じ都道府県内の病院") +
      theme(legend.position = "top",
            legend.text = element_text(size = 10),
            axis.text.x = element_text(angle = 90))
  })
  
  output$plot_data1_10 <- renderPlot({
    req(input$dynamicHospital_main)
    
    plot_data_rehospitalization <- data_list_filtered()[["data1_10"]]
    if(is.null(plot_data_rehospitalization)) return(NULL)
    
    highlight_hospitals <- input$dynamicHospital_main
    if (!is.null(input$dynamicHospital_comparison) && length(input$dynamicHospital_comparison) > 0) {
      highlight_hospitals <- c(input$dynamicHospital_main, input$dynamicHospital_comparison)
    }
    
    plot_data_rehospitalization |>
      filter(施設名_最新 %in% highlight_hospitals) |>
      mutate(施設名_最新 = fct_relevel(施設名_最新, highlight_hospitals)) |>
      mutate(label_value = ifelse(value >= 0.01, paste0(round(value * 100, 1), "%"), "")) |>
      ggplot(aes(x = 年度, y = value, fill = key, color = time)) +
      geom_bar(stat = "identity", position = "stack", alpha = 0.7) +
      geom_text(aes(label = label_value), position = position_stack(vjust = 0.5), color = "grey20") +
      facet_wrap(~ 施設名_最新) +
      theme_minimal() +
      scale_x_continuous(breaks = seq(min_year, max_year, by = 1)) +
      scale_y_continuous(labels = scales::percent) +
      scale_color_manual(values = c("blue", "red")) +
      labs(x = "", y = "", fill = "", title = "再入院",
           caption = "出典: 厚生労働省DPC導入の影響評価に係る調査「退院患者調査」の結果報告\n『（10）再入院の状況』をもとに作成\n比較病院は同じ都道府県内の病院\n期間：2015年までは6週囲内、2016年からは4週囲内") +
      theme(legend.position = "top",
            legend.text = element_text(size = 10),
            axis.text.x = element_text(angle = 90))
  })
  
  output$plot_data1_14 <- renderPlot({
    req(input$dynamicHospital_main, input$mdc_selector)
    
    plot_data_animation_raw <- data_list_filtered()[["data1_14"]]
    if(is.null(plot_data_animation_raw)) return(NULL)
    
    highlight_hospitals <- input$dynamicHospital_main
    if (!is.null(input$dynamicHospital_comparison) && length(input$dynamicHospital_comparison) > 0) {
      highlight_hospitals <- c(input$dynamicHospital_main, input$dynamicHospital_comparison)
    }
    
    plot_data_animation <- plot_data_animation_raw |>
        filter(MDC == input$mdc_selector) 

    x_sd <- plot_data_animation |>
      pull(患者構成の指標) |>
      sd(na.rm = TRUE)

    y_sd <- plot_data_animation |>
      pull(在院日数の指標) |>
      sd(na.rm = TRUE)

    x_max_rect <- 1 + x_sd
    y_max_rect <- 1 + y_sd


    plot_data_animation <- plot_data_animation |>
      filter(施設名_最新 %in% highlight_hospitals) |>
      mutate(施設名_最新 = fct_relevel(施設名_最新, highlight_hospitals)) |>
      filter(!(is.na(患者構成の指標) | is.na(在院日数の指標)))
    
    # Calculate max value for x and y axes within filtered data
    max_x <- max(plot_data_animation$患者構成の指標, na.rm = TRUE)
    max_y <- max(plot_data_animation$在院日数の指標, na.rm = TRUE)
    max_value <- max(max_x, max_y) + 0.1

    
    if (x_max_rect > max_value) {
      x_max_rect <- max_value
    }
    if (y_max_rect > max_value) {
      y_max_rect <- max_value
    }
    
    subtitle <- paste(
      "赤(右上)：重症患者割合高いが、似た構成の病院と比べても在院日数がさらに長い",
      "緑(右下)：重症患者割合高いが、似た構成の病院と比べて在院日数が短い",
      "橙(左上)：軽症患者中心だが、似た構成の病院と比べて在院日数が長い",
      "青(左下)：軽症患者中心だが、似た構成の病院と比べて在院日数がさらに短い",
      "白：県内平均の1標準偏差",

        sep = "\n"
    )
    ggplot(data = plot_data_animation, aes(x = 患者構成の指標, y = 在院日数の指標, color = 年度, label = 年度, group = 年度)) +
          geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1), fill = "#ADD8E6", alpha = 0.2, color = NA) + # Light Blue (Q1)
          geom_rect(aes(xmin = 1, xmax = max_value, ymin = 0, ymax = 1), fill = "#90EE90", alpha = 0.2, color = NA) + # Light Green (Q4)
          geom_rect(aes(xmin = 0, xmax = 1, ymin = 1, ymax = max_value), fill = "#FFB347", alpha = 0.2, color = NA) + # Light Orange (Q2)
          geom_rect(aes(xmin = 1, xmax = max_value, ymin = 1, ymax = max_value), fill = "#FF6961", alpha = 0.2, color = NA) + # Light Red (Q3)
          geom_rect(aes(xmin = 1 - x_sd, xmax = x_max_rect , ymin = 1 - y_sd, ymax = y_max_rect), fill = "white", alpha = 0.1, color = NA) +
          geom_hline(yintercept = 1) +
          geom_vline(xintercept = 1) +
      #geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
      geom_label_repel(aes(label = 年度), family = font_sans,
                       box.padding   = 0.35, 
                       point.padding = 0.5,
                         force = 10,             # ← 離れる力を強くする
                         max.overlaps = Inf,
                       segment.color = 'grey50') +
      facet_wrap(~ 施設名_最新) +
      theme_minimal(base_size = 16, base_family = font_sans) +
      coord_fixed() +
      labs(title = paste0("MDC: ", input$mdc_selector),
           subtitle = subtitle,
           x = "右に行くほど重症患者割合が高い",
           y = "上に行くほど在院日数が長い",
           caption = "出典: 厚生労働省DPC導入の影響評価に係る調査「退院患者調査」の結果報告\n『（14）患者構成と在院日数の関係』をもとに作成\n比較病院は同じ都道府県内の病院") +
      theme(legend.position = "none",
            text = element_text(family = font_sans),
            plot.title = element_text(family = font_sans),
            plot.caption = element_text(family = font_sans),
            axis.text = element_text(family = font_sans))
  }, height = 600, width = 900)
  
  output$plot_data2_2 <- renderPlot({
    req(input$dynamicHospital_main, input$year_selector, input$operation_selector)
    
    plot_data_mdc_surgery <- data_list_filtered()[["data2_2"]]
    if(is.null(plot_data_mdc_surgery)) return(NULL)
    
    highlight_hospitals <- input$dynamicHospital_main
    if (!is.null(input$dynamicHospital_comparison) && length(input$dynamicHospital_comparison) > 0) {
      highlight_hospitals <- c(input$dynamicHospital_main, input$dynamicHospital_comparison)
    }
    
    plot_data_mdc_surgery |>
      filter(施設名_最新 %in% highlight_hospitals) |>
      filter(!is.na(value)) |>
      mutate(年度 = as.integer(年度)) |>
      mutate(n = sum(value), .by = c(施設名_最新, 手術, 年度)) |>
      group_by(施設名_最新, MDC, 手術) |>
      mutate(has_any_high_割合 = any(value/n >= 0.1)) |>
      ungroup() |>
      mutate(label_MDC = if_else((has_any_high_割合 & 年度 == max(年度)), paste0(MDC), "")) |>
      ggplot(aes(x = 年度, y = value, fill = MDC)) +
      geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
      geom_text_repel(aes(x = 年度 + 1, label = label_MDC),
                      position = position_stack(vjust = 0.5, reverse = TRUE),
                      color = "grey20", direction = "y") +
      facet_grid(施設名_最新 ~ 手術) +
      theme_minimal() +
      scale_x_continuous(breaks = seq(as.integer(min_year), as.integer(max_year), by = 1)) +
      labs(x = "", y = "件数", fill = "",
           caption = "出典: 厚生労働省DPC導入の影響評価に係る調査「退院患者調査」の結果報告\n『（２）MDC別医療機関別件数（割合）』をもとに作成\n比較病院は同じ都道府県内の病院\n割合が10%以上のMDCは表示している") +
      theme(legend.position = "none",
            legend.text = element_text(size = 10),
            axis.text.x = element_text(angle = 90)) +
      coord_flip()
  })
  
  output$plot_data2_2_text <- renderTable({
    req(input$dynamicMunicipality, input$mdc_selector, input$prefecture)

    selected_prefecture <- input$prefecture
    selected_hospital_main <- input$dynamicHospital_main
    current_year_for_text <- max_year
    operation_types_for_text <- c("手術あり", "手術なし", "合計")

    DT_data2_8 <- setDT(copy(data_list_filtered()[["data2_8"]]))
    
    # Create empty list to store data frames for each operation type
    table_list <- list()
    
    for(current_operation in operation_types_for_text) {
      plot_data_text <- DT_data2_8[年度 == current_year_for_text & 施設名_最新 == selected_hospital_main]

      if(nrow(plot_data_text) > 0) {
        plot_data_text <- plot_data_text[get(current_operation) > 0]
        if(nrow(plot_data_text) > 0) {
          # Sort by current operation type and add rank within operation type
          plot_data_text <- plot_data_text[order(-get(current_operation))]
          top_n <- min(10, nrow(plot_data_text))
          plot_data_text <- head(plot_data_text, top_n)
          
          if(nrow(plot_data_text) > 0) {
            # Create data frame for current operation type with rank
            df <- data.frame(
              手術区分 = current_operation,
              順位 = seq_len(nrow(plot_data_text)),  # Rank resets for each operation type
              病名 = plot_data_text$病名,
              MDC = plot_data_text$MDC_main,
              病名 = plot_data_text$病名
            )
            table_list[[current_operation]] <- df
          }
        }
      }
    }
    
    # Combine all data frames
    do.call(rbind, table_list)
  }, 
  striped = TRUE,
  hover = TRUE,
  spacing = "s",
  align = "c",
  digits = 0)
  
  output$plot_data2_3_4 <- renderPlot({
    req(input$dynamicHospital_main, input$mdc_selector)
    
    data2_4_dt <- setDT(copy(data_list_filtered()[["data2_4"]]))[MDC == input$mdc_selector]
    setnames(data2_4_dt, "救急車による搬送", "value")
    selected_cols <- c("市町村番号", "都道府県名", "市町村名", "施設名_最新", "lon", "lat", "年度", "施設名", "告示番号", "MDC", "value")
    data2_4_processed <- data2_4_dt[, ..selected_cols][, name := "救急車による搬送"]
    
    data2_3_dt <- setDT(copy(data_list_filtered()[["data2_3"]]))[MDC == input$mdc_selector]
    
    data2_3_4_join <- rbind(data2_3_dt, data2_4_processed)
    setkey(data2_3_4_join, 市町村番号, 都道府県名, 市町村名, 施設名_最新, 年度)
    
    data2_3_4_join[, total_value := sum(value, na.rm = TRUE), by = .(施設名_最新, 年度, MDC)]
    data2_3_4_join[, proportion := value / total_value]
    data2_3_4_join <- data2_3_4_join[年度 >= 2013]
    
    highlight_hospitals <- input$dynamicHospital_main
    if (!is.null(input$dynamicHospital_comparison) && length(input$dynamicHospital_comparison) > 0) {
      highlight_hospitals <- c(input$dynamicHospital_main, input$dynamicHospital_comparison)
    }
    
    data2_3_4_join |>
      tibble() |>
      filter(!is.na(proportion), !is.na(年度)) |>
      filter(name != "予定入院") |>
      mutate(施設名_最新 = fct_relevel(施設名_最新, highlight_hospitals)) |>
      ggplot(aes(x = 年度, y = proportion, group = 施設名_最新, color = 施設名_最新)) +
      geom_line(linewidth = 3) +
      gghighlight(
        施設名_最新 %in% highlight_hospitals,
        use_direct_label = FALSE,
        unhighlighted_params = list(linewidth = 0.5)
      ) +
      facet_wrap(~ name) +
      theme_minimal() +
      scale_x_continuous(breaks = seq(2013, max_year, by = 1)) +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "", y = "", color = "",
           caption = paste0("出典: 厚生労働省DPC導入の影響評価に係る調査「退院患者調査」の結果報告\n",
                          "『（３）予定・救急医療入院医療機関別MDC別集計, （４）救急車による搬送の有無の医療機関別MDC別集計』をもとに作成\n",
                          "比較病院は同じ都道府県内の病院")) +
      theme(legend.position = "top")
  })
  
  output$plot_data2_8_main_hospital <- renderPlot({
    req(input$year_selector, input$operation_selector, input$dynamicHospital_main)
    
    validate(
      need(input$operation_selector %in% c("手術あり", "手術なし", "合計"),
           "Invalid operation selection")
    )
    
    DT_data2_8 <- setDT(copy(data_list_filtered()[["data2_8"]]))
    DT_data2_8_filtered <- DT_data2_8[年度 == input$year_selector]
    DT_data2_8_filtered <- DT_data2_8_filtered[施設名_最新 %in% c(input$dynamicHospital_main)]
    
    DT_data2_8_filtered_plot <- DT_data2_8_filtered |>
      filter(!!sym(input$operation_selector) > 0) |>
      arrange(施設名_最新, 年度, desc(!!sym(input$operation_selector))) |>
      mutate(percentage = !!sym(input$operation_selector)/sum(!!sym(input$operation_selector)),
             .by = c(施設名_最新, 年度)) |>
      mutate(cumsum_percentage = cumsum(percentage), .by = c(施設名_最新, 年度)) |>
      mutate(group_size = rank(desc(!!sym(input$operation_selector))), .by = c(施設名_最新, 年度)) |>
      mutate(value = !!sym(input$operation_selector)) |>
      filter(group_size <= 20)
    
    ggplot(DT_data2_8_filtered_plot,
           aes(x = reorder(paste0(病名, " (", MDC_main, ")"), desc(cumsum_percentage)),
               group = 施設名_最新)) +
      geom_bar(aes(y = percentage, fill = MDC_main), stat = "identity") +
      geom_line(aes(y = cumsum_percentage), linewidth = 3, color = "red") +
      geom_text(aes(label = value, y = percentage), size = 5, hjust = -0.5) +
      theme_minimal() +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "", y = "", color = "",
           caption = "出典: 厚生労働省DPC導入の影響評価に係る調査「退院患者調査」の結果報告\n『（８）疾患別手術別集計』をもとに作成\n上位20位までを表示している") +
      coord_flip()
  })
  
  output$plot_data2_8_municipalities <- renderPlot({
    req(input$dynamicMunicipality, input$dynamicDisease)
    
    selected_disease <- input$dynamicDisease
    selected_disease_label <- disease_list$病名[disease_list$label == selected_disease]
    
    DT_for_plot <- setDT(copy(data_list_filtered()[["data2_8"]]))
    DT_for_plot <- mutate(DT_for_plot, value = !!sym(input$operation_selector))
    
    DT_for_plot <- DT_for_plot[病名 == selected_disease_label]
    DT_for_plot <- DT_for_plot[value > 0]
    DT_for_plot <- DT_for_plot[市町村名 == input$dynamicMunicipality]
    DT_for_plot[, 年度別割合 := value / sum(value), by = 年度]
    
    ggplot(DT_for_plot,
           aes(x = 年度, y = 施設名_最新,
               fill = 年度別割合*100,
               label = paste0(value, "件\n", round(年度別割合*100, 1), "%"))) +
      geom_tile() +
      geom_text() +
      theme_minimal() +
      scale_fill_gradient(low = "white", high = "red") +
      labs(x = "", y = "", fill = "地域別割合(%)",
           title = paste0(selected_disease, "_", input$operation_selector,
                         "(", input$prefecture, input$dynamicMunicipality, ")"),
           caption = "出典: 厚生労働省DPC導入の影響評価に係る調査「退院患者調査」の結果報告\n『（８）疾患別手術別集計』をもとに作成\n市区町村単位で表示") +
      theme(legend.position = "top")
  })
  
  output$plot_data2_2_municipalities <- renderPlot({
    req(input$dynamicMunicipality, input$mdc_selector, input$prefecture, input$operation_selector)

    selected_prefecture <- input$prefecture
    selected_municipality <- input$dynamicMunicipality
    selected_operation <- input$operation_selector
    selected_mdc <- input$mdc_selector

    DT <- setDT(copy(data_list_filtered()[["data2_2"]]))
    DT_casted <- dcast(DT, 年度 + 市町村名 + MDC + 市町村番号 + 都道府県名 + 施設名_最新 ~ 手術, value.var = "value", fun.aggregate = sum, fill = 0)
    setnames(DT_casted, old = c("有り", "無し"), new = c("手術あり", "手術なし"))
    DT_casted[, `手術あり` := fcoalesce(`手術あり`, 0L)]
    DT_casted[, `手術なし` := fcoalesce(`手術なし`, 0L)]
    DT_casted[, 合計 := `手術あり` + `手術なし`]

    DT_casted <- mutate(DT_casted, value = !!sym(selected_operation))

    DT_casted <- DT_casted[MDC == selected_mdc] # Filter by MDC, use MDC instead of MDC_main
    DT_casted <- DT_casted[value > 0]
    DT_casted <- DT_casted[市町村名 == selected_municipality]

    DT_casted <- DT_casted[, 年度別割合 := value / sum(value), by = 年度]

    ggplot(DT_casted, aes(x = as.integer(年度), y = 施設名_最新, 
                         fill = 年度別割合*100, 
                         label = paste0(value, "件(", round(年度別割合*100, 1), "%", ")"))) +
      geom_tile() +
      geom_text() +
      theme_minimal() +
      scale_fill_gradient(low = "white", high = "red") +
      scale_x_continuous(breaks = seq(as.integer(min_year), as.integer(max_year), by = 1)) +
      labs(x = "", y = "", fill = "割合(%)", 
           title = paste0(selected_mdc, "_", selected_operation, "(", selected_municipality, ")"), 
           caption = "出典: 厚生労働省DPC導入の影響評価に係る調査「退院患者調査」の結果報告\n『（２）MDC別医療機関別件数（割合）』をもとに作成\n市区町村単位で表示") +
      theme(legend.position = "top")
  })
  
  # Download handler
  output$download_plot_all <- downloadHandler(
    filename = function() {
      paste0("hospital_report_", input$dynamicHospital_main, "_", Sys.Date(), ".html")
    },
    content = function(file) {
      # Create temporary directory
      temp_dir <- tempdir()
      
      # Save current working directory
      orig_wd <- getwd()
      
      # Temporary files in temp directory
      temp_report <- file.path(temp_dir, "temp_report.qmd")
      temp_output <- file.path(temp_dir, "temp_report.html")
      
      # Set parameters
      params_list <- list(
        prefecture = input$prefecture,
        municipality = input$dynamicMunicipality,
        hospital_main = input$dynamicHospital_main,
        year = input$year_selector,
        operation = input$operation_selector
      )
      
      # Create YAML header
      yaml_header <- c(
        "---",
        "title: \"Hospital Analysis Report\"",
        "author: \"MITTI\"",
        "format:",
        "  html:",
        "    toc: true",
        "    toc-depth: 3",
        "    number-sections: true",
        "    embed-resources: true",
        "    theme: default",
        "params:"
      )
      
      # Create parameter YAML
      params_yaml <- c(
        paste0("  prefecture: \"", params_list$prefecture, "\""),
        paste0("  municipality: \"", params_list$municipality, "\""),
        paste0("  hospital_main: \"", params_list$hospital_main, "\""),
        paste0("  year: ", params_list$year),
        paste0("  operation: \"", params_list$operation, "\"")
      )
      
      # Complete YAML header
      full_yaml <- c(yaml_header, params_yaml, "---")
      
      # Read hospital_report.qmd content
      report_content <- readLines(here("hospital_report.qmd"))
      
      # Find YAML header section
      yaml_end_idx <- which(report_content == "---")[2]
      
      if (length(yaml_end_idx) > 0 && !is.na(yaml_end_idx)) {
        # Replace YAML header
        modified_content <- c(
          full_yaml,
          report_content[(yaml_end_idx+1):length(report_content)]
        )
      } else {
        # Use as is if no YAML section found
        modified_content <- c(
          full_yaml,
          report_content
        )
      }
      
      # Add final newline
      modified_content <- c(modified_content, "")
      
      # Write to temporary file
      writeLines(modified_content, temp_report)
      
      # Create necessary directories
      dir.create(file.path(temp_dir, "rds"), showWarnings = FALSE, recursive = TRUE)
      dir.create(file.path(temp_dir, "data", "rds"), showWarnings = FALSE, recursive = TRUE)
      
      # Create .here file
      file.create(file.path(temp_dir, ".here"))
      
      # Copy RDS files
      file.copy(
        from = list.files(here("rds"), full.names = TRUE),
        to = file.path(temp_dir, "rds"),
        recursive = TRUE
      )
      
      file.copy(
        from = list.files(here("data", "rds"), full.names = TRUE),
        to = file.path(temp_dir, "data", "rds"),
        recursive = TRUE
      )
      
      # Show progress notification
      showNotification("レポートをレンダリング中です。しばらくお待ちください...",
                      type = "message",
                      duration = NULL,
                      id = "render_notif")
      
      # Change working directory
      setwd(temp_dir)
      
      # Render report
      tryCatch({
        # Render as HTML
        system2("quarto",
                args = c("render", "temp_report.qmd",
                        "--to", "html",
                        "--output", "temp_report.html"))
        
        # Copy if rendering successful
        if (file.exists(temp_output)) {
          file.copy(temp_output, file, overwrite = TRUE)
          removeNotification("render_notif")
          showNotification("レポートが完成しました！",
                         type = "message",
                         duration = 5)
        } else {
          removeNotification("render_notif")
          showNotification("レポートの生成に失敗しました。",
                         type = "error",
                         duration = 10)
        }
      }, error = function(e) {
        removeNotification("render_notif")
        showNotification(paste("エラーが発生しました:", e$message),
                       type = "error",
                       duration = 10)
      }, finally = {
        # Return to original working directory
        setwd(orig_wd)
      })
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server) 
