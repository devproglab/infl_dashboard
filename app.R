#---------------------------------------------------------------------------------------------------
# Load libraries, functions & dependencies
#---------------------------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(glue)
library(leaflet)
library(plotly)
library(sass)
library(shiny)
library(shiny.fluent)
library(shiny.router)
library(shinycssloaders)
library(readxl)
library(shinyjs)
source('plots.R')
shiny::addResourcePath("shiny.router", system.file("www", package = "shiny.router"))
shiny_router_js_src <- file.path("shiny.router", "shiny.router.js")
shiny_router_script_tag <-
  shiny::tags$script(type = "text/javascript", src = shiny_router_js_src)

#---------------------------------------------------------------------------------------------------
# Technical preparations
#---------------------------------------------------------------------------------------------------
# Set encoding
Sys.setlocale('LC_TIME', 'Russian')
options(encoding = "UTF-8")
# Prepare Russian month labels for graphs
labelset <- clock_labels_lookup('ru')[[2]]
labelset <- substr(labelset, 1, nchar(labelset) - 1)
labelset[5] <- 'май'
# Prepare color palette
palette <-
  adjustcolor(brewer.pal(n = 9, name = 'Set1'), alpha.f = 0.6)
hse_green = palette[3]
hse_red   =  palette[1]
hse_blue  = palette[2]
# Prepare list of models
model_groups <- read_xlsx('data_output/models/model_groups.xlsx')
models <- substr(model_groups$model, 1, nchar(model_groups$model)-6)
groups_counter <- model_groups %>%
  group_by(group) %>%
  summarize(n = n())
groups_counter <- groups_counter[match(unique(model_groups$group), groups_counter$group),]
groups_counter$cumsum <- cumsum(groups_counter$n)
groups_nav <- list()
for (i in 1:nrow(groups_counter)) {
  elem <- list(key = paste('g', i, sep=''), name = groups_counter$group[i], startIndex = groups_counter$cumsum[i] - groups_counter$n[i], count = groups_counter$n[i], isCollapsed = ifelse(i==1, FALSE, TRUE))
  groups_nav[[i]] <- elem
}

prods_list <- as.list(models)
options <- list(list(key = models[1], text = models[1]))
for (i in 2:length(models)) {
  elem <- list(key = models[i], text = models[i])
  options[[i]] <- elem
}

#---------------------------------------------------------------------------------------------------
# Define helper functions
#---------------------------------------------------------------------------------------------------
# Create value box
makeCard <-
  function(title,
           tooltip = NULL,
           content = NULL,
           title2 = NULL,
           tooltip2 = NULL,
           content2 = NULL,
           size = 12,
           style = NULL,
           card_type = 'neutral',
           icon) {
    div(
      class = glue("card card-{card_type} ms-depth-8 ms-sm{size} ms-xl{size}"),
      style = style,
      content,
      Stack(
        tokens = list(childrenGap = 5),
        Stack(
          horizontal = TRUE,
          Text(variant = "xxLarge", title, block = TRUE, nowrap = TRUE),
          if (!is.null(tooltip)) {
            TooltipHost(
              content = tooltip,
              delay = 0,
              Text(FontIcon(iconName = 'info'))
            )
          }
        ),
        if (is.null(title2)) {
          tagList(br(), br())
        },
        content2,
        if (!is.null(title2)) {
          Stack(
            horizontal = TRUE,
            Text(variant = "xxLarge", title2, block = TRUE, nowrap = TRUE),
            if (!is.null(tooltip2)) {
              TooltipHost(
                content = tooltip2,
                delay = 0,
                Text(FontIcon(iconName = 'info'))
              )
            }
          )        },
        div(class = 'icon',
            tags$i(class = paste('fa', icon, sep = ' '
            )))
      )
    )
  }
# Create a single page
makePage <- function (title, subtitle, contents) {
  tagList(div(
    class = "page-title",
    span(title, class = "ms-fontSize-32 ms-fontWeight-semibold", style =
           "color: #323130"),
    br(),
    span(subtitle, class = "ms-fontSize-20 ms-fontWeight-regular", style =
           "color: #605E5C; margin-top: 14px;")
  ),
  contents)
}
# Create grid layout
lay <- function(mainUI) {
  div(
    class = "grid-container",
    div(class = "header", header),
    div(class = "sidenav", navigation),
    div(class = "cards", cards),
    div(class = "main", mainUI),
    div(class = "footer", footer)
  )
}

#---------------------------------------------------------------------------------------------------
# Define layout elements and pages
#---------------------------------------------------------------------------------------------------
# Header bar with logo and buttons
header <- tagList(
  img(src = "hse-logo.png", class = "logo"),
  div(Text(variant = "xLarge", "Анализ потребительских цен"), class = "title"),
  span(CommandBar(
    items = list(
      CommandBarItem("Поделиться ссылкой", "Share"),
      CommandBarItem("Скачать данные", "Download")
    ),
    farItems = list(CommandBarItem("Справка", "Info", iconOnly = TRUE)),
    style = list(width = "100%")
  ), style = "margin-left: auto")
)
# Sidebar navigation & product choice
navigation <- tagList (
 
  Nav(
    groups = list(list(links = list(
      list(
        name = 'Общие сведения',
        url = '#!/',
        key = 'home',
        icon = 'Home'
      ),
      list(
        name = 'Изменение цен - г/г',
        url = '#!/yoy',
        key = 'yoy',
        icon = 'Home'
      ),
      list(
        name = 'Изменение цен - м/м',
        url = '#!/mom',
        key = 'mom',
        icon = 'Home'
      ),
      list(
        name = 'Структура розничной цены',
        url = '#!/struct',
        key = 'struct',
        icon = 'Home'
      )
    ))),
    initialSelectedKey = 'home',
    styles = list(
      root = list(
        height = '100%',
        boxSizing = 'border-box',
        overflowY = 'auto'
      )
    )
  ),
  Text('Анализируемые товары:', variant='large'),
  GroupedList(
    items = prods_list,
    groups = groups_nav,
    selectionMode = 0,
    onShouldVirtualize = FALSE,
    onRenderCell = JS("(depth, item) => (
        jsmodule['react'].createElement('div', { className : 'model_choice', style: { paddingLeft: 20 }, label: item, onClick : () => {Shiny.setInputValue('prod_select', item) } }, item)
      )")
  ),
  div(
    textInput('prod_select', label = NULL, value = models[1], ),
    style = 'display:none'
  )
)

# Footer with credits and links
footer <- Stack(
  horizontal = TRUE,
  horizontalAlign = 'space-between',
  tokens = list(childrenGap = 20),
  Text(
    variant = "medium",
    "Центр экономической экспертизы ИГМУ ВШЭ",
    block = FALSE
  ),
  Stack(
    horizontal = TRUE,
    horizontalAlign = 'end',
    Text(
      variant = "medium",
      nowrap = FALSE,
      "Методологическая справка",
      style = 'padding: 0px 14px 0px 14px'
    ),
    Text(
      variant = "medium",
      nowrap = FALSE,
      "Презентация",
      style = "border-left: 1px solid darkgray; padding: 0px 14px 0px 14px"
    )
  )
)
# Load value boxes
cards <- htmlOutput('card_set')
# Home page
home_page <- makePage(title = 'Факторный анализ потребительских цен',
                      subtitle = "Общие сведения",
                      contents = htmlOutput('main_loader'))
# Y-o-y inflation page
yoy_page <- makePage(
  title = "Изменение цен",
  subtitle = "К аналогичному периоду предыдущего года",
  contents = div(shinycssloaders::withSpinner(plotlyOutput('price_yoy'), type =
                                                6))
)
# Period-over-period (w-o-w / m-o-m) inflation page
mom_page <- makePage(
  title = "Изменение цен",
  subtitle = "К предыдущему периоду",
  contents = div(shinycssloaders::withSpinner(plotlyOutput('price_mom'), type =
                                                6))
)
# Price structure page
struct_page <- makePage(
  title = "Структура потребительской цены",
  subtitle = "По данным Росстата",
  contents = div(shinycssloaders::withSpinner(plotlyOutput('price_structure'), type =
                                                6))
)

#---------------------------------------------------------------------------------------------------
# Define unique URLs for pages & setup the UI
#---------------------------------------------------------------------------------------------------
router <- make_router(
  route("/", home_page),
  route("yoy", yoy_page),
  route('mom', mom_page),
  route('struct', struct_page)
)
ui <- fluentPage(
  lay(router$ui),
  useShinyjs(),
  tags$head(
    htmlOutput('cards_toggler'),
    # htmlOutput('model_toggler'),
    tags$link(href = "style.css", rel = "stylesheet", type = "text/css"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta2/css/all.min.css"),
    shiny_router_script_tag
  )
)

#---------------------------------------------------------------------------------------------------
# Define server-side dynamic functions
#---------------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  router$server(input, output, session)
  # Hide value boxes on the main page
  output$cards_toggler <- renderUI({
    if (is_page('/')) {
      tags$style('.cards {visibility:hidden!important; overflow:hidden!important;}')
    } else {
      tags$style('.cards {visibility:visible!important; overflow:visible!important;}')
    }
  })
  # Logics for highlighting the chosen model
  output$main_loader <- renderUI({
    tags$script(HTML(
      paste("function highlight() {if ($('.chosen_model').length == 0) { $('[label=", '"', 
            model()$model_name, '"', "]').addClass('chosen_model')}
      } $('.chosen_model').removeClass('chosen_model');", 
            sep = "")
    ))
  })
  shinyjs::runjs('
  setInterval( function(){ 
  highlight()}, 1)') 
  # output$model_toggler <- renderUI({
  #   script <- paste("$('.chosen_model').removeClass('chosen_model'); ",
  #                   "$('[label=", '"', model()$model_name, '"', "]').addClass('chosen_model');",
  #                   sep='')
  #   tags$script(HTML(
  #     script
  #   ))
  # })

  # Load the model for a product
  model <- reactive({
    req(input$prod_select)
    model_name = input$prod_select
    model <- loadR(model_name)
    return(model)
  })
  # Create value boxes
  output$card_set <- renderUI({
    estimated <- model()
    weight = estimated$weight
    model_name <- estimated$model_name
    n.ahead <- estimated$n.ahead
    weekly <- estimated$weekly
    price = round(last(estimated$df[[1]]), 2)
    
    if (weekly) {
      length <- nrow(estimated$dfmod_unsmoothed)
      period_change = sum(estimated$dfmod_unsmoothed[(length - n.ahead + 1 - 4):(length - n.ahead), 1])
      forecast_period = xts(t(colSums(round(estimated$forecast[2:5] * 100, 2))), order.by=zoo::index(estimated$forecast[5]))
    } else {
      period_change = nth(estimated$dfmod_unsmoothed[, 1],-(n.ahead + 1))
      forecast_period = round(estimated$forecast[2] * 100, 2)
    }
    
    yoy_change = nth(estimated$decomp2[, 1],-1)
    forecast_yoy = round(estimated$forecast2[2] * 100, 2)
    rub <- '\U20BD'
    
    date_current = nth(zoo::index(estimated$dfmod_unsmoothed),-(n.ahead +
                                                                  1))
    label_current <-
      paste(day(as.Date(date_current)), ' ', labelset[month(as.Date(date_current))], '-', substr(year(as.Date(date_current)), 3, 4), sep =
              '')
    if (weekly) {
      date_prev_period = nth(zoo::index(estimated$dfmod_unsmoothed),-(n.ahead +
                                                                        5))
    } else {
      date_prev_period = nth(zoo::index(estimated$dfmod_unsmoothed),-(n.ahead +
                                                                        2))
    }
    
    label_prev_period <-
      paste(day(as.Date(date_prev_period)), ' ', labelset[month(as.Date(date_prev_period))], '-', substr(year(as.Date(
        date_prev_period
      )), 3, 4), sep = '')
    
    if (weekly) {
      date_fcast_period = as.Date(zoo::index(estimated$forecast[5]))
    } else {
      date_fcast_period = as.Date(zoo::index(forecast_period))
    }
    
    label_fcast_period <-
      paste(day(as.Date(date_fcast_period)), ' ', labelset[month(as.Date(date_fcast_period))], '-', substr(year(as.Date(
        date_fcast_period
      )), 3, 4), sep = '')
    
    date_prev_yoy = nth(zoo::index(estimated$decomp2[, 1]),
                        ifelse(weekly,-53,-13))
    date_next_yoy = nth(zoo::index(estimated$decomp2[, 1]),
                        ifelse(weekly,-52,-12))
    date_fcast_yoy = as.Date(zoo::index(forecast_yoy))
    label_prev_yoy <-
      paste(day(as.Date(date_prev_yoy)), ' ', labelset[month(as.Date(date_prev_yoy))], '-', substr(year(as.Date(date_prev_yoy)), 3, 4), sep =
              '')
    label_fcast_yoy <-
      paste(day(as.Date(date_fcast_yoy)), ' ', labelset[month(as.Date(date_fcast_yoy))], '-', substr(year(as.Date(date_fcast_yoy)), 3, 4), sep =
              '')
    label_next_yoy <-
      paste(day(as.Date(date_next_yoy)), ' ', labelset[month(as.Date(date_next_yoy))], '-', substr(year(as.Date(date_next_yoy)), 3, 4), sep =
              '')
    Stack(
      div(model_name, style = 'margin-bottom:10px;', class = 'ms-fontSize-20 ms-fontWeight-regular'),
      Stack(
        horizontal = TRUE,
        horizontalAlign = 'space-between',
        tokens = list(childrenGap = 10),
        # Current price card
        makeCard(
          title = paste(
            price,
            ' ',
            rub,
            '/',
            substr(
              model_name,
              regexpr("([^, ]*$)", model_name)[1],
              stop = nchar(model_name)
            ),
            sep = ''
          ),
          content2 = Text(paste('Цена, ', label_current, sep = ''), variant = 'mediumPlus'),
          size = 10,
          card_type = 'neutral',
          icon = 'fa-coins'
        ),
        # w-o-w / m-o-m card
        makeCard(
          content = Text('Инфляция:', variant = 'mediumPlus'),
          content2 = Text('Прогноз:', variant = 'mediumPlus'),
          title = paste(
            ifelse(period_change > 0, '+', ''),
            round(period_change * 100, 2),
            '%, м/м',
            sep = ''
          ),
          tooltip = paste(label_current, '/', label_prev_period),
          tooltip2 = paste(label_fcast_period, '/', label_current),
          title2 = paste(
            ifelse(forecast_period$value > 0, '+', ''),
            round(forecast_period$value, 2),
            '%, м/м',
            sep = ''
          ),
          size = 10,
          card_type = ifelse(period_change > 0, "bad", "good"),
          icon = ifelse(period_change > 0, "fa-arrow-up", "fa-arrow-down")
        ),
        # y-o-y card
        makeCard(
          title = paste(
            ifelse(yoy_change > 0, '+', ''),
            round(yoy_change * 100, 2),
            '%, г/г',
            sep = ''
          ),
          tooltip = paste(label_current, '/', label_prev_yoy),
          content = Text('Инфляция:', variant = 'mediumPlus'),
          content2 = Text('Прогноз:', variant = 'mediumPlus'),
          title2 = paste(
            ifelse(forecast_yoy$value > 0, '+', ''),
            round(forecast_yoy$value, 2),
            '%, г/г',
            sep = ''
          ),
          tooltip2 = paste(label_fcast_yoy, ' / ', label_next_yoy, sep =
                  ''),
          size = 10,
          card_type = ifelse(yoy_change > 0, "bad", "good"),
          icon = ifelse(yoy_change > 0, "fa-arrow-up", "fa-arrow-down")
        ),
        # CPI weight card
        if (is.na(weight)) {
          makeCard(
            title = Text('Вес в ИПЦ недоступен', variant = 'mediumPlus'),
            size = 10,
            card_type = 'neutral',
            icon = 'fa-shopping-basket'
          )
        } else {
          makeCard(
            title = paste(weight, '%', sep = ''),
            content2 = Text('Вес в ИПЦ', variant = 'mediumPlus'),
            size = 10,
            card_type = 'neutral',
            icon = 'fa-shopping-basket'
          )
        }
      )
    )
  })
  # Plot y-o-y inflation graph when on respective page
  output$price_yoy <- renderPlotly({
    if (is_page('yoy')) {
      plotdecomp(model(), yoy = 1)
    }
  })
  # Plot w-o-w / m-o-m inflation graph when on respective page
  output$price_mom <- renderPlotly({
    if (is_page('mom')) {
      plotdecomp(model(), yoy = 0)
    }
  })
  # Plot price structure graph when on respective page if data exists
  output$price_structure <- renderPlotly({
    if (is_page('struct')) {
      validate(need(
        !is.null(model()$price_decomp),
        "Данные по структуре розничной цены данного товара недоступны"
      ))
      try(plot_price_decomp(model()))
    }
  })
}
# Start Shiny App
shinyApp(ui, server)
