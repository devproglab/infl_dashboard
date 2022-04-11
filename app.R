#---------------------------------------------------------------------------------------------------
# Load libraries, functions & dependencies
#---------------------------------------------------------------------------------------------------
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
# palette <-
#   adjustcolor(brewer.pal(n = 9, name = 'Set1'), alpha.f = 0.6)
# hse_green = palette[3]
# hse_red   =  palette[1]
# hse_blue  = palette[2]
# Prepare list of models
model_groups <- read_xlsx('data_output/models/model_groups.xlsx')
models <- substr(model_groups$model, 1, nchar(model_groups$model)-6)
groups_counter <- model_groups %>%
  group_by(group) %>%
  summarise(n = n())
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
  div(contents, class='page-content'))
}
# Create grid layout
lay <- function(mainUI) {
  div(
    class = "grid-container",
    div(class = "header", header),
    div(class = "sidenav", navigation),
    div(class = "cards", cards),
    div(class = "main", mainUI, style='overflow:hidden!important;'),
    div(class = "footer", footer)
  )
}

#---------------------------------------------------------------------------------------------------
# Define layout elements and pages
#---------------------------------------------------------------------------------------------------
# Header bar with logo and buttons
header <- tagList(
  div(class = 'logo-block',
  Link(href = '/', img(src = "hse-logo.png", class = "logo")),
  div(Link(href = '/', Text(variant = "xLarge", "Анализ потребительских цен"), class = 'title-link'), class = "title")
  ),
  span(
    TooltipHost(
      content = "Загрузка данных отображаемого графика в формате .xlsx",
      delay = 0,
    CommandBar(
    items = list(
        CommandBarItem("Скачать", "Download", id="download")
    ),
    style = list(width = "100%")
  )), style = "margin-left: auto")
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
        icon = 'CalculatorPercentage'
      ),
      list(
        name = 'Изменение цен - м/м',
        url = '#!/mom',
        key = 'mom',
        icon = 'CalculatorPercentage'
      ),
      list(
        name = 'Структура розничной цены',
        url = '#!/struct',
        key = 'struct',
        icon = 'AreaChart'
      ),
      list(
        name = 'Анализ предложения на рынке',
        url = '#!/market',
        key = 'market',
        icon = 'PieDouble'
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
    Link(href = 'https://disk.yandex.ru/i/-K1zML1CoFWjRA', target='_blank',
      Text(
      variant = "medium",
      nowrap = FALSE,
      "Методологическая справка",
      style = 'padding: 0px 14px 0px 14px'
    )),
    Link(href = 'https://disk.yandex.ru/i/uX5WL4YZDniOqQ', target='_blank',
      Text(
      variant = "medium",
      nowrap = FALSE,
      "Презентация",
      style = "border-left: 1px solid darkgray; padding: 0px 14px 0px 14px"
    ))
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
  contents = div(
    MessageBar("На данном графике жирной линией обозначено, как изменилась цена выбранного товара по сравнению с аналогичным периодом предыдущего года. Это изменение раскладывается на влияние отдельных факторов, релевантных для данного рынка. Под инфляцией спроса понимается изменение динамики спроса на товар, а также влияние общеинфляционных тенденций в экономике на изменение его цены.
Затемненная область соответствует прогнозным значениям, полученным на основе построенной статистической модели. Для прогнозов также приводятся доверительные интервалы, обозначающие диапазон, в котором наиболее вероятно будет находиться значение инфляции для выбранного продукта."
                    , messageBarType='0', isMultiline=FALSE, truncated=TRUE),
    span(downloadLink("download_yearly", "Скачать данные в .xlsx"), style='visibility:hidden; overflow:hidden;'),
    tags$table( width='100%',
      tags$thead(
        tags$th('Динамика цен', width='60%'),
        tags$th('Результаты моделирования, с 2015 года', width='40%')
      ),
      tags$tr(
      tags$td(shinycssloaders::withSpinner(plotlyOutput('price_yoy'), type=6)),
      tags$td(shinycssloaders::withSpinner(tableOutput('impacts'), type=6), valign='top')
      )
    ),
    style='height:600px;'
  )
)
# Period-over-period (w-o-w / m-o-m) inflation page
mom_page <- makePage(
  title = "Изменение цен",
  subtitle = "К предыдущему периоду",
  contents = div(
    MessageBar("На данном графике жирной линией обозначено, как изменилась цена выбранного товара по сравнению с предыдущим периодом - неделей или месяцем. Это изменение раскладывается на влияние отдельных факторов, релевантных для данного рынка, включая сезонность. Под инфляцией спроса понимается изменение динамики спроса на товар, а также влияние общеинфляционных тенденций в экономике на изменение его цены.
Затемненная область соответствует прогнозным значениям, полученным на основе построенной статистической модели. Для прогнозов также приводятся доверительные интервалы, обозначающие диапазон, в котором наиболее вероятно будет находиться значение инфляции для выбранного продукта."
               , messageBarType='0', isMultiline=FALSE, truncated=TRUE),
    span(downloadLink("download_monthly", "Скачать данные в .xlsx"), style='visibility:hidden; overflow:hidden;'),
    shinycssloaders::withSpinner(plotlyOutput('price_mom'), type =6),
    style='height:600px;'
    )
)
# Price structure page
struct_page <- makePage(
  title = "Структура потребительской цены",
  subtitle = "По данным Росстата",
  contents = div(
    MessageBar("На данном графике представлена динамика средней цены выбранного товара в разбивке по основным статьям структуры розничной цены. 
Эти статьи отражают вклад каждого из участников цепочки поставок в формирование конечной потребительской стоимости товара."
               , messageBarType='0', isMultiline=FALSE, truncated=TRUE),
    span(downloadLink("download_str", "Скачать данные в .xlsx"), style='visibility:hidden; overflow:hidden;'),
    shinycssloaders::withSpinner(plotlyOutput('price_structure'), type = 6),
    style='height:600px;'
    )
)
# Market analysis page
market_page <- makePage(
  title = "Анализ предложения на рынке",
  subtitle = "По данным Росстата и ФТС",
  contents = div(
    MessageBar("На данном графике представлена динамика средней цены выбранного товара в разбивке по основным статьям структуры розничной цены. 
Эти статьи отражают вклад каждого из участников цепочки поставок в формирование конечной потребительской стоимости товара."
               , messageBarType='0', isMultiline=FALSE, truncated=TRUE),
    # span(downloadLink("download_str", "Скачать данные в .xlsx"), style='visibility:hidden; overflow:hidden;'),
    # shinycssloaders::withSpinner(plotlyOutput('price_structure'), type = 6),
    style='height:600px;'
  )
)

#---------------------------------------------------------------------------------------------------
# Define unique URLs for pages & setup the UI
#---------------------------------------------------------------------------------------------------
router <- make_router(
  route("/", home_page),
  route("yoy", yoy_page),
  route('mom', mom_page),
  route('struct', struct_page),
  route('market', market_page)
)
ui <- fluentPage(
  lay(router$ui),
  useShinyjs(),
  tags$head(
    htmlOutput('toggler'),
    tags$script(src='tooltiper.js', type="text/javascript"),
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
  # JS workarounds
  output$toggler <- renderUI({
    if (is_page('/')) {
      # Disabling value boxes on the main page
      shinyjs::runjs("$('.cards').css('visibility', 'hidden').css('overflow', 'hidden');") 
      # Disabling download button
      shinyjs::runjs("$('#download').css('display', 'none')") 
      # Highlighting the page in the menu
      script <- ''
      for (i in c('yoy', 'mom', 'struct')) {
        script <- paste(script, 
                        paste("$('[href=", '"', "#!/", i, '"', "]').addClass('unchosen_page'); ", sep = ""), sep="")
      }
      shinyjs::runjs(script) 
      shinyjs::runjs(paste("$('[href=", '"', "#!/", '"', "]').removeClass('unchosen_page').addClass('chosen_page')", sep = "")) 
    } else {
      # Enabling value boxes on other pages
      shinyjs::runjs("$('.cards').css('visibility', 'visible').css('overflow', 'visible');") 
      # Enabling download button
      shinyjs::runjs("$('#download').css('display', 'block')") 
      # Highlighting pages in the menu
      if (is_page('yoy')) {
        script <- ''
        for (i in c('', 'mom', 'struct')) {
          script <- paste(script, 
                      paste("$('[href=", '"', "#!/", i, '"', "]').removeClass('unchosen_page').addClass('unchosen_page'); ", sep = ""), sep="")
        }
        shinyjs::runjs(script) 
        shinyjs::runjs(paste("$('[href=", '"', "#!/yoy", '"', "]').addClass('chosen_page')", sep = "")) 
        shinyjs::runjs("$('#download').click(function(){ window.location.href = $('#download_yearly').attr('href');});") 

      }
      if (is_page('mom')) {
        script <- ''
        for (i in c('', 'yoy', 'struct')) {
          script <- paste(script, 
                          paste("$('[href=", '"', "#!/", i, '"', "]').removeClass('unchosen_page').addClass('unchosen_page'); ", sep = ""), sep="")
        }
        shinyjs::runjs(script) 
        shinyjs::runjs(paste("$('[href=", '"', "#!/mom", '"', "]').addClass('chosen_page')", sep = "")) 
        shinyjs::runjs("$('#download').click(function(){ window.location.href = $('#download_monthly').attr('href');});")
      }
      if (is_page('struct')) {
        script <- ''
        for (i in c('', 'mom', 'yoy')) {
          script <- paste(script, 
                          paste("$('[href=", '"', "#!/", i, '"', "]').removeClass('unchosen_page').addClass('unchosen_page'); ", sep = ""), sep="")
        }
        shinyjs::runjs(script) 
        shinyjs::runjs(paste("$('[href=", '"', "#!/struct", '"', "]').addClass('chosen_page')", sep = "")) 
        shinyjs::runjs("$('#download').click(function(){ window.location.href = $('#download_str').attr('href');});") 
      }
    }
    # Function to highlight the chosen model in the menu
    tags$script(HTML(
      paste("function highlight() {if ($('.chosen_model').length == 0) { $('[label=", '"', 
            model()$model_name, '"', "]').addClass('chosen_model')}
      } $('.chosen_model').removeClass('chosen_model');", 
            sep = "")
    ))
  })
  # Ensure the chosen model is constantly highlighted
  shinyjs::runjs('setInterval( function(){highlight(); tips()}, 1)') 
  # Load the model for a product
  model <- reactive({
    req(input$prod_select)
    model_name = input$prod_select
    model <- loadR(model_name)
    return(model)
  })
  # Create value boxes
  output$card_set <- renderUI({
    # Load model data
    estimated <- model()
    weight = estimated$weight
    model_name <- estimated$model_name
    n.ahead <- estimated$n.ahead
    weekly <- estimated$weekly
    price = round(last(estimated$df[[1]]), 2)
    
    if (weekly) {
    # Sum up 4 latest data points for a weekly model + get forecast
      length <- nrow(estimated$dfmod_unsmoothed)
      period_change = sum(estimated$dfmod_unsmoothed[(length - n.ahead + 1 - 4):(length - n.ahead), 1])
      if (!n.ahead==0) {
        forecast_period = xts(t(colSums(round(estimated$forecast[2:5] * 100, 2))), order.by=zoo::index(estimated$forecast[5]))
      } else {
        forecast_period=NULL
      }
    } else {
    # Get the latest data point for a monthly model 
      period_change = nth(estimated$dfmod_unsmoothed[, 1],-(n.ahead + 1))
      if (!n.ahead==0) {
        forecast_period = round(estimated$forecast[2] * 100, 2)
      } else {
        forecast_period=NULL
      }
    }
    
    # Get y-o-y change and define ruble symbol
    yoy_change = nth(estimated$decomp2[, 1],-1)
    forecast_yoy = round(estimated$forecast2[2] * 100, 2)
    rub <- '\U20BD'
    
    # Get date of the latest data point and create a label
    date_current = nth(zoo::index(estimated$dfmod_unsmoothed),-(n.ahead +
                                                                  1))
    label_current <-
      paste(day(as.Date(date_current)), ' ', labelset[month(as.Date(date_current))], '-', substr(year(as.Date(date_current)), 3, 4), sep =
              '')
    
    # Get date of the previous data point and create a label
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
    
    # Get date of the forecasted data point and create a label
    if (weekly) {
      date_fcast_period = as.Date(zoo::index(estimated$forecast[5]))
    } else {
      date_fcast_period = as.Date(zoo::index(forecast_period))
    }
    label_fcast_period <-
      paste(day(as.Date(date_fcast_period)), ' ', labelset[month(as.Date(date_fcast_period))], '-', substr(year(as.Date(
        date_fcast_period
      )), 3, 4), sep = '')
    
    # Get date of the forecasted y-o-y data point and create a label
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
    
    # Stack 4 cards together
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
        # m-o-m card
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
      plotdecomp(model(), yoy = 1)
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
  output$impacts <- renderTable({
    estimated <- model()
    shares <- as.data.frame(round(estimated$shares,2))
    impacts <- as.data.frame(round(estimated$cump_impact,2))
    final <- cbind(shares, impacts)
    other <- last(final)
    final <- final[-nrow(final),]
    final <- final[order(final[,1], decreasing=TRUE),]
    final <- rbind(final, other)
    final <- cbind(rownames(final), final)
    names(final) <- c('Фактор', 'Объясняющаяя способность', 'Накопленный вклад в цену')
    final[,2] <- paste(final[,2], '%', sep='')
    final[,3] <- paste(final[,3], ' руб.', sep='')
    final
    }, align='lcc')
  # Generate y-o-y downloadable .xlsx
  output$download_yearly <- downloadHandler(
    filename = function() {
      model_name = model()$model_name
      paste(model_name, "_г_г.xlsx", sep="")
    },
    content = function(file) {
      model_name = model()$model_name
      estimated <- loadR(model_name)
      data <- data.frame(estimated$decomp2)
      data$date <- as.Date((rownames(data)))
      data <- data[,c(ncol(data),(1:ncol(data)-1))]
      names(data) <- c('Дата', 'Изменение цены г/г', 'Инфляция спроса', estimated$varlabels, 'Прочие факторы')
      write_xlsx(data, file)
    })
  # Generate m-o-m downloadable .xlsx
  output$download_monthly <- downloadHandler(
    filename = function() {
      model_name = model()$model_name
      paste(model_name, "_м_м.xlsx", sep="")
    },
    content = function(file) {
      model_name = model()$model_name
      estimated <- loadR(model_name)
      data <- data.frame(estimated$decomp)
      data$date <- as.Date(rownames(data))
      data <- data[,c(ncol(data),(1:ncol(data)-1))]
      names(data) <- c('Дата', 'Изменение цены м/м', 'Инфляция спроса', estimated$varlabels, 'Сезонность', 'Прочие факторы')
      write_xlsx(data, file)
    })
  # Generate price structure downloadable .xlsx
  output$download_str <- downloadHandler(
    filename = function() {
      model_name = model()$model_name
      estimated <- loadR(model_name)
      paste(model_name, "_cтруктура.xlsx", sep="")
    },
    content = function(file) {
      model_name = model()$model_name
      estimated <- loadR(model_name)
      data <- estimated$price_decomp
      names <- colnames(data)
      data <- data.frame(data)
      data$price <- rowSums(data)
      data$date <- as.Date(rownames(data))
      data <- data[,c(ncol(data), (ncol(data)-1), 1:(ncol(data)-2))]
      names(data) <- c('Дата', 'Цена', names)
      write_xlsx(data, file)
    })
}
# Start Shiny App
shinyApp(ui, server)
