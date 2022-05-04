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

# Prepare list of supply markets for analysis
market_groups <- read_xlsx('data_output/supply_analysis/market_groups.xlsx')
markets <- substr(market_groups$model, 1, nchar(market_groups$model)-6)
groups_counter <- market_groups %>%
  group_by(group) %>%
  summarise(n = n())
groups_counter <- groups_counter[match(unique(market_groups$group), groups_counter$group),]
groups_counter$cumsum <- cumsum(groups_counter$n)
groups_nav_supply <- list()
for (i in 1:nrow(groups_counter)) {
  elem <- list(key = paste('g', i, sep=''), name = groups_counter$group[i], startIndex = groups_counter$cumsum[i] - groups_counter$n[i], count = groups_counter$n[i], isCollapsed = ifelse(i==1, FALSE, TRUE))
  groups_nav_supply[[i]] <- elem
}
prods_list_supply <- as.list(markets)
options <- list(list(key = markets[1], text = markets[1]))
for (i in 2:length(markets)) {
  elem <- list(key = markets[i], text = markets[i])
  options[[i]] <- elem
}


rus_map <- readRDS('data_output/federal_districts.rds')

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
  )
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
        name = 'Потребительские цены',
        url = '#!/cons',
        key = 'cons',
        icon = 'CalculatorPercentage'
      ),
      list(
        name = 'Цены производителей, импорта и экспорта',
        url = '#!/prod',
        key = 'prod',
        icon = 'CalculatorPercentage'
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
  htmlOutput('sidebar_choice')
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
    )),
    reactOutput("reactPanel")
  )
)
# Load value boxes
cards <- htmlOutput('card_set')
# Home page
home_page <- makePage(title = 'Факторный анализ потребительских цен',
                      subtitle = "Общие сведения",
                      contents = htmlOutput('main_loader'))
# constuct tabs for Consumer Prices page
pivot_consumer <- Pivot(
  PivotItem(headerText = "Изменение цен - г/г", 
            tags$br(),
            tags$span(
            tags$table(tags$tr(
              tags$td("На данном графике жирной линией обозначено, как изменилась цена выбранного товара по сравнению с аналогичным периодом предыдущего года. Это изменение раскладывается на влияние отдельных факторов, релевантных для данного рынка. Под инфляцией спроса понимается изменение динамики спроса на товар, а также влияние общеинфляционных тенденций в экономике на изменение его цены.
Затемненная область соответствует прогнозным значениям, полученным на основе построенной статистической модели. Для прогнозов также приводятся доверительные интервалы, обозначающие диапазон, в котором наиболее вероятно будет находиться значение инфляции для выбранного продукта."
                      , width='80%'),
              tags$td(span(CommandBar(
                  items = list(
                    CommandBarItem("Скачать", "Download", id="download_yearly", onClick=JS("function() { window.location.href = $('#download_yearly_shadow').attr('href'); }")),
                    CommandBarItem("Пояснения к факторам", "Info", id="info_panel", onClick=JS("function() { Shiny.setInputValue('showPanel', Math.random()); }"))
                  )
                ),style="display:block;"), width='20%', align='right')
            )),
            class="ms-depth-8 text-card"),
            tags$table( width='100%',
                        tags$tr(
                          tags$td('Динамика цен', width='60%'),
                          tags$td('Результаты моделирования, с 2015 года', width='40%'),
                          style="font-weight: bold; text-align:center;"
                        ),
                        tags$tr(
                          tags$td(
                            plotlyOutput('price_yoy')),
                          tags$td(tableOutput('impacts'), valign='top')
                        )
            )),
  PivotItem(headerText = "Изменение цен - м/м", 
            tags$br(),
            tags$span(
              tags$table(tags$tr(
                tags$td("На данном графике жирной линией обозначено, как изменилась цена выбранного товара по сравнению с аналогичным периодом предыдущего года. Это изменение раскладывается на влияние отдельных факторов, релевантных для данного рынка. Под инфляцией спроса понимается изменение динамики спроса на товар, а также влияние общеинфляционных тенденций в экономике на изменение его цены.
Затемненная область соответствует прогнозным значениям, полученным на основе построенной статистической модели. Для прогнозов также приводятся доверительные интервалы, обозначающие диапазон, в котором наиболее вероятно будет находиться значение инфляции для выбранного продукта."
                        , width='80%'),
                tags$td(span(CommandBar(
                  items = list(
                    CommandBarItem("Скачать", "Download", id="download_monthly", onClick=JS("function() { window.location.href = $('#download_mom_shadow').attr('href'); }")),
                    CommandBarItem("Пояснения к факторам", "Info", id="info_panel", onClick=JS("function() { Shiny.setInputValue('showPanel', Math.random()); }"))
                  )
                ), style="display:block;"), width='20%', align='right')
              )),
              class="ms-depth-8 text-card"),
            tags$table(width='100%', 
                       tags$thead(
                         tags$th('Динамика цен', width='60%'),
                         tags$th('Пояснения', width='40%')
                       ),
                       tags$tr(tags$td(plotlyOutput('price_mom')))
                       )),
  PivotItem(headerText = "Структура розничной цены", 
            tags$br(),
            tags$span(
              tags$table(tags$tr(
                tags$td("На данном графике жирной линией обозначено, как изменилась цена выбранного товара по сравнению с аналогичным периодом предыдущего года. Это изменение раскладывается на влияние отдельных факторов, релевантных для данного рынка. Под инфляцией спроса понимается изменение динамики спроса на товар, а также влияние общеинфляционных тенденций в экономике на изменение его цены.
Затемненная область соответствует прогнозным значениям, полученным на основе построенной статистической модели. Для прогнозов также приводятся доверительные интервалы, обозначающие диапазон, в котором наиболее вероятно будет находиться значение инфляции для выбранного продукта."
                        , width='80%'),
                tags$td(span(CommandBar(
                  items = list(
                    CommandBarItem("Скачать", "Download", id="download_struct", onClick=JS("function() { window.location.href = $('#download_str_shadow').attr('href'); }"))                  )
                ), style="display:block;"), width='20%', align='right')
              )),
              class="ms-depth-8 text-card"),
            tags$table(width='100%', 
                       tags$thead(
                         tags$th('Динамика цен', width='60%'),
                         tags$th('Пояснения', width='40%')
                       ),
                       tags$tr(
              tags$td(plotlyOutput('price_structure'))
            )
            ))
)
# Consumer prices page
cons_page <- makePage(
  title = "Динамика потребительских цен",
  subtitle = "",
  contents = div(
    div(class = "cards", cards),
    span(downloadLink("download_yearly_shadow", "Скачать данные в .xlsx"),
         downloadLink("download_mom_shadow", "Скачать данные в .xlsx"),
         downloadLink("download_str_shadow", "Скачать данные в .xlsx"),
         style='visibility:hidden; overflow:hidden;'),
    pivot_consumer,
    style='height:600px;'
  )
)
# Producer, import & export prices page
prod_page <- makePage(
  title = "Динамика цен производителей, импорта и экспорта",
  subtitle = ".....",
  contents = div(

    style='height:600px;'
  )
)

# Market analysis page
datepicker <- airMonthpickerInput(inputId = "date_map",
                                  inline = TRUE,
                                  language = 'ru',
                                  value = as.Date('2022-02-01'),
                                  minDate = as.Date('2011-01-01'),
                                  maxDate = as.Date('2022-02-01'))
pivot_graphs <- tags$table(width='100%', tags$tr(
  tags$td(
    shinycssloaders::withSpinner(plotlyOutput('supply_analysis'), type=6),
    width="100%", colspan=2)
),
tags$tr(tags$td(
  h4('На графике ниже представлена географическая структура производства товара')
)),
tags$tr(
  tags$td(
    # shinycssloaders::withSpinner(plotlyOutput('supply_map'), type=6)
    plotlyOutput('map_sa')
  )
)
)
market_page <- makePage(
  title = "Анализ предложения на рынке",
  subtitle = "По данным Росстата и ФТС",
  contents = div(
    tags$br(),
    tags$div(
      tags$div("На данном графике жирной линией обозначено, как изменилась цена выбранного товара по сравнению с аналогичным периодом предыдущего года. Это изменение раскладывается на влияние отдельных факторов, релевантных для данного рынка. Под инфляцией спроса понимается изменение динамики спроса на товар, а также влияние общеинфляционных тенденций в экономике на изменение его цены.
Затемненная область соответствует прогнозным значениям, полученным на основе построенной статистической модели. Для прогнозов также приводятся доверительные интервалы, обозначающие диапазон, в котором наиболее вероятно будет находиться значение инфляции для выбранного продукта.",
               style="flex-basis: 70%;"),
      tags$div(
        tags$div(Toggle.shinyInput("sa_toggle",value = TRUE,
                                   label = "",
                                   onText = "Сезонное сглаживание включено",
                                   offText = "Сезонное сглаживание отключено"
        )),
        tags$div(CommandBar(
          items = list(
            CommandBarItem("Скачать", "Download", id="download_yearly", onClick=JS("function() { window.location.href = $('#download_yearly_shadow').attr('href'); }"))
          ))),
        style='display:flex; flex-wrap:nowrap; flex-direction:column;justify-content: space-around;align-items:center;'),
      style="display:flex; flex-wrap:nowrap;justify-content: space-around;", class="text-card ms-depth-8"),
#     
#     tags$span(
#       tags$table(tags$tr(
#         tags$td("На данном графике жирной линией обозначено, как изменилась цена выбранного товара по сравнению с аналогичным периодом предыдущего года. Это изменение раскладывается на влияние отдельных факторов, релевантных для данного рынка. Под инфляцией спроса понимается изменение динамики спроса на товар, а также влияние общеинфляционных тенденций в экономике на изменение его цены.
# Затемненная область соответствует прогнозным значениям, полученным на основе построенной статистической модели. Для прогнозов также приводятся доверительные интервалы, обозначающие диапазон, в котором наиболее вероятно будет находиться значение инфляции для выбранного продукта."
#                 , width='80%'),
#         
#         tags$td(,span(
#           
#           
#         ),style="display:block;"), width='20%', align='center')
#       )),
#       class="ms-depth-8 text-card"),
    # MessageBar("В данном разделе представлен анализ совокупного предложения рассматриваемого товара на рынке. Ресурсы, доступные для потребления,
    #            рассчитываются как сумма внутреннего производства и импортных поставок за вычетом экспорта и изменения запасов."
    #            , messageBarType='0', isMultiline=FALSE, truncated=TRUE),
    # span(downloadLink("download_str", "Скачать данные в .xlsx"), style='visibility:hidden; overflow:hidden;'),
    # shinycssloaders::withSpinner(plotlyOutput('supply_analysis'), type = 6),
    tags$table(width="100%",
               
               
               tags$tr(
                 tags$td(pivot_graphs, width="80%"),
                 tags$td(datepicker, width="20%", valign='bottom', align='center', style='padding-bottom:10%;')
               )),
    style='height:1300px;')
  )



#---------------------------------------------------------------------------------------------------
# Define unique URLs for pages & setup the UI
#---------------------------------------------------------------------------------------------------
router <- make_router(
  route("/", home_page),
  route("cons", cons_page),
  route("prod", prod_page),
  route('market', market_page)
)
ui <- fluentPage(
  lay(router$ui),
  useShinyjs(),
  tags$head(
    tags$script(src='tooltiper.js', type="text/javascript"),
    # tags$link(href="https://static2.sharepointonline.com/files/fabric/office-ui-fabric-core/7.2.0/css/fabric.min.css"),
    tags$link(href = "style.css", rel = "stylesheet", type = "text/css"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta2/css/all.min.css"),
    shiny_router_script_tag,
  ),
  htmlOutput('toggler')
)

#---------------------------------------------------------------------------------------------------
# Define server-side dynamic functions
#---------------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  router$server(input, output, session)
  # JS workarounds
  output$sidebar_choice <- renderUI({
      tags$span(
        tags$span(
        GroupedList(
        items = prods_list,
        groups = groups_nav,
        selectionMode = 0,
        onShouldVirtualize = FALSE,
        onRenderCell = JS("(depth, item) => (
        jsmodule['react'].createElement('div', { className : 'model_choice', style: { paddingLeft: 20 }, label: item, onClick : () => {Shiny.setInputValue('prod_select', item) } }, item)
      )")),
      div(
        textInput('prod_select', label = NULL, value = models[1]),
        style = 'display:none'
      ), id='choice_models', style='display:block;'
    ),
      tags$span(GroupedList(
        items = prods_list_supply,
        groups = groups_nav_supply,
        selectionMode = 0,
        onShouldVirtualize = FALSE,
        onRenderCell = JS("(depth, item) => (
        jsmodule['react'].createElement('div', { className : 'market_choice', style: { paddingLeft: 20 }, label: item, onClick : () => {Shiny.setInputValue('market_select', item) } }, item)
      )")
      ),
      div(
        textInput('market_select', label = NULL, value = markets[1]),
        style = 'display:none'  
      ), id='choice_markets', style='display:none'
      )
    )
  })
  output$toggler <- renderUI({
    if (is_page('/')) {
      shinyjs::runjs(HTML("
      $('#choice_markets').css('display', 'none');
      $('#choice_models').css('display', 'block');
                          "))
      # Disabling download button
      # shinyjs::runjs("$('#download').css('display', 'none')") 
      # Highlighting the page in the menu
      script <- ''
      for (i in c('cons', 'prod', 'market')) {
        script <- paste(script, 
                        paste("$('[href=", '"', "#!/", i, '"', "]').addClass('unchosen_page'); ", sep = ""), sep="")
      }
      shinyjs::runjs(script) 
      shinyjs::runjs(paste("$('[href=", '"', "#!/", '"', "]').removeClass('unchosen_page').addClass('chosen_page')", sep = ""))
    } else {
      # Enabling download button
      # shinyjs::runjs("$('#download').css('display', 'block')") 
      # Highlighting pages in the menu
      if (is_page('cons')) {
        shinyjs::runjs(HTML("
      $('#choice_markets').css('display', 'none');
      $('#choice_models').css('display', 'block');
                          "))
        script <- ''
        for (i in c('', 'prod', 'market')) {
          script <- paste(script, 
                      paste("$('[href=", '"', "#!/", i, '"', "]').removeClass('unchosen_page').addClass('unchosen_page'); ", sep = ""), sep="")
        }
        shinyjs::runjs(script) 
        shinyjs::runjs(paste("$('[href=", '"', "#!/cons", '"', "]').addClass('chosen_page')", sep = "")) 
        # shinyjs::runjs("$('#download').click(function(){ window.location.href = $('#download_yearly_shadow').attr('href');});") 

      }
      if (is_page('prod')) {
        shinyjs::runjs(HTML("
      $('#choice_markets').css('display', 'block');
      $('#choice_models').css('display', 'none');
                          "))
        script <- ''
        for (i in c('', 'cons', 'market')) {
          script <- paste(script, 
                          paste("$('[href=", '"', "#!/", i, '"', "]').removeClass('unchosen_page').addClass('unchosen_page'); ", sep = ""), sep="")
        }
        shinyjs::runjs(script) 
        shinyjs::runjs(paste("$('[href=", '"', "#!/prod", '"', "]').addClass('chosen_page')", sep = "")) 
        # shinyjs::runjs("$('#download').click(function(){ window.location.href = $('#download_yearly').attr('href');});") 
        
      }
      if (is_page('market')) {
        shinyjs::runjs(HTML("
      $('#choice_models').css('display', 'none');
      $('#choice_markets').css('display', 'block');
                          "))
        script <- ''
        for (i in c('', 'prod', 'cons')) {
          script <- paste(script, 
                          paste("$('[href=", '"', "#!/", i, '"', "]').removeClass('unchosen_page').addClass('unchosen_page'); ", sep = ""), sep="")
        }
        shinyjs::runjs(script) 
        shinyjs::runjs(paste("$('[href=", '"', "#!/market", '"', "]').addClass('chosen_page')", sep = "")) 
        # shinyjs::runjs("$('#download').click(function(){ window.location.href = $('#download_str').attr('href');});") 
      }
    }
  })

  # Load the model for a product
  model <- reactive({
    req(input$prod_select)
    model_name = input$prod_select
    model <- loadR(model_name)
    return(model)
  })
  market <- reactive({
    req(input$market_select)
    market_name = input$market_select
    print(market_name)
    market <- loadR(market_name, where='data_output/supply_analysis/')
    market
    return(market)
  })
  observeEvent(input$market_select, {
    innerhtml <- paste("if ($('.chosen_market').length == 0) {
        $('[label=", '"', req(market()$category), '"', "]').addClass('chosen_market')
      }", sep = "")
    shinyjs::runjs(HTML(paste('$(".chosen_market").removeClass("chosen_market");
                              window.highlight = function() {', innerhtml, '}', sep='')))
  })
  observeEvent(input$prod_select, {
    innerhtml <- paste("if ($('.chosen_model').length == 0) {
        $('[label=", '"', req(model()$model_name), '"', "]').addClass('chosen_model')
      }", sep = "")
    shinyjs::runjs(HTML(paste('$(".chosen_model").removeClass("chosen_model");
                              window.highlight = function() {', innerhtml, '}', sep='')))
  })
  # Ensure the chosen model is constantly highlighted
  shinyjs::runjs('setInterval( function(){ highlight(); highlight2(); tips()}, 1)') 
  # Create information panels
  isPanelOpen <- reactiveVal(FALSE)
  output$reactPanel <- renderReact({
    Panel(
      headerText = "Справка",
      isOpen = isPanelOpen(),
      model()$model_name,
      onDismiss = JS("function() { Shiny.setInputValue('hidePanel', Math.random()); }")
    )
  })
  observeEvent(input$showPanel, isPanelOpen(TRUE))
  observeEvent(input$hidePanel, isPanelOpen(FALSE))
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
      plotdecomp(model(), yoy = 0)
  })
  # Plot price structure graph when on respective page if data exists
  output$price_structure <- renderPlotly({
      validate(need(
        !is.null(model()$price_decomp),
        "Данные по структуре розничной цены данного товара недоступны"
      ))
      try(plot_price_decomp(model()))
  })
  # Plot supply structure on respective page
  output$supply_analysis <- renderPlotly({
    stack_supply(market(), sa = input$sa_toggle)
  })
  output$supply_analysis_non_sa <- renderPlotly({
    stack_supply(market(), sa = FALSE)
  }) 
  # observe({
  #   data_map <- market()$df_FD
  #   data_map$date
  #   dates_map <- unique(data_map$date)
  #   dates_map
  #   pars <- paste("minDate = '", first(dates_map), "', maxDate = '", last(dates_map), "'", sep='')
  #   # updateAirDateInput(session, "date_map", value = last(dates_map), options=pars)
  # })
  output$map_sa <- renderPlotly({
    plot_map(market(), level='reg', sa=input$sa_toggle, date_choice=req(input$date_map))
  })
  output$supply_map <- renderPlotly({
    production_map(market(), rus_map, sa=input$sa_toggle, date_choice=req(input$date_map))
  })
  output$supply_map_non_sa <- renderPlotly({
    production_map(market(), rus_map, date_choice=req(input$date_map), sa=FALSE)
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
  output$download_yearly_shadow <- downloadHandler(
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
  output$download_mom_shadow <- downloadHandler(
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
  output$download_str_shadow <- downloadHandler(
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
