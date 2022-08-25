#---------------------------------------------------------------------------------------------------
# Load libraries, functions & dependencies
#---------------------------------------------------------------------------------------------------
# Sys.setlocale(category = "LC_ALL", locale = "C")
options(encoding = "UTF-8")
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

market_models_corresp <- read_xlsx('data_output/correspondence.xlsx')

# rus_map <- readRDS('data_output/federal_districts.rds')
date_last <- as.Date('2022-02-01')
date_last_trade <- as.Date('2022-01-01')
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

info_card <- function(text, toggle_id = NULL, toggleOn = NULL, toggleOff = NULL, download_id = NULL, panel = FALSE, display_download=TRUE,
                      dropdown = NULL) {
  d <- c()
  if (!is.null(toggle_id)) {
    for (i in 1:length(toggle_id)) {
      d[[i]] <- tags$div(Toggle.shinyInput(toggle_id[i], value = TRUE,
                                           label = "",
                                           onText = toggleOn[i],
                                           offText = toggleOff[i] 
      ))
    }
  }
  if (!is.null(dropdown)) {
    if (is.null(toggle_id)) {
      k <- 1
    } else {
      k <- length(toggle_id) + 1
    }
    options <- list(
      list(key = "m", text = "Месячные данные"),
      list(key = "q", text = "Квартальные данные"),
      list(key = "y", text = "Годовые данные")
    )
    d[[k]] <- Dropdown.shinyInput(dropdown, value = "m", options = options)
  }

  tags$div(
    tags$div(text,
             style="flex-basis: 70%; display: flex;align-items: center;"),
    tags$div(
      d,
      tags$div(
        if (!panel) {
          if (display_download) {
            CommandBar(
              items = list(
                CommandBarItem("Скачать", "Download", id=download_id, onClick=JS(paste0("function() { window.location.href = $('#", download_id, '_shadow', "').attr('href'); }")))
              ))            
          }
        } else {
          if (display_download) {
            CommandBar(
              items = list(
                CommandBarItem("Скачать", "Download", id=download_id, onClick=JS(paste0("function() { window.location.href = $('#", download_id, '_shadow', "').attr('href'); }"))),
                CommandBarItem("Пояснения к факторам", "Info", id="info_panel", onClick=JS("function() { Shiny.setInputValue('showPanel', Math.random()); }"))
              ))  
          } else {
            CommandBar(
              items = list(
                CommandBarItem("Пояснения к факторам", "Info", id="info_panel", onClick=JS("function() { Shiny.setInputValue('showPanel', Math.random()); }"))
              ))              
          }

        }
      ),
      style='display:flex; flex-wrap:nowrap; flex-direction:column;justify-content: space-around; align-items:center;'),
    style="display:flex; flex-wrap:nowrap;justify-content: space-between; margin-top:15px;", class="text-card ms-depth-8")
}

calendar_temp <- function(id, date_first, date_last) {
  
  Calendar.shinyInput(id, showGoToToday=FALSE, 
                      isDayPickerVisible = FALSE,
                      isMonthPickerVisible = TRUE,
                      highlightSelectedMonth = TRUE,
                      autoNavigateOnSelection = FALSE,
                      strings = JS("
                                                {
                                                  months: [
                                                    'Январь',
                                                    'Февраль',
                                                    'Март',
                                                    'Апрель',
                                                    'Май',
                                                    'Июнь',
                                                    'Июль',
                                                    'Август',
                                                    'Сентбярь',
                                                    'Октябрь',
                                                    'Ноябрь',
                                                    'Декабрь',
                                                  ],
                                                  shortMonths: ['Янв', 'Фев', 'Мар', 'Апр', 'Май', 'Июнь', 'Июль', 'Авг', 'Сен', 'Окт', 'Ноя', 'Дек'],
                                                  prevYearAriaLabel: 'Предыдущий год,',
                                                  nextYearAriaLabel: 'Следующий год,',	
                                                  nextYearRangeAriaLabel:	'Следующее десятилетие,',
                                                  prevYearRangeAriaLabel: 'Предыдущее десятилетие, ',	
                                                }
                                                "),
                      # formatDate=JS("(date) => {return !date ? '' : (date.getMonth() + 1) + '/' + (date.getFullYear()) }"),
                      maxDate=JS(paste("new Date('", year(date_last), "-", ifelse(nchar(month(date_last))==1, paste0(0, month(date_last)), month(date_last)),  "-01')", sep='')),
                      minDate=JS(paste0("new Date('", as.character(date_first), "')")),
                      value=JS(paste("new Date('", year(date_last), "-", ifelse(nchar(month(date_last))==1, paste0(0, month(date_last)), month(date_last)),  "-01')", sep='')))
}


#---------------------------------------------------------------------------------------------------
# Define layout elements and pages
#---------------------------------------------------------------------------------------------------
# Header bar with logo and buttons
header <- tagList(
  div(class = 'logo-block',
      Link(href = '/', img(src = "hse-logo.png", class = "logo")),
      div(Link(href = '/', Text(variant = "xLarge", "Анализ потребительских рынков"), class = 'title-link'), class = "title")
  )
)
# Sidebar navigation & product choice
navigation <- tagList (
  Nav(
    groups = list(list(links = list(
      # list(
      #   name = 'Общие сведения',
      #   url = '#!/',
      #   key = 'home',
      #   icon = 'Home'
      # ),
      list(
        name = 'Потребительские цены',
        url = '#!/',
        key = 'cons',
        icon = 'LineChart'
      ),
      list(
        name = 'Цены производителей, импорта и экспорта',
        url = '#!/prod',
        key = 'prod',
        icon = 'StackedLineChart'
      ),
      list(
        name = 'Анализ рыночной конъюнктуры',
        url = '#!/market ',
        key = 'market',
        icon = 'PieDouble'
      ),
      list(
        name = 'Анализ нелегального оборота',
        url = '#!/counterfeit',
        key = 'counterfeit',
        icon = 'BlockedSite'
      ),
      list(
        name = 'Анализ затрат и выгод ОЦМ',
        url = '#!/balance',
        key = 'balance',
        icon = 'Compare'
      )
    ))),
    initialSelectedKey = 'cons',
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
cards_cpi <- htmlOutput('card_set_CPI')
cards_ppi <- htmlOutput('card_set_PPI')
cards_market <- htmlOutput('card_set_market')

# Home page
home_page <- makePage(title = 'Факторный анализ потребительских цен',
                      subtitle = "Общие сведения",
                      contents = "<Description of the dashboard's main features>")
# construct tabs for Consumer Prices page
pivot_consumer <- Pivot(
  PivotItem(headerText = "Изменение цен - г/г", 
            info_card(text = tags$span("На графике жирной линией обозначено, как изменилась цена выбранного товара по сравнению с аналогичным периодом предыдущего года. Это изменение раскладывается на влияние отдельных факторов, релевантных для данного рынка. Под инфляцией спроса понимается изменение динамики спроса на товар, а также влияние общеинфляционных тенденций в экономике на изменение его цены.",
                                       tags$br(),
                                       "Затемненная область соответствует прогнозным значениям, полученным на основе построенной статистической модели. Для прогнозов также приводятся доверительные интервалы, обозначающие диапазон, в котором наиболее вероятно будет находиться значение инфляции для выбранного продукта.",
                                       tags$br(),
                                       'Подробное описание используемых в анализе факторов доступно в разделе "Пояснения к факторам"'),
                      download_id = "download_yearly",
                      panel = TRUE),
            tags$div(
              tags$div(plotlyOutput('price_yoy'), style='flex-basis: 70%;'),
              tags$div(tableOutput('impacts')),
              style="display:flex; flex-wrap:nowrap;justify-content: space-around;"
            )
  ),
  PivotItem(headerText = "Изменение цен - к предыдущему периоду", 
            info_card(text = tags$span("На графике жирной линией обозначено, как изменилась цена выбранного товара по сравнению с предыдущим месяцем/неделей. Это изменение раскладывается на влияние отдельных факторов, релевантных для данного рынка. Под инфляцией спроса понимается изменение динамики спроса на товар, а также влияние общеинфляционных тенденций в экономике на изменение его цены.",
                                       tags$br(),
                                       "Затемненная область соответствует прогнозным значениям, полученным на основе построенной статистической модели. Для прогнозов также приводятся доверительные интервалы, обозначающие диапазон, в котором наиболее вероятно будет находиться значение инфляции для выбранного продукта.",
                                       tags$br(),
                                       'Подробное описание используемых в анализе факторов доступно в разделе "Пояснения к факторам"'),
                      download_id = "download_monthly",
                      panel = TRUE),
            plotlyOutput('price_mom')
  ),
  PivotItem(headerText = "Структура розничной цены", 
            info_card(text = tags$span("На графике представлена динамика средней цены выбранного товара в разбивке по основным статьям структуры розничной цены (согласно ежегодным данным Росстата)."),
                      download_id = "download_struct",
                      pane = TRUE),
            plotlyOutput('price_structure')
  )
)
# Consumer prices page
cons_page <- makePage(
  title = "Динамика потребительских цен",
  subtitle = "По данным Росстата",
  contents = div(
    div(class = "cards", cards_cpi),
    span(downloadLink("download_yearly_shadow", "Скачать данные в .xlsx"),
         downloadLink("download_mom_shadow", "Скачать данные в .xlsx"),
         downloadLink("download_str_shadow", "Скачать данные в .xlsx"),
         style='visibility:hidden; overflow:hidden;'),
    pivot_consumer,
    style='height:1000px;'
  )
)

# Producer, import & export prices page
pivot_prodprices <- Pivot(
  PivotItem(headerText = 'Цены производителей',
            info_card(text = tags$span("На графике изображены средние отпускные цены производителей промышленной продукции для товаров, входящих в рассматриваемую товарную группу."),
                      # toggle_id = "sa_toggle",
                      # toggleOn = "Сезонное сглаживание включено",
                      # toggleOff = "Сезонное сглаживание отключено",
                      display_download=FALSE),
            plotlyOutput('prod_prices')),
  PivotItem(headerText = 'Внешнеторговые цены',
            info_card(text = tags$span("На графике изображены средние цены импорта и экспорта товаров, входящих в рассматриваемые товарные группы. Они получены как частное общей стоимости проданной продукции и ее количества в натуральном выражении.",
                                       tags$br(), "По умолчанию результат конвертируется в рубли по среднемесячному валютному курсу. Для отображения исходных данных в долларах США необходимо воспользоваться переключателем справа. "),
                      toggle_id = "usd_toggle",
                      toggleOn = "Конвертация цен в рубли включена",
                      toggleOff = "Конвертация цен в рубли отключена",
                      display_download=FALSE),
            plotlyOutput('trade_prices'))
)
prod_page <- makePage(
  title = "Динамика цен производителей, импорта и экспорта",
  subtitle = "По данным Росстата и ФТС",
  contents = div(
    div(class = "cards", cards_ppi),
    pivot_prodprices,
    style='height:600px;'
  )
)

# Market analysis page
pivot_market <- Pivot(overflowBehavior="menu",
  PivotItem(headerText = "Динамика совокупного предложения", 
            info_card(text = tags$span("На графике представлена динамика расчетного объема совокупного предложения на рынке. Предложение складывается из внутреннего производства, изменения запасов и сальдо чистого экспорта.",
                                       tags$br(), "Все расчёты производится в натуральном выражении. По умолчанию используются сезонно-сглаженные данные. Для отображения исходных значений необходимо воспользоваться переключателем справа."),
                      toggle_id = "sa_toggle_m",
                      toggleOn = "Сезонное сглаживание включено",
                      toggleOff = "Сезонное сглаживание отключено",
                      display_download=FALSE,
                      dropdown = 'type_supply'),
            plotlyOutput('supply_analysis')
  ),
  PivotItem(headerText = "Географическая структура производства",
            info_card(text = tags$span("Карта отражает географическое распределение внутреннего производства товара в России. Выбор анализируемого периода производится с помощью календаря справа от карты. По умолчанию данные представлены на уровне федеральных округов. 
                       Для отображения данных на уровне отдельных субъектов РФ необходимо воспользоваться переключателем", tags$sup("*"),
                                       tags$br(), tags$sub("*Региональные данные не всегда полны ввиду необходимости обеспечения конфиденциальности первичной статистической информации.")),
                      toggle_id = "map_volume_toggle",
                      toggleOn = "Данные по федеральным округам",
                      toggleOff = "Данные по субъектам федерации",
                      display_download=FALSE,
                      dropdown = 'type_volume_map'),
            tags$table(width="100%",
                       tags$tr(
                         tags$td(plotlyOutput('map_volume'), width="80%"),
                         tags$td(
                           htmlOutput('chosen_period_volume'),
                           calendar_temp('date_map_volume', date_first=as.Date('2011-01-01'), date_last=date_last),
                           width="20%", valign='bottom', align='center', style='padding-bottom:10%;')
                       ))
  ),
  PivotItem(headerText = "Региональная динамика производства",
            info_card(text = tags$span("Карта отражает динамику внутреннего производства товара в России. Выбор анализируемого периода производится с помощью календаря справа от карты. По умолчанию данные представлены на уровне федеральных округов. 
                       Для отображения данных на уровне отдельных субъектов РФ необходимо воспользоваться переключателем справа", tags$sup("*"), ".",
                                       tags$br(),
                                       tags$sub("*Региональные данные не всегда полны ввиду необходимости обеспечения конфиденциальности первичной статистической информации.")),
                      toggle_id = "map_dynam_toggle",
                      toggleOn = "Данные по федеральным округам",
                      toggleOff = "Данные по субъектам федерации",
                      display_download=FALSE,
                      dropdown = 'type_dyn_map'),
            tags$table(width="100%",
                       tags$tr(
                         tags$td(plotlyOutput('map_dynam'), width="80%"),
                         tags$td(
                           htmlOutput('chosen_period_dynam'),
                           calendar_temp('date_map_dynam', date_first=as.Date('2012-01-01'), date_last=date_last),
                           width="20%", valign='bottom', align='center', style='padding-bottom:10%;')
                       ))
  ),
  PivotItem(headerText = "Внешняя торговля",
            info_card(text = tags$span("На графиках представлены показатели, характеризующие состояние внешней торговли рассматриваемым товаром.",
                                       tags$br(),
                                       "Для выбранного в календаре месяца отображаются три крупнейших направления экспорта и импорта.",
                                       tags$br(),
                                       "Показатель географической концентрации рассчитан как индекс Херфиндаля-Хиршмана на основе долей отдельных торговых партнеров в экспорте и импорте.
                       Чем ближе значение индекса к 1, тем более концентрированным является российский экспорт/импорт товара.",
                                       tags$br(),
                                       "Индексы концентрации и доли в производстве расчитаны на сглаженных данных для исключения сезонного фактора."),
                      toggle_id = NULL,
                      display_download=FALSE),
            tags$div(
              tags$div(plotlyOutput('plot_trade'), style='flex-basis: 70%;'),
              tags$div(calendar_temp('date_trade_country', date_first=as.Date('2014-02-01'), date_last=date_last_trade)),
              style="display:flex; flex-wrap:nowrap;justify-content: space-around;"
            )
  ),
  PivotItem(headerText = 'Розничные продажи',
            info_card(text = tags$span("На графике изображен совокупный объем розничных продаж в выбранной товарной группе."),
                      toggle_id = "retail_sa",
                      toggleOn = "Сезонное сглаживание включено",
                      toggleOff = "Сезонное сглаживание отключено",
                      display_download=FALSE),
            htmlOutput('retail_categ'),
            plotlyOutput('retail')
            ),
  PivotItem(headerText = 'Уровень наценки',
            info_card(text = tags$span("На графике изображены средние уровни фактически сложившейся торговой наценки в сфере оптовой и розничной торговли товарами выбранной группы."),
                      toggle_id = NULL,
                      display_download=FALSE),
            htmlOutput('margin_categ'),
            plotlyOutput('margin')
  )
)


market_page <- makePage(
  title = "Анализ рыночной конъюнктуры",
  subtitle = "По данным Росстата и ФТС",
  contents = div(
    div(class = "cards", cards_market),
    pivot_market,
    style='height:1300px;')
)
# counterfeit analysis
cntf_page <- makePage(
  title = "Анализ нелегального оборота",
  subtitle = "Расчеты НИУ ВШЭ",
  contents = div(
    info_card(text = tags$span("На информационной панели представлены результаты оценки доли нелегального оборота продукции на квартальном уровне. Расчет производится с помощью
                                балансового метода, предполагающего сопоставление легального предложения и объема розничных продаж."),
              toggle_id = NULL,
              download_id = NULL,
              display_download=FALSE),
    plotlyOutput('counterfeit_plot'),
    style='height:1300px;'
  )
)
# OCM balance analysis
balance_page <- makePage(
  title = "Анализ затрат и выгод ОЦМ",
  subtitle = "Расчеты НИУ ВШЭ",
  contents = div(
    info_card(text = tags$span(HTML('На графике представлен <b>баланс фактических затрат и выгод</b>, реализовавшихся в отрасли после введения
                                    обязательной цифровой маркировки. Расчет производится на основе данных статистики Росстата. Для каждого показателя из точки 
                                    "до введения ОЦМ" строится трендовый прогноз развития отрасли для сценария, в котором маркировка не вводится.
                                    Эффект от введения ОЦМ определяется как отклонение фактической отраслевой динамики от прогнозных значений.')),
              toggle_id = NULL,
              download_id = NULL,
              display_download=FALSE),
    tags$div(
      tags$div(
        plotlyOutput('balance_plot'),
               style="flex-basis: 75%; display: flex; align-items: center;"
        ),
      tags$div(),
    style="display:flex; flex-wrap:nowrap; flex-direction:row;"),
    style='height:1300px;'
  )
)
#---------------------------------------------------------------------------------------------------
# Define unique URLs for pages & setup the UI
#---------------------------------------------------------------------------------------------------
router <- make_router(
  # route("/", home_page),
  route("/", cons_page),
  route('market', market_page),
  route("prod", prod_page),
  route("counterfeit", cntf_page),
  route("balance", balance_page)
  
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
  date_to_lab <- function(date) {
    paste(day(as.Date(date)), ' ', labelset[month(as.Date(date))], '-', substr(year(as.Date(date)), 3, 4), sep =
            '')
  }
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
      # Highlighting the page in the menu
      script <- ''
      for (i in c('prod', 'market', 'counterfeit', 'balance')) {
        script <- paste(script, 
                        paste("$('[href=", '"', "#!/", i, '"', "]').addClass('unchosen_page'); ", sep = ""), sep="")
      }
      shinyjs::runjs(script) 
      shinyjs::runjs(paste("$('[href=", '"', "#!/", '"', "]').removeClass('unchosen_page').addClass('chosen_page')", sep = ""))
    } else {
      # Highlighting pages in the menu
      # if (is_page('cons')) {
      #   shinyjs::runjs(HTML("
      # $('#choice_markets').css('display', 'none');
      # $('#choice_models').css('display', 'block');
      #                     "))
      #   script <- ''
      #   for (i in c('', 'prod', 'market')) {
      #     script <- paste(script, 
      #                     paste("$('[href=", '"', "#!/", i, '"', "]').removeClass('unchosen_page').addClass('unchosen_page'); ", sep = ""), sep="")
      #   }
      #   shinyjs::runjs(script) 
      #   shinyjs::runjs(paste("$('[href=", '"', "#!/cons", '"', "]').addClass('chosen_page')", sep = "")) 
      # }
      if (is_page('prod')) {
        shinyjs::runjs(HTML("
      $('#choice_markets').css('display', 'block');
      $('#choice_models').css('display', 'none');
                          "))
        script <- ''
        for (i in c('', 'market', 'counterfeit', 'balance')) {
          script <- paste(script, 
                          paste("$('[href=", '"', "#!/", i, '"', "]').removeClass('unchosen_page').addClass('unchosen_page'); ", sep = ""), sep="")
        }
        shinyjs::runjs(script) 
        shinyjs::runjs(paste("$('[href=", '"', "#!/prod", '"', "]').addClass('chosen_page')", sep = "")) 
        
      }
      if (is_page('market')) {
        shinyjs::runjs(HTML("
      $('#choice_models').css('display', 'none');
      $('#choice_markets').css('display', 'block');
                          "))
        script <- ''
        for (i in c('', 'prod', 'counterfeit', 'balance')) {
          script <- paste(script, 
                          paste("$('[href=", '"', "#!/", i, '"', "]').removeClass('unchosen_page').addClass('unchosen_page'); ", sep = ""), sep="")
        }
        shinyjs::runjs(script) 
        shinyjs::runjs(paste("$('[href=", '"', "#!/market", '"', "]').addClass('chosen_page')", sep = "")) 
      }
      if (is_page('counterfeit')) {
        shinyjs::runjs(HTML("
      $('#choice_models').css('display', 'none');
      $('#choice_markets').css('display', 'block');
                          "))
        script <- ''
        for (i in c('', 'prod', 'market', 'balance')) {
          script <- paste(script, 
                          paste("$('[href=", '"', "#!/", i, '"', "]').removeClass('unchosen_page').addClass('unchosen_page'); ", sep = ""), sep="")
        }
        shinyjs::runjs(script) 
        shinyjs::runjs(paste("$('[href=", '"', "#!/market", '"', "]').addClass('chosen_page')", sep = "")) 
      }
      if (is_page('balance')) {
        shinyjs::runjs(HTML("
      $('#choice_models').css('display', 'none');
      $('#choice_markets').css('display', 'block');
                          "))
        script <- ''
        for (i in c('', 'prod', 'market', 'counterfeit')) {
          script <- paste(script, 
                          paste("$('[href=", '"', "#!/", i, '"', "]').removeClass('unchosen_page').addClass('unchosen_page'); ", sep = ""), sep="")
        }
        shinyjs::runjs(script) 
        shinyjs::runjs(paste("$('[href=", '"', "#!/market", '"', "]').addClass('chosen_page')", sep = "")) 
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
  shinyjs::runjs(paste0('setInterval( function(){ highlight(); highlight2(); tips();}, 1);'))
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
  output$card_set_CPI <- renderUI({
    # Load model data
    estimated <- model()
    weight = estimated$weight
    if ("marking" %in% names(estimated$decomp2)) {
      markingImpact <- round(estimated$cump_impact['Маркировка'],2)
    } else {
      markingImpact <- NA
    }
    
    
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
        if (is.na(markingImpact)) {
          makeCard(
            title = Text('ОЦМ не ведется', variant = 'mediumPlus'),
            size = 10,
            card_type = 'neutral',
            icon = 'fa-shopping-basket'
          )
        } else {
          makeCard(
            title = paste(markingImpact, ' ', rub, sep = ''),
            content2 = Text('Вклад ОЦМ в цену', variant = 'mediumPlus'),
            size = 10,
            card_type = 'neutral',
            icon = 'fa-shopping-basket'
          )
        }
      )
    )
  })
  # value boxes for producer prices
  output$card_set_PPI <- renderUI({
    # Load model data
    data <- market()
    model_name <- data$category
    ed <- last(unique(data$prod_price$OKEI))
    ed <- case_when(
      ed=='Тысяча штук' ~ 'тыс. шт.',
      ed=='литр' ~ 'л.'
    )
    rub <- '\U20BD'
    price = round(last(data$prod_price$value), 2)
    # Get date of the latest data point for PPI and create a label
    label_current <- date_to_lab(last(data$prod_price$date))
    prod_price_names <- data$prod_price_names
    # trade prices
    label_current_trade <-date_to_lab(last(data$trade_price$date))
    price_ex <- round(last(data$trade_price[data$trade_price$NAPR=='ЭК',]$mean_price_rub))
    price_im <- round(last(data$trade_price[data$trade_price$NAPR=='ИМ',]$mean_price_rub))
    
    # producer inflation
    label_prev <- date_to_lab(nth(data$prod_price$date, -2))
    label_yoy <- date_to_lab(nth(data$prod_price$date, -13))
    ppi_prev <- round((last(data$prod_price$value) / nth(data$prod_price$value, -2) - 1) * 100, 2)
    ppi_yoy <- round((last(data$prod_price$value) / nth(data$prod_price$value, -13) -1 ) * 100, 2)
    
    # Stack 3 cards together
    Stack(
      div(model_name, style = 'margin-bottom:10px;', class = 'ms-fontSize-20 ms-fontWeight-regular'),
      Stack(
        horizontal = TRUE,
        horizontalAlign = 'space-between',
        tokens = list(childrenGap = 10),
        # current producer price card
        makeCard(
          title = paste(
            price,
            ' ',
            rub,
            '/',
            ed,
            sep = ''
          ),
          content2 = span(Text(paste('Цены производителей, ', label_current, sep = ''), variant = 'mediumPlus'),
                          tags$sup(TooltipHost(
                            content = span(paste0('До 2017 года по ОКПД: "', prod_price_names[[1]], '"'), tags$br(), paste0('После 2017 года по ОКПД2: "', prod_price_names[[2]], '"')),
                            delay = 0,
                            Text(FontIcon(iconName = 'info'))
                          ))),
          size = 10,
          card_type = 'neutral',
          icon = 'fa-coins'
        ),
        # y-o-y card
        makeCard(
          title = paste(
            ifelse(ppi_yoy > 0, '+', ''),
            ppi_yoy,
            '%',
            sep = ''
          ),
          tooltip = paste(label_current, '/', label_yoy),
          content = Text('Инфляция цен производителей, г/г:', variant = 'mediumPlus'),
          content2 = Text('Инфляция цен производителей, м/м:', variant = 'mediumPlus'),
          title2 = paste(
            ifelse(ppi_prev > 0, '+', ''),
            ppi_prev,
            '%',
            sep = ''
          ),
          tooltip2 = paste(label_current, ' / ', label_prev, sep =
                             ''),
          size = 10,
          card_type = ifelse(ppi_yoy > 0, "bad", "good"),
          icon = ifelse(ppi_yoy > 0, "fa-arrow-up", "fa-arrow-down")
        ),
        # export-import price
        makeCard(
          content = Text(paste0('Цены экспортеров, ',label_current_trade,':'), variant = 'mediumPlus'),
          content2 = Text(paste0('Цены импортеров, ',label_current_trade,':'), variant = 'mediumPlus'),
          title = paste(
            price_ex,
            ' ',
            rub,
            '/',
            ed,
            sep = ''
          ),
          title2 = paste(
            price_im,
            ' ',
            rub,
            '/',
            ed,
            sep = ''
          ),
          size = 10,
          card_type = 'neutral',
          icon = 'fa-coins'
        )
      )
    )
  })
  # value boxes for market analysis
  output$card_set_market <- renderUI({
    # Load model data
    data <- market()
    model_name <- data$category
    ed <- data$ed
    rub <- '\U20BD'
    
    # supply volumes
    supply_month <- format(round(last(data$df_sa$total)), big.mark = " ")
    
    
    # Get date of the latest supply point
    label_current <- date_to_lab(last(data$df_sa$date))
    prod_price_names <- data$prod_price_names
    
    # internal production
    production_dyn_prev <- round(
      (last(data$df_sa[data$df_sa$type=='Внутреннее производство',]$value) / nth(data$df_sa[data$df_sa$type=='Внутреннее производство',]$value, -2) - 1) * 100, 2
      )
    production_dyn_yoy <- round((last(data$df_sa[data$df_sa$type=='Внутреннее производство',]$value) / nth(data$df_sa[data$df_sa$type=='Внутреннее производство',]$value, -13) - 1) * 100, 2)
    label_prev <- date_to_lab(nth(data$df_sa[data$df_sa$type=='Внутреннее производство',]$date,-2))
    label_yoy <- date_to_lab(nth(data$df_sa[data$df_sa$type=='Внутреннее производство',]$date, -13))
    
    # retail sales
    if (!is.null(data$df_retail_sa)) {
      no_retail <- FALSE
      retail_q <- format(round(last(data$df_retail_sa$value)), big.mark = " ")
      retail_q_yoy <- format(round((last(data$df_retail_sa$value) / nth(data$df_retail_sa$value, -5) - 1)*100, 2), big.mark = " ")
      
      label_retail <- date_to_lab(last(data$df_retail_sa$date))
      label_retail_yoy <- date_to_lab(nth(data$df_retail_sa$date, -5))
    } else {
      no_retail <- TRUE
      # retail_q <- NA
      # retail_q_yoy <- NA
      # label_retail <- NA
      # label_retail_yoy <- NA
    }
    if (!no_retail) {
      retail_card <- makeCard(
        content = Text(paste0('Объем розничных продаж, ', label_retail, ':'), variant = 'mediumPlus'),
        content2 = Text(paste0('Темп роста розничных продаж, г/г',':'), variant = 'mediumPlus'),
        title = paste(
          retail_q,
          ' ',
          rub,
          sep = ''
        ),
        title2 = paste0(retail_q_yoy, '%'),
        size = 10,
        card_type = ifelse(retail_q_yoy < 0, "bad", "good"),
        icon = ifelse(retail_q_yoy > 0, "fa-arrow-up", "fa-arrow-down")
      )
    } else {
      retail_card <- makeCard(
        content = Text(paste0('Объем розничных продаж,', ':'), variant = 'mediumPlus'),
        title = '',
        content2 = 'Нет данных',
        size = 10,
        card_type = 'neutral',
        icon = "fa-empty-set"
      )
    }
    
    
    
    
    # Stack 3 cards together
    Stack(
      div(model_name, style = 'margin-bottom:10px;', class = 'ms-fontSize-20 ms-fontWeight-regular'),
      Stack(
        horizontal = TRUE,
        horizontalAlign = 'space-between',
        tokens = list(childrenGap = 10),
        # current producer price card
        makeCard(
          title = paste(
            supply_month,
            ed,
            sep = ' '
          ),
          content2 = span(Text(paste('Совокупное предложение, ', label_current, sep = ''), variant = 'mediumPlus'),
                          tags$sup(TooltipHost(
                            content = span('До 2017 года по ОКПД: ', paste(data$OKPD, collapse='; '), tags$br(), 'После 2017 года по ОКПД2: ', paste(data$OKPD2, collapse='; ')),
                            delay = 0,
                            Text(FontIcon(iconName = 'info'))
                          ))),
          size = 10,
          card_type = 'neutral',
          icon = 'fa-coins'
        ),
        # y-o-y card
        makeCard(
          title = paste(
            ifelse(production_dyn_yoy > 0, '+', ''),
            production_dyn_yoy,
            '%',
            sep = ''
          ),
          tooltip = paste(label_current, '/', label_yoy),
          content = Text('Динамика внутреннего производства, г/г:', variant = 'mediumPlus'),
          content2 = Text('Динамика внутреннего производства, м/м:', variant = 'mediumPlus'),
          title2 = paste(
            ifelse(production_dyn_prev > 0, '+', ''),
            production_dyn_prev,
            '%',
            sep = ''
          ),
          tooltip2 = paste(label_current, ' / ', label_prev, sep =
                             ''),
          size = 10,
          card_type = ifelse(production_dyn_yoy < 0, "bad", "good"),
          icon = ifelse(production_dyn_yoy > 0, "fa-arrow-up", "fa-arrow-down")
        ),
        retail_card
        # makeCard(
        #   content = Text(paste0('Объем розничных продаж, ', ifelse(!no_retail, label_retail, NULL), ':'), variant = 'mediumPlus'),
        #   content2 = ifelse(!no_retail, Text(paste0('Темп роста розничных продаж, г/г',':'), variant = 'mediumPlus'), NULL),
        #   title = ifelse(!no_retail, paste(
        #     retail_q,
        #     ' ',
        #     rub,
        #     sep = ''
        #   ), 'нет данных'),
        #   title2 = ifelse(!no_retail, paste0(retail_q_yoy, '%'), NULL),
        #   size = 10,
        #   card_type = ifelse(!no_retail, ifelse(retail_q_yoy < 0, "bad", "good"), 'neutral'),
        #   icon = ifelse(!no_retail, ifelse(retail_q_yoy > 0, "fa-arrow-up", "fa-arrow-down"), "fa-empty-set")
        # )
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
  output$counterfeit_plot <- renderPlotly({
    validate(need(
      !is.na(market()$counterfeit),
      "Данные по доле нелегального розничного оборота недоступны"
    ))
    try(counterfeit_plot(market(), opacity=0))
  })
  output$balance_plot <- renderPlotly({
    validate(need(
      !is.na(market()$counterfeit),
      "Данные по балансу затрат и выгод ОЦМ недоступны"
    ))
    try(balance_plot(market()))
  })
  # Plot supply structure on respective page
  output$supply_analysis <- renderPlotly({
    stack_supply(market(), sa = input$sa_toggle_m, type=input$type_supply)
  })
  output$map_volume <- renderPlotly({
    plot_map(data=market(), level_fed=input$map_volume_toggle, date_choice=input$date_map_volume, dynam = FALSE, type=input$type_volume_map)
  })
  output$map_dynam <- renderPlotly({
    plot_map(data=market(), level_fed=input$map_dynam_toggle, date_choice=input$date_map_dynam, dynam = TRUE, type=input$type_dyn_map)
  })

  output$plot_trade <- renderPlotly({
    plot_trade(data=market(), date_choice=input$date_trade_country)
  })
  output$trade_prices <- renderPlotly({
    trade_prices(market(), convert=input$usd_toggle)
  })
  output$prod_prices <- renderPlotly({
    prod_prices(market())
  })

  output$retail <- renderPlotly({
    validate(need(
      !nrow(market()$df_retail)==0,
      "Данные по розничным продажам данного товара недоступны"
    ))
    try(retail_trade(data=market(), sa=input$retail_sa))
    
  })
  output$retail_categ <- renderUI({
    if (!length(market()$retail_names[["2010"]])==0) {
      tags$div(
        tags$div("До 2016 года включительно отображены данные в категориях ОКПД:", paste(market()$retail_names[["2010"]], collapse=',')),
        tags$div("С 2017 года отображены данные в категориях ОКПД2:", paste(market()$retail_names[["2017"]], collapse=','))
      )      
    }
  })
  output$margin <- renderPlotly({
    margin_plot(data=market())
  })
  output$margin_categ <- renderUI({
    tags$div(
      tags$div("Для оптовой торговли отображены данные в категории: ", paste(market()$margin_whole, collapse=',')),
      tags$div("Для розничной торговли отображены данные в категории: ", paste(market()$margin_retail, collapse=','))
    )
  })
  output$chosen_period_volume <- renderUI({
    tags$div("Отображаются данные за: ",
             if (input$type_volume_map=='m') as.yearmon(input$date_map_volume),
             if (input$type_volume_map=='q') paste0(quarter(input$date_map_volume), 'К', year(input$date_map_volume)),
             if (input$type_volume_map=='y') paste0(year(input$date_map_volume), ' год')
    )
  })
  output$chosen_period_dynam <- renderUI({
    tags$div("Отображаются данные за: ",
             if (input$type_dyn_map=='m') as.yearmon(input$date_map_dynam),
             if (input$type_dyn_map=='q') paste0(quarter(input$date_map_dynam), 'К', year(input$date_map_dynam)),
             if (input$type_dyn_map=='y') paste0(year(input$date_map_dynam), ' год')
    )
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
    # names(final) <- c('Фактор', 'Объясняющаяя способность', 'Накопленный вклад в цену')
    final <- final[,c(1,3)]
    names(final) <- c('Фактор', 'Накопленный вклад в цену')
    final[,2] <- paste(final[,2], ' руб.', sep='')
    # final[,2] <- paste(final[,2], '%', sep='')
    # final[,3] <- paste(final[,3], ' руб.', sep='')
    final
  }, align='lc')
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
