options(encoding = "UTF-8")
# using() install missing libraries and load all the rest --------------------------------------
using <- function(...) {
  libs <- unlist(list(...))
  req <- unlist(lapply(libs,require,character.only=TRUE))
  need <- libs[req==FALSE]
  if (length(need)>0) {
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}

using('data.table', 'readxl', 'dplyr', 'xts', 'plotly', 'stats', 'writexl',
      'lubridate', 'ggplot2', 'shiny', 'scales', 'flexdashboard', 'RColorBrewer', 'clock',
      'downloadthis', 'glue', 'leaflet', 'sass', 'shiny', 'shiny.fluent', 'shiny.react', 'shiny.router',
      'shinycssloaders', 'shinyjs', 'stringr', 'tmap', 'sf', 'shinyWidgets', 'geofacet', 'purrr', 'reshape2', 'tidyr', 'forcats',
      'htmltools', 'tsibble')

theme_set(theme_minimal())
options(scipen=999)

labelset <- clock_labels_lookup('ru')[[2]]
labelset <- substr(labelset, 1, nchar(labelset)-1)
labelset[5] <- 'май' 

# plotdecomp() plot decomposition --------------------------------------------------------------
plotdecomp = function(model, yoy=1) {
  model_name <- model$model_name
  varlabels <- model$varlabels
  weekly <- model$weekly
  n.ahead <- model$n.ahead
  
  if (yoy) {
    decomp2           = model$decomp2*100
    forecast2         = data.frame(model$forecast2*100)
    if (!n.ahead==0) {
      forecast2$date <- as.Date(rownames(forecast2))
      forecast2$date[1] <- forecast2$date[1] 
    }
    varlabels <- c('Инфляция спроса', varlabels, 'Прочие факторы')
    graph_material <- data.frame(decomp2)
    names(graph_material) <- c('Значение', varlabels)
    Sys.setlocale("LC_TIME", "Russian")
    if (weekly) {graph_material$date <- as.Date(rownames(graph_material))} else {
      graph_material$date <- as.Date(as.yearmon(rownames(graph_material)))
    } 
    graph_material_long <- reshape2::melt(graph_material[,-1], id.vars='date')
    graph_material <- data.frame(decomp2)
    if (weekly) {graph_material$date <- as.Date(rownames(graph_material))} else {
      graph_material$date <- as.Date(as.yearmon(rownames(graph_material)))
    }
    par(mar = c(0.5,0.5,0.5,0.5))
    trace(grDevices::png, quote({
      if (missing(type) && missing(antialias)) {
        type <- "cairo-png"
        antialias <- "subpixel"
      }
    }), print = FALSE)
    supplem <- graph_material[,-c(1,ncol(graph_material))]
    supplem[supplem<0] <- 0
    upper_lim <- max(rowSums(abs(supplem)))*1.2
    supplem <- graph_material[,-c(1,ncol(graph_material))]
    supplem[supplem>0] <- 0
    lower_lim <- min(rowSums(supplem, na.rm=TRUE))*1.2
    
    ncolors <- ncol(decomp2) - 1
    if (ncolors<=9) {
      colors <- adjustcolor(brewer.pal(n = 9, name = 'Set1'), alpha.f=0.6)
    } else {
      colors <- adjustcolor(brewer.pal(n = 12, name = 'Set3'), alpha.f=1)
    }
    if (!n.ahead==0) {
      if (weekly) {
        shade_start <- last(graph_material$date) + days(4)
      } else {
        shade_start <- last(graph_material$date) + weeks(2)
      }   
      if (weekly) {
        shade_end <- last(forecast2$date) + days(4)
      } else {
        shade_end <- last(forecast2$date) + weeks(2)
      }
      p <- ggplot() +
        annotate("rect", xmin = shade_start, xmax = shade_end, ymin = lower_lim, ymax = upper_lim, alpha = .5)
    }
    if (!n.ahead==0) {
      p <- p + 
        geom_ribbon(data = forecast2, aes(x = date, ymin = lower, ymax = upper), alpha=0.5, fill='#72bcd4') +
        geom_line(data = forecast2, aes(x = date,  y = value, text = paste('Прогноз, ', paste(day(as.Date(date)), labelset[month(as.Date(date))], year(as.Date(date)), sep=' '), ' г/г: ', round(value,2), '%<br>', 'Нижняя граница: ', round(lower, 2), '%<br>', 'Верхняя граница: ', round(upper, 2), '%', sep='')), size = ifelse(weekly, 0.7, 1), color = "blue", group = 1)
    } else {
      p <- ggplot()
    }
      p <- p + geom_line(data = graph_material, aes(x = date,  y = value, text = paste('Изменение цены, ', paste(day(as.Date(date)), labelset[month(as.Date(date))], year(as.Date(date)), sep=' '), ' г/г: ', round(value,2), '%', sep='')), size = ifelse(weekly, 0.7, 1), color = "black", group = 1) +
      geom_col(data = graph_material_long, position = "stack", show.legend = NA, 
               aes(x = date, y = value, fill = variable, width = ifelse(weekly, 7, 25), text = paste('Фактор: ', variable, '<br>Влияние, ', paste(day(as.Date(date)), labelset[month(as.Date(date))], year(as.Date(date)), sep=' '), ': ', round(value,2), '%', sep=''))) +
      scale_fill_manual(labels = labels, values=colors) +
      scale_x_date(breaks='6 months', labels = scales::label_date("%b-%y"), expand=expansion(add = 10)) +
      xlab(NULL) +
      ylab('Изменение цены, г/г') +
      scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(lower_lim, upper_lim)) +
      theme(legend.position="bottom") +
      theme(plot.margin = unit(c(0,0,0,0), 'lines')) +
      labs(fill='')
      
      t <- ggplotly(p, tooltip='text') %>% config(displayModeBar = F) %>%
        layout(legend = list(orientation = "h", x = 0, y = -0.1)) %>%
               layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
                      paper_bgcolor = "rgba(0, 0, 0, 0)") %>%
        config(locale = 'ru')
      t$x$data[[1]]$hoverinfo <- "none"
      # t <- t %>% layout(
      #   xaxis = list(
      #     rangeslider = list(type = "date")))
      current_labels <- t[["x"]][["layout"]][["xaxis"]][["ticktext"]]
      current_labels <- as.Date(as.yearmon(current_labels, "%b-%y"))
      labels <- paste(labelset[month(current_labels)], substr(year(current_labels), 3, 4), sep='-')
      t[["x"]][["layout"]][["xaxis"]][["ticktext"]] <- labels
      t
  } else {
    decomp           = model$decomp*100
    forecast         = data.frame(model$forecast*100)
    if (!n.ahead==0) {
      forecast$date <- as.Date(rownames(forecast))
      forecast$date[1] <- forecast$date[1]
    }
    varlabels <- c('Инфляция спроса', varlabels, 'Сезонность', 'Прочие факторы')
    graph_material <- data.frame(decomp)
    names(graph_material) <- c('Значение', varlabels)
    Sys.setlocale("LC_TIME", "Russian")
    if (weekly) {graph_material$date <- as.Date(rownames(graph_material))} else {
      graph_material$date <- as.Date(as.yearmon(rownames(graph_material)))
    } 
    graph_material_long <- reshape2::melt(graph_material[,-1], id.vars='date')
    graph_material <- data.frame(decomp)
    if (weekly) {graph_material$date <- as.Date(rownames(graph_material))} else {
      graph_material$date <- as.Date(as.yearmon(rownames(graph_material)))
    } 
    par(mar = c(0.5,0.5,0.5,0.5))
    trace(grDevices::png, quote({
      if (missing(type) && missing(antialias)) {
        type <- "cairo-png"
        antialias <- "subpixel"
      }
    }), print = FALSE)
    supplem <- graph_material[,-c(1,ncol(graph_material))]
    supplem[supplem<0] <- 0
    upper_lim <- max(rowSums(abs(supplem)))*1.2
    supplem <- graph_material[,-c(1,ncol(graph_material))]
    supplem[supplem>0] <- 0
    lower_lim <- min(rowSums(supplem, na.rm=TRUE))*1.2
    ncolors <- ncol(decomp) - 1
    if (ncolors<=9) {
      colors <- adjustcolor(brewer.pal(n = 9, name = 'Set1'), alpha.f=0.6)
    } else {
      colors <- adjustcolor(brewer.pal(n = 12, name = 'Set3'), alpha.f=1)
    }
    if (!n.ahead==0) {
      if (weekly) {
        shade_start <- last(graph_material$date) + days(4)
      } else {
        shade_start <- last(graph_material$date) + weeks(2)
      }    
      if (weekly) {
        shade_end <- last(forecast$date) + days(4)
      } else {
        shade_end <- last(forecast$date) + weeks(2)
      }
  
        p <- ggplot() +
          annotate("rect", xmin = shade_start, xmax = shade_end, ymin = lower_lim, ymax = upper_lim, alpha = .5)
    }
      if (!n.ahead==0) {
          p <- p + geom_ribbon(data = forecast, aes(x = date, ymin = lower, ymax = upper), alpha=0.5, fill='#72bcd4') +
            geom_line(data = forecast, aes(x = date,  y = value, text = paste('Прогноз, ', paste(day(as.Date(date)), labelset[month(as.Date(date))], year(as.Date(date)), sep=' '), ' м/м: ', round(value,2), '%<br>', 'Нижняя граница: ', round(lower, 2), '%<br>', 'Верхняя граница: ', round(upper, 2), '%', sep='')), size = ifelse(weekly, 0.7, 1), color = "blue", group = 1)
      } else {
          p <- ggplot()
        }
      p <- p + geom_line(data = graph_material, aes(x = date,  y = value, text = paste('Изменение цены, ', paste(day(as.Date(date)), labelset[month(as.Date(date))], year(as.Date(date)), sep=' '), ' м/м: ', round(value,2), '%', sep='')), size = ifelse(weekly, 0.7, 1), color = "black", group = 1) +
        geom_col(data = graph_material_long, position = "stack", show.legend = NA, 
                 aes(x = date, y = value, fill = variable, width = ifelse(weekly, 7, 25), text = paste('Фактор: ', variable, '<br>Влияние, ', paste(day(as.Date(date)), labelset[month(as.Date(date))], year(as.Date(date)), sep=' '), ': ', round(value,2), '%', sep=''))) +
        scale_fill_manual(labels = labels, values=colors) +
        scale_x_date(breaks='6 months', labels = scales::label_date("%b-%y"), expand=expansion(add = 10)) +
        xlab(NULL) +
        ylab('Изменение цены, м/м') +
        scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(lower_lim, upper_lim)) +
        theme(plot.margin = unit(c(0,0,0,0), 'lines')) +
        labs(fill='') +
        theme(legend.position="bottom")
    
      t <- ggplotly(p, tooltip='text') %>% config(displayModeBar = F) %>%
        layout(legend = list(orientation = "h", x = 0, y = -0.1)) %>% 
        layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
               paper_bgcolor = "rgba(0, 0, 0, 0)") %>%
        config(locale = 'ru')
      t$x$data[[1]]$hoverinfo <- "none"
      # t <- t %>% layout(
      #   xaxis = list(
      #     rangeslider = list(type = "date")))
      current_labels <- t[["x"]][["layout"]][["xaxis"]][["ticktext"]]
      current_labels <- as.Date(as.yearmon(current_labels, "%b-%y"))
      labels <- paste(labelset[month(current_labels)], substr(year(current_labels), 3, 4), sep='-')
      t[["x"]][["layout"]][["xaxis"]][["ticktext"]] <- labels
      t
    }
}

plotimpacts <- function(estimated) {
  
 shares <- as.data.frame(round(estimated$shares,2))
 impacts <- as.data.frame(round(estimated$cump_impact,2))
 final <- cbind(shares, impacts)
 other <- last(final)
 final <- final[-nrow(final),]
 final <- final[order(final[,1], decreasing=TRUE),]
 final <- rbind(final, other)

 final <- final[,2,drop=F]
  # names(final) <- c('Объясняющаяя способность', 'Накопленный вклад в изменение цены с 2015 года')
  names(final) <- c('Накопленный вклад в изменение цены с 2015 года')
  
  fig <- plot_ly(
    type = 'table',
    header = list(
      values = c("<b>Фактор</b>", paste('<b>', names(final), '</b>', sep='')),
      align = c('center', rep('center', ncol(final)))
    ),
    cells = list(
      values = rbind(
        rownames(final), 
        # t(paste(as.matrix(unname(final[,1])), '%', sep=''))
        t(paste(as.matrix(unname(final[,1])), ' руб.', sep=''))
      ),
      align = c('left', rep('center', ncol(final)))
    ))
  m <- list(
    l = 5,
    r = 5,
    b = 20,
    t = 20,
    pad = 5
  )
  fig %>% config(displayModeBar = F) %>% layout(margin=m)
}

plot_price_decomp <- function(estimated) {
  decomp <- estimated$price_decomp
  product_name <- estimated$model_name
  weekly <- estimated$weekly
  Sys.setlocale("LC_TIME", "Russian")
  varlabels <- colnames(decomp)
  graph_material <- data.frame(decomp)
  names(graph_material) <- varlabels
  if (weekly) {graph_material$date <- as.Date(rownames(graph_material))} else {
    graph_material$date <- as.Date(as.yearmon(rownames(graph_material)))
  } 
  graph_material_long <- reshape2::melt(graph_material, id.vars='date')
  graph_material$sum <- rowSums(graph_material[,1:8])
  par(mar = c(0.5,0.5,0.5,0.5))
  trace(grDevices::png, quote({
    if (missing(type) && missing(antialias)) {
      type <- "cairo-png"
      antialias <- "subpixel"
    }
  }), print = FALSE)
  p <- ggplot() +
    geom_col(data = graph_material_long, position = "stack", show.legend = NA, 
             aes(x = date, y = value, fill = variable, width = ifelse(weekly, 7, 25), text = paste(variable, ' (', paste(day(as.Date(date)), labelset[month(as.Date(date))], year(as.Date(date)), sep=' '), '): ', round(value,2), ' руб.', sep=''))) +
    geom_line(data = graph_material, aes(x = date,  y = sum, text = paste('Розничная цена, ', paste(day(as.Date(date)), labelset[month(as.Date(date))], year(as.Date(date)), sep=' '), ': ', round(sum, 2), ' &#8381;', sep='')), size = ifelse(weekly, 0.7, 1), color = "black", group = 1) +
    scale_fill_manual(labels = labels, values=adjustcolor(brewer.pal(n = 9, name = 'Set1'),alpha.f=0.6)) +
    scale_x_date(breaks='6 months', labels = scales::label_date("%b-%y"), expand=expansion(add = 10)) +
    xlab(NULL) +
    ylab('Розничная цена, руб.') +
    theme(plot.margin = unit(c(0,0,0,0), 'lines')) +
    labs(fill='')
  t <- ggplotly(p, tooltip='text') %>% config(displayModeBar = F)  %>%
    layout(legend = list(orientation = "h", x = 0, y = -0.1)) %>% 
    layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
           paper_bgcolor = "rgba(0, 0, 0, 0)") %>%
    config(locale = 'ru')
  # t <- t %>% layout(
  #   xaxis = list(
  #     rangeslider = list(type = "date")))
  current_labels <- t[["x"]][["layout"]][["xaxis"]][["ticktext"]]
  current_labels <- as.Date(as.yearmon(current_labels, "%b-%y"))
  labels <- paste(labelset[month(current_labels)], substr(year(current_labels), 3, 4), sep='-')
  t[["x"]][["layout"]][["xaxis"]][["ticktext"]] <- labels
  t
}
# plotelast() plot coefficients with confidence intervals --------------------------------------
plotelast = function(model, nosave=FALSE) {
  alphahat         = model$alphahat
  alphahatsd       = model$alphahatsd
  statenames       = c('Спрос', model$varlabels)
  par(mfrow = c(3, 3), xpd = F)
  # filename = paste(model$model_name, ".png", sep='')
  for (i in 1:ncol(alphahat)) {
    plotalphas(y = alphahat[, i], sdy = alphahatsd[, i], namey = statenames[i])
  }
  title(paste('Эластичности:', model$model_name), outer = TRUE, line=-1)
  if (!nosave) {
    dev.copy(png, paste('./fig/elast/', names(model$df[[1]]), '.png', sep=''), width=1280, height=720)
    dev.off ()
  }
}
# plotalphas() coefficients plot with confidence intervals  ------------------------------------
plotalphas = function(y, sdy, namey) {
  par(mar = c(3, 4, 3.5, 0.5))
  plot(coredata(y), lwd = 3, type = 'l', col = 'blue', xaxt = 'n',
       ylim = c(min(y - sdy), max(y + sdy)), ylab = '', main = namey, xlab = '')
  polygon(x = c(1:length(coredata(y)), length(coredata(y)):1),
          y = c(coredata(y - sdy), coredata(y + sdy)[length(coredata(y)):1]),
          col = rgb(0,0,1,0.15), border = NA)
  abline(v = seq(1, length(y), 12), lty = 3, col = 'gray')
  if (min(y - sdy) <= 0 & max(y + sdy) >= 0) {abline(h = 0, lty = 2, col = 'black')}
  axis(1, at = 1:length(coredata(y)), tick = F,
       labels = paste0(substr(zoo::index(y), 1, 3), '\n', substr(zoo::index(y), 5, 8)))
}
# plotdata() smooth model data plot  ---------------------------------------------------------
plotdata = function(model) {
  df <- (Reduce(function(df1, df2) merge(df1, df2), model$df))
  par(mfrow = c(2, 3), xpd = T)
  for (i in 1:ncol(df)) {
    p = plot(df[,i], col = 'red', grid.col = 'white',
             main = names(df)[i], format.labels="%Y", xaxt='n')
    print(p)
  }
}
# plotinputs() smooth difference model data plot  ----------------------------------------------
plotinputs = function(model) {
  dfmod_sa        = model$dfmod_sa
  dfmod_unsmoothed = model$dfmod_unsmoothed
  names <- c(model$model_name, model$varlabels)
  ## plot inputs
  par(mfrow = c(2, 3), xpd = T)
  for (i in 1:ncol(dfmod_sa)) {
    p = plot(merge(dfmod_sa[,i], na.approx(dfmod_unsmoothed[, i])), col = c('red', 'gray', 'black'), grid.col = 'white',
             main = names[i], format.labels="%Y", xaxt='n')
    print(p)
  }
}
# loadR() load an estimated model into a separate variable ---------------------
loadR <- function(fileName, where='./data_output/models/'){
  fileName2 <- paste(where, fileName, '.RData', sep='')
  load(fileName2)
  get(ls()[which(ls() == fileName)])
}
#' Interactive bar chart of supply sources
#'
#' @param data A list returned by prepare_data()
#'
#' @return Produces an interactive graph showing the dynamics of total supply on the market  
#' @export
stack_supply <- function(data, sa = TRUE, save=FALSE, type='m') {
  df <- data$df
  df_sa <- data$df_sa
  ed <- data$ed
  if (sa) {
    df <- df_sa
  } else {
    df <- df
  }
  # conversion to necessary frequency
  if (type=='q') {
    df <- df %>%
      as_tsibble(index = date, key=type) %>% 
      index_by(yq = ~ yearquarter(.)) %>% 
      as_tibble() %>%
      group_by(type, yq) %>%
      summarise(value = sum(value, na.rm=TRUE)) %>%
      group_by(yq) %>%
      mutate(total = sum(value, na.rm=TRUE), yq=as.Date(yq)) %>%
      rename("date"="yq")
  } else if (type=='y') {
    df <- df %>%
      as_tsibble(index = date, key=type) %>% 
      index_by(yq = ~ year(.)) %>% 
      as_tibble() %>%
      group_by(type, yq) %>%
      summarise(value = sum(value, na.rm=TRUE)) %>%
      group_by(yq) %>%
      mutate(total = sum(value, na.rm=TRUE), yq=as.Date(paste0(yq,'-01-01'))) %>%
      rename("date"="yq")  
    }
  colors <- adjustcolor(brewer.pal(n = 9, name = 'Set1'), alpha.f=0.6)
  p <- ggplot(df)
  p <- p +
    geom_bar(aes(fill=type, y=value, x=date, text=paste(type, ', ', format(date, format='%b %y'), ': ', format(round(value,2), big.mark=" "), ' ', ed, sep='')), stat="identity") +
    geom_line(aes(y=total, x=date, color=ifelse(sa, 'Итого, сез. сглаж.', 'Итого'), 
                  text=paste(ifelse(sa, 'Совокупное предложение (сез. сглаж.), ', 'Совокупное предложение, '), format(date, '%b %y'), ': ', format(round(total,2), big.mark=" "), ' ', ed, sep=''),
                  group=1),
              size=1.2) +
    xlab('') +
    ylab(ed) +
    scale_fill_manual(values = colors, name='') +
    scale_x_date(breaks='6 months', labels = scales::label_date("%b-%y"), expand=expansion(add = 10))
  if (sa) {
    p <- p + scale_color_manual(values = c('Итого, сез. сглаж.' = "#008272"), name='')
  } else {
    p <- p + scale_color_manual(values = c("Итого" = "#008272"), name='')
  }
  p
  if (save) {
    ggsave(paste('fig/', str_replace_all(data$category, ' ', '_'), ifelse(sa, '_sa', ''), '_stack.jpg', sep=''), scale=3)
  }
  t <- ggplotly(p, tooltip='text') %>% config(displayModeBar = F) %>%
    # layout(legend = list(orientation = "h", x = 0, y = -0.1)) %>%
    layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
           paper_bgcolor = "rgba(0, 0, 0, 0)") %>%
    config(locale = 'ru')
  for (i in 1:length(t$x$data)) {
    t$x$data[[i]]$name <- str_sub(str_replace_all(t$x$data[[i]]$name, regex('[//(//)0-9]*'), ''), 1, -2)
    if (!i>=5) {
      t$x$data[[i]]$legendgroup <- 'bars'
    } else {
      t$x$data[[i]]$legendgroup <- 'line'
    }
  }
  t
}
#' Interactive map displaying production across Federal Districts of Russia
#'
#' @param data A list returned by prepare_data()
#'
#' @return Produces an interactive graph showing the dynamics of total supply on the market  
#' @export
production_map <- function(data, rus_map, date_choice, sa = TRUE) {
  if (sa) {
    df <- data$df_FD_sa 
  } else{
    df <- data$df_FD
  }
  ed <- data$ed
  names(df)[2] <- 'name'
  # choice_date <- floor_date(as.Date(), unit='month')
  limits <- c(min(df$value, na.rm=TRUE), max(df$value, na.rm=TRUE))
  
  data_date <- df %>%
    filter(date == date_choice)
  if (date_choice >= as.Date('2015-10-01')) {
    rus_map_noCrimea <- rus_map %>%
      mutate(name = droplevels(replace(name, name == "Крымский федеральный округ", 'Южный федеральный округ')))
    plot_data <- rus_map_noCrimea %>%
      left_join(data_date)
  } else {
    plot_data <- rus_map %>%
      left_join(data_date)
  }
  
  patterns = c("Северо-Западный федеральный округ", "Сибирский федеральный округ", "Уральский федеральный округ", "Дальневосточный федеральный округ", "Южный федеральный округ", 
               "Приволжский федеральный округ", "Центральный федеральный округ", "Северо-Кавказский федеральный округ")
  replacement = c("СЗФО", "СФО", "УФО", "ДФО", "ЮФО", "ПФО", "ЦФО", "СКФО")
  
  plot_data <- plot_data %>%
    mutate(name = str_replace_all(name, setNames(replacement, patterns)))
  
  # breaks <- log_breaks(n=5, base=10)(limits)
  t <- ggplot(plot_data, aes(x=long, y=lat, fill=value, text=paste('Производство, ', name, ', ', format(date_choice, '%b %y'), ': ', round(value,2), ' ', ed, sep=''))) + 
    coord_fixed(ratio = 2) +
    geom_polygon(colour="black", size=0.1, aes(group=group)) +
    scale_fill_gradient(name=paste('Производство, ', ed, sep=''), limits=limits, trans='log10') +
    theme_void()
  t
  t <- ggplotly(t, tooltip='text') %>% config(displayModeBar = F) %>%
    # layout(legend = list(orientation = "h", x = 0, y = -0.1)) %>%
    layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
           paper_bgcolor = "rgba(0, 0, 0, 0)", xaxis = list(showgrid = FALSE, showline=FALSE, fixedrange = TRUE), yaxis = list(showgrid = FALSE, showline=FALSE, fixedrange = TRUE)) %>%
    config(locale = 'ru')
  t
}
plot_map <- function(level_fed, data, date_choice, dynam, type='m') {
  ed <- data$ed
  if (!level_fed) df <- data$df_reg %>% mutate(value = ifelse(value==0, NA, value)) else df <- data$df_FD %>% mutate(value = ifelse(value==0, NA, value))
  if (length(date_choice)==0) {
    date_choice = last(df$date)
  }
  date_choice=floor_date(as.Date(date_choice), unit='months')
  # conversion to necessary frequency
  if (type=='q') {
    laglen <- 4
    date_choice=floor_date(as.Date(date_choice), unit='quarter')
    df <- df %>%
      as_tsibble(index = date, key=OKATO) %>% 
      index_by(yq = ~ yearquarter(.)) %>% 
      as_tibble() %>%
      group_by(OKATO, yq) %>%
      summarise(value = sum(value, na.rm=TRUE), across()) %>%
      select(-date) %>%
      rename("date"="yq") %>%
      distinct() %>%
      mutate(date=as.Date(date))
  } else if (type=='y') {
    laglen <- 1
    date_choice=floor_date(as.Date(date_choice), unit='year')
    df <- df %>%
      as_tsibble(index = date, key=OKATO) %>% 
      index_by(yq = ~ year(.)) %>% 
      as_tibble() %>%
      group_by(OKATO, yq) %>%
      summarise(value = sum(value, na.rm=TRUE), across()) %>%
      select(-date) %>%
      rename("date"="yq") %>%
      distinct() %>%
      mutate(date=as.Date(paste0(date,'-01-01')))
  } else {
    laglen <- 12
  }
  if (!level_fed) {
    grid <- read.table('data_output/rus_grid_reg2.csv', header=TRUE, sep=',') %>%
      mutate(code = ifelse(nchar(code)==10, paste0('0', code), code)) %>%
      select(-code_FD, -FD) %>% rename("name" = "name_short")
    if (dynam) {
      data_date <- df %>% group_by(OKATO_id) %>% mutate(value = (value/lag(value, laglen) - 1)*100 ) %>%
        filter(date == date_choice) %>%
        right_join(grid, by=c("OKATO_id" = "code")) %>% select(-row, -col) %>% rename("code" = "OKATO_id")
    } else {
      data_date <- df %>%
        filter(date == date_choice) %>%
        right_join(grid, by=c("OKATO_id" = "code")) %>% select(-row, -col) %>% rename("code" = "OKATO_id")
    }
  }
  if (level_fed) {
    grid <- read.table('data_output/rus_grid_reg2.csv', header=TRUE, sep=',') %>%
      mutate(code = ifelse(nchar(code)==10, paste0('0', code), code)) %>%
      mutate(code_FD = ifelse(nchar(code_FD)==2, paste0('0', code_FD), code_FD))
    if (dynam) {
      data_date <- df %>% group_by(OKATO_id) %>% mutate(value = (value/lag(value, laglen) - 1)*100 ) %>%
        filter(date == date_choice) %>%
        right_join(grid, by=c("OKATO_id" = "code_FD")) %>% select(-row, -col)
    } else {
      data_date <- df %>%
        filter(date == date_choice) %>%
        right_join(grid, by=c("OKATO_id" = "code_FD"))
    }
    grid <- grid %>% select(-code_FD) %>% rename("name" = "FD")
  }
  p <- data_date %>% ggplot() +
    geom_rect(mapping=aes(xmin=1, xmax=2, ymin=1, ymax=2, fill=value, text=""),
                                          color=NA, alpha=1) +
    facet_geo(~ code, grid = grid) +
    labs(x='', y='') +
    theme_minimal() + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          strip.placement = "bottom",
          plot.title = element_text(hjust = 5),
          strip.background = element_blank(),
          strip.text.x = element_blank(), 
          line = element_blank(),
          plot.margin = margin(-2, "points"),
          panel.spacing = unit(-2, "points")
          )
  if (dynam) {
    p <- p + scale_fill_gradientn(name='Темпы роста, % г/г',
                                  colours=c("#e83131", "#ffbfbf", "dark gray", "#c3ffbf","#3AE831"),
                                  na.value="#D0D0D0",
                                  oob=oob_squish,
                                  # trans="pseudo_log",
                                  values=rescale(c(-10,-0.00000001,0,0.000000001,10)),
                                  n.breaks=4,
                                  minor_breaks = NULL,
                                  limits=c(-10,10))
    # p <- p + scale_fill_manual(name='Темпы роста производства, %',
    #                               values=c("1" = "#FF0000", "2" = "#e83131", "3" = "#ffbfbf", "4" = "#c3ffbf", "5" = "#3AE831", "6" = "#0dff00"),
    #                               na.value="#D0D0D0")
    # p <- p + binned_scale("fill",
    #                           "foo",
    #                           ggplot2:::binned_pal(scales::manual_pal(c("#FF0000","#e83131","#ffbfbf","#c3ffbf", "#3AE831", "#0dff00"))),
    #                           guide="coloursteps",
    #                           breaks=c(-10,-5,0,5,10),
    #                           limits=c(-10, 10),
    #                           oob=oob_squish,
    #                           na.value="#D0D0D0",
    #                       show.limits = TRUE,
    #)
  } else {
    p <- p + scale_fill_gradient(name=paste('Производство, ', ed, sep=''), na.value="#D0D0D0", high="#85D6FF", low = "#0078D4")
  }
  if (!level_fed) {
    p <- p + geom_text(aes(x = 1.5, y = 1.5, label = name, 
                           text=paste0(name_long, ', ', as.yearmon(date_choice), ': ', ifelse(is.na(value),
                                                                                              'нет данных',
                                                                                              paste0(round(value, 1), ifelse(dynam,
                                                                                                                             '%',
                                                                                                                             paste0(' ', ed))
                                                                                                     )
                                                                                              )
                                       )
                           ), col="white", size=4)
  } else {
    p <- p + geom_text(aes(x = 1.5, y = 1.5, label = paste0(name_short, '\n', FD), 
                           text=paste0(FD, ', ', as.yearmon(date_choice), ': ', ifelse(is.na(value),
                                                                                       'нет данных',
                                                                                       paste0(round(value, 1), ifelse(dynam,
                                                                                                                      '%',
                                                                                                                      paste0(' ', ed))
                                                                                              )
                                                                                       )
                                       )
                           ), col="white", size=4)
  }
  p <- ggplotly(p, height=600, tooltip='text') %>%
    config(doubleClick = F, displayModeBar = F) %>% layout(dragmode=FALSE, plot_bgcolor  = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)", 
                                          xaxis = list(showgrid = FALSE, showline=FALSE, fixedrange = TRUE), yaxis = list(showgrid = FALSE, showline=FALSE, fixedrange = TRUE)) %>% 
    config(locale = 'ru')
  # p$x$data <- modify_depth(p$x$data, 1, function(x) {
  #   # x[["hoverinfo"]] = ifelse(!x[["hoveron"]]=="fills", "none", x[["hoverinfo"]])
  #   x$hoveron
  # }) 
  idx <- which(unlist(lapply(p$x$data, function(x) x$hoveron=="fills")))
  p$x$data[idx] <- lapply(p$x$data[idx], function(x) x <- list_modify(x, hoverinfo="none"))
  p 
}


plot_trade <- function(data, date_choice) {
  df <- data$trade_concentr %>%
    pivot_longer(-date) %>% filter(!name %in% c('HHI_im', 'HHI_ex')) %>% 
    mutate(name = case_when(
      name == 'HHI_im_sa' ~ 'Импорт',
      name == 'HHI_ex_sa' ~ 'Экспорт'
    )) 
  p1 <- df %>% ggplot() +
    geom_line(aes(x=date, y=value, col=name, text=paste0('Индекс географической концентрации',
                                                         ifelse(name=="Экспорт", ' экспорта', ' импорта'),
                                                         ', ',
                                                         as.yearmon(date),
                                                         ': ',
                                                         round(value, 2)),
                  group=name)) +
    geom_text(data=last(df %>% filter(name=='Экспорт')), aes(x=date, y=value), label="Экспорт", vjust=1.5) +
    geom_text(data=last(df %>% filter(name=='Импорт')), aes(x=date, y=value), label="Импорт", vjust=1.5) +
    scale_color_manual(values=c("Экспорт" = 'red', "Импорт" = 'blue'), name='Индекс концентрации') +
    xlab('') +
    ylab('Пункты') +
    theme(legend.position="none")
  p1 <- ggplotly(p1, height=700, tooltip='text') %>%
    config(displayModeBar = F, locale = 'ru') %>% layout(plot_bgcolor  = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)")
  df <- data$trade_shares %>% mutate(share=share*100)
  p2 <- df %>% ggplot() +
    geom_line(aes(x=date, y=share, col=type, text=paste0('Доля',
                                                         ifelse(type=="Экспорт", ' экспорта', ' импорта'),
                                                         ' от внутреннего производства, ',
                                                         as.yearmon(date),
                                                         ': ',
                                                         round(share, 2), 
                                                         '%'),
                  group=type)) +
    geom_text(data=last(df %>% filter(type=='Экспорт')), aes(x=date, y=share), label="Экспорт", vjust=1.5) +
    geom_text(data=last(df %>% filter(type=='Импорт')), aes(x=date, y=share), label="Импорт", vjust=1.5) +
    scale_color_manual(values=c("Экспорт" = 'red', "Импорт" = 'blue'), name='Доля от внутреннего производства') +
    xlab('') +
    ylab('Проценты') +
    theme(legend.position="none")
  p2 <- ggplotly(p2, height=700, tooltip='text') %>%
    config(displayModeBar = F, locale = 'ru') %>% layout(plot_bgcolor  = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)")
  p2
  if (length(date_choice)==0) {
    date_choice = last(df$date)
  }
  date_choice=floor_date(as.Date(date_choice), unit='months')
  df <- data$trade_country %>% filter(date == date_choice) %>% mutate(NAPR = case_when(
    NAPR=='ИМ' ~ 'Импорт',
    NAPR=='ЭК' ~ 'Экспорт'))
  # ed <- tolower(data$trade_price$EDIZM[1])
  ed <- data$ed_tradeprice
  bars <- map(unique(df$NAPR)
                    , ~geom_bar(stat = "identity", position = "stack", data = df %>% filter(NAPR == .x)))
  labels <- geom_text(aes(y = label_y, label = NAME), colour = "white", 
                      data = df %>% group_by(NAPR) %>%
                        arrange(NAPR, desc(value)) %>%
                        mutate(label_y = cumsum(value) - 0.5*value))
  p3 <- df %>% 
    ggplot(aes(x = NAPR, y = value, fill = reorder(NAME, value), text=paste0(NAPR, ifelse(NAPR=='ЭК', ' в ', ' из '), NAME,', ',as.yearmon(date),': ',
                                                                                 format(round(value), big.mark=" "), ' ', ed))) + 
    bars +
    labels +
    guides(fill=guide_legend("ordering")) +
    xlab('') +
    ylab(ed) +
    scale_y_continuous(labels = function(x) format(x, big.mark = " ")) +
    theme(legend.position="none")
  p3
  # p3 <- df %>%
  #   arrange(NAPR, rev(NAME)) %>%
  #   group_by(NAPR) %>%
  #   mutate(label_y = cumsum(value) - 0.5*value) %>%
  #   ggplot(aes(x=NAPR, y = value, fill = NAME)) +
  #   geom_col(stat_count="identity", aes(text=paste0(NAPR,
  #                            ' в ',
  #                            NAME,
  #                            ', ',
  #                            as.yearmon(date),
  #                            ': ',
  #                            format(round(value), big.mark=" "),
  #                            ' ',
  #                            ed
  #                            ))) +
  #   geom_text(aes(y = label_y, label = NAME),  colour = "white") +
  #   # facet_wrap(NAPR~., scale='free_y') + 
  #   xlab('') +
  #   ylab(ed) +
  #   scale_y_continuous(labels = function(x) format(x, big.mark = " ")) +
  #   theme(legend.position="none")
  p3 <- ggplotly(p3, height=900, tooltip='text') %>%
    config(displayModeBar = F, locale = 'ru') %>% layout(plot_bgcolor  = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)") 
  idx <- which(unlist(lapply(p3$x$data, function(x) !is.null(x$hoveron))))
  p3$x$data[idx] <- lapply(p3$x$data[idx], function(x) x <- list_modify(x, hoverinfo="none"))
  # fig1 <- subplot(p1, p2, nrows = 2, margin = 0.05)
  # fig2 <- subplot(p3, nrows = 1, margin = 0.05)
  # fig <- subplot(fig1, fig2, nrows = 2, margin = 0.05)
  # fig
  fig4 <- subplot(style(p3, showlegend=F), p1, style(p2, showlegend=F), nrows = 3, margin = 0.05)
  annotations = list( 
    list( 
      x = 0.5,  
      y = 1.0,  
      text = paste0("Крупнейшие торговые партнеры, ", as.yearmon(date_choice)), 
      xref = "paper",  
      yref = "paper",  
      xanchor = "center",  
      yanchor = "bottom",  
      showarrow = FALSE 
    ),  
    list( 
      x = 0.5,  
      y = 2/3 - 0.05,  
      text = "Индексы географической концентрации",  
      xref = "paper",  
      yref = "paper",  
      xanchor = "center",  
      yanchor = "bottom",  
      showarrow = FALSE 
    ),  
    list( 
      x = 0.5,  
      y = 1/3 - 0.05,  
      text = "Доля внешней торговли от внутреннего производства",  
      xref = "paper",  
      yref = "paper",  
      xanchor = "center",  
      yanchor = "bottom",  
      showarrow = FALSE 
    )
    )
  fig4 %>% layout(annotations = annotations) %>%
    config(doubleClick = F, displayModeBar = F) %>% layout(dragmode=FALSE,  
                                                           xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)) %>% 
    config(locale = 'ru')
}

trade_prices <- function(data, convert) {
  df <- data$trade_price
  # ed <- tolower(df$EDIZM[1])
  ed <- data$ed_tradeprice
  p <- df %>% mutate(NAPR = case_when(NAPR=='ЭК' ~ "Экспорт", NAPR=='ИМ' ~ 'Импорт')) 
  if (convert) {
    p <- p %>% select(-mean_price) %>% rename("mean_price"="mean_price_rub")
  }
  p <- p %>% ggplot(aes(x=date, y=mean_price)) +
      geom_line(aes(col=NAPR, text=paste0('Средняя цена',
                                                         ifelse(NAPR=="Экспорт", ' экспорта', ' импорта'),
                                                         ', ',
                                                         as.yearmon(date),
                                                         ': ',
                                                         round(mean_price, 2), 
                                                         ifelse(convert, ' руб./', '$/'), ed),
                  group=NAPR)) +
    geom_text(data=last(df %>% filter(NAPR=='Экспорт')), aes(x=date, y=mean_price), label="Экспорт", vjust=1.5) +
    geom_text(data=last(df %>% filter(NAPR=='Импорт')), aes(x=date, y=mean_price), label="Импорт", vjust=1.5) +
    scale_color_manual(values=c("Экспорт" = 'red', "Импорт" = 'blue'), name='Доля от внутреннего производства') +
    xlab('') +
    ylab(paste0(ifelse(convert, 'руб./', '$/'), ed)) +
    theme(legend.position="none")
  p <- ggplotly(p, tooltip='text') %>%
    config(displayModeBar = F, locale = 'ru') %>% layout(plot_bgcolor  = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)")
  p
}
prod_prices <- function(data) {
  df <- data$prod_price
  our_strwrap <- function(x) sapply(strwrap(x, width = 40, simplify= FALSE), paste, collapse = "\n")
  # ed <- tolower(df$EDIZM[1])
  p <- df %>% ggplot() + geom_line(aes(x=date, y=value, col=group, group=group, text=paste0('Средние цены производителей, ', 
                                                our_strwrap(OKPD),
                                               ', ',
                                               as.yearmon(date),
                                               ': ',
                                               round(value, 2), 
                                               ' руб.'),
                         group=1)) +
    xlab('') +
    ylab('руб.') +
    theme(legend.position='none')
  p <- ggplotly(p, tooltip='text') %>%
    config(displayModeBar = F, locale = 'ru') %>% layout(plot_bgcolor  = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)")
  p
}
retail_trade <- function(data, sa=TRUE) {
  if (!is.null(data$df_retail_FD_sa)) {
    
    if (sa) {
      df <- data$df_retail_FD_sa
    } else {
      df <- data$df_retail_FD
    }
    df <- df %>%
      group_by(date) %>%
      mutate(total = sum(value, na.rm=TRUE))
    colors <- adjustcolor(brewer.pal(n = 9, name = 'Set1'), alpha.f=0.6)
    p <- ggplot(df) +
      geom_col(aes(fill=OKATO, y=value, x=date, text=paste0('Продажи, ', OKATO, ', ', as.yearmon(date), ': ', format(value, big.mark=" "), ' ', data$ed_retail)), position='stack') +
      geom_line(aes(y=total, x=date, color='Итого', text=paste0('Продажи, всего, ', as.yearmon(date), ': ', format(total, big.mark=" "), ' ', data$ed_retail),
                    group=1),
                size=1.2) +
      xlab('') +
      ylab(paste0('Розничные продажи, ', data$ed_retail)) +
      scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE)) +
      scale_fill_manual(values = colors, name='') +
      scale_x_date(breaks='6 months', labels = scales::label_date("%b-%y"), expand=expansion(add = 10)) +
      scale_color_manual(values = c("Итого" = "#008272"), name='') +
      theme(legend.position="bottom")
    p
    t <- ggplotly(p, tooltip='text') %>%
      config(displayModeBar = F, locale = 'ru') %>% layout(plot_bgcolor  = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)") %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.1))
    for (i in 1:length(t$x$data)) {
      t$x$data[[i]]$name <- str_sub(str_replace_all(t$x$data[[i]]$name, regex('[//(//)0-9]*'), ''), 1, -2)
    }
  } else {
    if (sa) {
      df <- data$df_retail_sa
    } else {
      df <- data$df_retail
    }
    df <- df %>%
      group_by(date) %>%
      mutate(total = sum(value, na.rm=TRUE)) %>%
      mutate(date=as.Date(date))
    colors <- adjustcolor(brewer.pal(n = 9, name = 'Set1'), alpha.f=0.6)
    p <- ggplot(df) +
      # geom_bar(aes(y=value, x=date, text=paste0('Продажи, ', as.yearmon(date), ':', format(value, big.mark=" "), ' руб.')), stat="identity") +
      geom_line(aes(y=total, x=date, color='Итого', text=paste0('Продажи, всего, ', as.yearmon(date), ': ', format(total, big.mark=" "), ' руб.'),
                    group=1),
                size=1.2) +
      xlab('') +
      scale_x_date(breaks='6 months', labels = scales::label_date("%b-%y"), expand=expansion(add = 10)) +
      scale_color_manual(values = c("Итого" = "#008272"), name='') +
      theme(legend.position="bottom")
    t <- ggplotly(p, tooltip='text') %>%
      config(displayModeBar = F, locale = 'ru') %>% layout(plot_bgcolor  = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)") %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.1))
  }
  t
} 
margin_plot <- function(data) {
  df <- data$margin %>%
    mutate(value = value*100)
  df$type <- case_when(
    str_detect(df$name,'розн') ~ 'Розничная торговля',
    str_detect(df$name,'оптов') ~ 'Оптовая торговля',
  )
  
  p <- df %>% ggplot() + geom_line(aes(x=date, y=value, col=type, text=paste0('Уровень наценки',
                                                                                ', ',
                                                                                as.yearmon(date),
                                                                                ': ',
                                                                                format(round(value, 2), big.mark=" "), 
                                                                                '%'),
                                       group=type)) +
    scale_color_discrete(name='') +
    xlab('') +
    ylab('%') +
    theme(legend.position='bottom')
  p <- ggplotly(p, tooltip='text') %>%
    config(displayModeBar = F, locale = 'ru') %>% layout(plot_bgcolor  = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)") %>%
    layout(legend = list(orientation = "h", x = 0.3, y = -0.1))
  p 
} 
empty_plot <- function(title = NULL, x, y){
  p <- plotly_empty(type = "scatter", mode = "markers") %>%
    config(
      displayModeBar = FALSE
    ) %>%
    layout(
      title = list(
        text = title,
        font = list(size=15),
        yref = "paper",
        y = y, 
        x = x
      )
    )
  return(p)
} 
balance_plot <- function(data) {
  deltas <- data$meanImpact
  if (!is.na(deltas)) {
    vals <- round(c(deltas$d_tax_implied, deltas$d_prod, deltas$d_fot, -deltas$marking_cost)/10^9,1)
    df <- data.frame(
      desc=c('Налоги', 'Выручка', 'ФОТ', 'Издержки', 'Итого'),
      value=c(vals, sum(vals))
    )
    df$desc <- factor(df$desc, levels = df$desc)
    df$id <- seq_along(df$value)
    df$type <- ifelse(df$value > 0, "in", "out")
    df[df$desc %in% c("Итого"), "type"] <- "net"
    df$end <- cumsum(df$value)
    df$end <- c(head(df$end, -1), 0)
    df$start <- c(0, head(df$end, -1))
    df <- df[, c(3, 1, 4, 6, 5, 2)]
    
    balance <- ggplot(df) +
      geom_rect(aes(x = desc, fill = type, xmin = id - 0.4, xmax = id + 0.4, ymin = end, ymax = start, text=
                      paste0(desc, ': ', value, ' млрд руб.')
      )) +
      geom_segment(data = df[1:(nrow(df) -1),], aes(x = id + 0.4,
                                                    xend = id + 0.6,
                                                    y = end,
                                                    yend = end), col='darkgray') +
      geom_text(aes(x=desc, y=start+(end-start)/2, label=paste0(value)), color="black", size=4) +
      coord_flip() +
      ggtitle('Баланс затрат и выгод') +
      ylab('млрд руб.') +
      xlab(NULL) +
      scale_fill_manual(values=c("in"="#B0E3AB", "out"="#F9AAB0", "net"='#C7E0F4')) +
      theme(legend.position = 'none', plot.title = element_text(hjust = 0.5))
    balance <- ggplotly(balance, tooltip='text') %>% layout(plot_bgcolor  = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)",
                                                            autosize = T, dragmode=FALSE,  
                                                            xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
    balance$x$data[[7]]$hoverinfo <- "none"
    
    # df2 <- df %>% 
    #   filter(desc == 'Налоги') %>%
    #   pull(value)
    # 
    # taxStruct = data$taxStruct
    # df2 = data.frame(value = round(df2 * (taxStruct %>% pull(structure)),1),
    #                  name = taxStruct %>% pull(type))
    # df2 <- df2[order(df2$value, decreasing=T),]
    # df2 <- df2[c(2:nrow(df2),1),]
    # df2$id <- seq_along(df2$value)
    # df2$name <- factor(df2$name, levels = df2$name)
    # df2$type <- ifelse(df2$value > 0, "in", "out")
    # df2[df2$name %in% c("Всего"), "type"] <- "net"
    # 
    # df2$end <- cumsum(df2$value)
    # df2$end <- c(head(df2$end, -1), 0)
    # df2$start <- c(0, head(df2$end, -1))
    # df2 <- df2[, c(3, 1, 4, 6, 5, 2)]
    # 
    # balance_tax <- df2 %>%
    #   ggplot() +
    #   geom_rect(aes(x = name, fill = type, xmin = id - 0.4, xmax = id + 0.4, ymin = end, ymax = start, text=
    #                   paste0(name, ': ', value, ' млрд руб.')
    #   )) +
    #   geom_segment(data = df2[1:(nrow(df2) -1),], aes(x = id + 0.4,
    #                                                 xend = id + 0.6,
    #                                                 y = end,
    #                                                 yend = end), col='darkgray') +
    #   geom_text(aes(x=name, y=start+(end-start)/2, label=paste0(value)), color="black", size=4) +
    #   coord_flip() +
    #   # ggtitle('Прирост налоговых поступлений') +
    #   ylab('млрд руб.') +
    #   xlab(NULL) +
    #   scale_fill_manual(values=c("in"="#B0E3AB", "out"="#F9AAB0", "net"='#C7E0F4')) +
    #   theme(legend.position = 'none', plot.title = element_text(hjust = 0.5))
    # balance_tax
    # balance_tax <- ggplotly(balance_tax, tooltip='text') %>% layout(plot_bgcolor  = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)",
    #                                                         autosize = T, dragmode=FALSE,  
    #                                                         xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
    # balance_tax$x$data[[8]]$hoverinfo <- "none"
    # 
    # balance_tax
      
      
    
    
    # balance
    empl_fig <- plot_ly(
      domain = list(x = c(0.25, 0.5), y = c(0.8, 1)),
      type = "indicator",
      mode = "delta",
      gauge = list(shape = "bullet"),
      delta = list(reference = 0, font = list(size=18, family='Segoe UI', color='#323130')),
      value = round(deltas$d_empl),
      title = list(text = "Занятость, чел.", font = list(size=18, family='Segoe UI', color='#323130')),
      # height = 150
    )
    capacity_fig <- plot_ly(
      domain = list(x = c(0.5, 0.75), y = c(0.8, 1)),
      type = "indicator",
      mode = "delta",
      gauge = list(shape = "bullet"),
      delta = list(reference = 1, relative=TRUE, font = list(size=18, family='Segoe UI', color='#323130')),
      value = 1 + round(deltas$d_cap_implied,2)/100,
      title= list(text = "Загрузка мощностей", font = list(size=18, family='Segoe UI', color='#323130')),
      # height = 150
    )
    # capacity_fig
    combo2 <- subplot(empl_fig, capacity_fig, nrows=2) %>%  layout(plot_bgcolor  = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)",
                                                                   autosize = T, dragmode=FALSE,  
                                                                   xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
    # combo2
    combo3 <- subplot(combo2, balance, nrows=2, titleY = TRUE, which_layout=2, heights=c(0.25,0.75)) %>%  layout(plot_bgcolor  = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)",
                                                                                                  autosize = T, dragmode=FALSE,  
                                                                                                  xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
  } else {
    combo3 <- empty_plot("Баланс затрат и выгод ОЦМ еще не рассчитан", x = 0.5, y=0.5) %>%  layout(plot_bgcolor  = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)",
                                                                                                   autosize = T, dragmode=FALSE,  
                                                                                                   xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
  }
  combo3 %>% config(displayModeBar = F)
}
tax_delta_structure <- function(data) {
  deltas <- data$meanImpact
  if (!is.na(deltas)) {
  vals <- round(c(deltas$d_tax_implied, deltas$d_prod, deltas$d_fot, -deltas$marking_cost)/10^9,1)
  df <- data.frame(
      desc=c('Налоги', 'Выручка', 'ФОТ', 'Издержки', 'Итого'),
      value=c(vals, sum(vals))
    )
  df2 <- df %>% 
    filter(desc == 'Налоги') %>%
    pull(value)
  
  taxStruct = data$taxStruct
  df2 = data.frame(value = round(df2 * (taxStruct %>% pull(structure)),1),
                   name = taxStruct %>% pull(type))
  df2 <- df2[order(df2$value, decreasing=T),]
  df2 <- df2[c(2:nrow(df2),1),]
  df2$id <- seq_along(df2$value)
  df2$name <- factor(df2$name, levels = df2$name)
  df2$type <- ifelse(df2$value > 0, "in", "out")
  df2[df2$name %in% c("Всего"), "type"] <- "net"
  
  df2$end <- cumsum(df2$value)
  df2$end <- c(head(df2$end, -1), 0)
  df2$start <- c(0, head(df2$end, -1))
  df2 <- df2[, c(3, 1, 4, 6, 5, 2)]
  
  balance_tax <- df2 %>%
    ggplot() +
    geom_rect(aes(x = name, fill = type, xmin = id - 0.4, xmax = id + 0.4, ymin = end, ymax = start, text=
                    paste0(name, ': ', value, ' млрд руб.')
    )) +
    geom_segment(data = df2[1:(nrow(df2) -1),], aes(x = id + 0.4,
                                                    xend = id + 0.6,
                                                    y = end,
                                                    yend = end), col='darkgray') +
    geom_text(aes(x=name, y=start+(end-start)/2, label=paste0(value)), color="black", size=4) +
    coord_flip() +
    ggtitle('Прирост налоговых поступлений') +
    ylab('млрд руб.') +
    xlab(NULL) +
    scale_fill_manual(values=c("in"="#B0E3AB", "out"="#F9AAB0", "net"='#C7E0F4')) +
    theme(legend.position = 'none', plot.title = element_text(hjust = 0.5))
  balance_tax
  balance_tax <- ggplotly(balance_tax, tooltip='text') %>% layout(plot_bgcolor  = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)",
                                                                  autosize = T, dragmode=FALSE,  
                                                                  xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
  balance_tax$x$data[[8]]$hoverinfo <- "none"
  } else {
    balance_tax <- empty_plot("Прирост налоговых поступлений еще не расчитан", x = 0.5, y=0.5) %>%  layout(plot_bgcolor  = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)",
                                                                                         autosize = T, dragmode=FALSE,  
                                                                                         xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
  }
  balance_tax %>% config(displayModeBar = F)
}
counterfeit_plot <- function(data, opacity=0) {
  if (!is.na(data$counterfeit)) {
  df <- data$counterfeit %>%
    drop_na() %>%
    filter(date > as.Date('2014-01-01'))
  value <- last(df %>%
                  pull(counterfeit_smooth))
  date <- last(df %>%
                 pull(date))
  cntft_plot <- plot_ly(
    domain = list(x = c(0.5, 1), y = c(0.2, 0.6)),
    value = value,
    # title = list(text = paste0('Доля нелегального оборота\n',as.yearqtr(date)), font=list(size=20, family='Segoe UI', color='#323130')),
    type = "indicator",
    mode = "gauge+number",
    gauge = list(
      axis = list(range = list(NULL, 50), ticks='outside', ticklen=5, tickcolor='#323130', tickfont=list(size=14, family='Segoe UI', color='#323130'), ticksuffix='%'),
      bar = list(color='#e83131'),
      borderwidth = 0,
      bordercolor = '#0078D4',
      threshold = list(line = list(color='#0078D4'), thickness=15, value = value)
    ),
    number = list(
      suffix='%',
      prefix='     ',
      font = list(size=18, family='Segoe UI', color='#323130')
    )
  ) 
  # df <- df %>%
  #   mutate(counterfeit_smooth = lowess(df$counterfeit, f=1/4)$y)
  cntft_dyn <- plot_ly(df,
                       x = df$date, y = df$counterfeit_smooth, mode = 'lines', hoverinfo = 'text', text=paste0('Доля нелегального оборота',
                                                                                                                   ', ',
                                                                                                                   as.yearmon(df$date),
                                                                                                                   ': ',
                                                                                                                   format(round(df$counterfeit_smooth, 1), big.mark=" "), 
                                                                                                                   '%'),
                       type = "scatter", line = list(color = "#007d3c")) %>%
    add_trace(y = ~counterfeit, name = 'trace 0', mode = 'lines', opacity=opacity, hoverinfo = 'none')  %>%
    layout(yaxis = list(title = 'Доля нелегального оборота, %', fixedrange=T), 
           xaxis = list(fixedrange = TRUE, dtick = "M3", tickformat="%qК%Y%"))

  if (!is.na(data$markingDate)) {
    vline <- function(x = 0, color = "red") {
      list(
        type = "line", 
        y0 = 0, 
        y1 = 1, 
        yref = "paper",
        x0 = x, 
        x1 = x, 
        line = list(color = color)
      )
    }

    cntft_dyn <- cntft_dyn %>%
      layout(shapes = list(vline(data$markingDate))) %>%
      add_segments(x = data$markingDate, xend = last(df$date), y = df[df$date==data$markingDate,'counterfeit_smooth'], text=NA, yend = df[df$date==data$markingDate,'counterfeit_smooth'], line = list(color='black')) %>%
      # add_segments(x = last(df$date), xend = last(df$date), y = df[df$date==data$markingDate,'counterfeit_smooth'], yend = last(df$counterfeit_smooth), line = list(dash = "dash")) %>%
      add_annotations(x = last(df$date),
                       y = last(df$counterfeit_smooth),
                       xref = "x", yref = "y",
                       axref = "x", ayref = "y",
                       text = "",
                       arrowcolor = "black",
                       showarrow = T,
                       line = list(dash = "dash"),
                       ax = last(df$date),
                       ay = df[df$date==data$markingDate,'counterfeit_smooth']) %>%
      add_annotations(x = as.Date(round( (as.numeric(data$markingDate) + as.numeric(last(df$date))) /2,0)),
                       y = df[df$date==data$markingDate,'counterfeit_smooth']*1.1,
                       xref = "x", yref = "y",
                       text = paste0( round( last(df$counterfeit_smooth) - df[df$date==data$markingDate,'counterfeit_smooth'], 1), '%'),
                       font = list(size=18),
                       showarrow = F) %>%
      add_annotations(x = data$markingDate-months(10), y = max(max(df$counterfeit_smooth), max(df$counterfeit))*0.9,
                      text = c("Начало ОЦМ в отрасли"), hoverinfo='none', showarrow=F) 
    cntft_dyn %>% layout(showlegend = FALSE)
    
  }
  
  annotations = list( 
    list( 
      x = 0.75,  
      y = 1,  
      text = paste0('Доля нелегального оборота, ',as.yearqtr(date)),  
      xref = "paper",  
      yref = "paper",  
      xanchor = "center",  
      yanchor = "bottom",  
      showarrow = FALSE,
      font = list(size = 18)
    ),  
    list( 
      x = 0.25,  
      y = 1,  
      text = "Динамика доли нелегального оборота",  
      xref = "paper",  
      yref = "paper",  
      xanchor = "center",  
      yanchor = "bottom",  
      showarrow = FALSE,
      font = list(size = 18)
    )
  )
  combo <- subplot(cntft_dyn, cntft_plot, nrows  = 1, titleY = TRUE) %>% layout(annotations = annotations, showlegend = F, plot_bgcolor  = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)",
                                                                                dragmode=FALSE) %>%
    config(displayModeBar = F, locale = 'ru')
  combo 
  } else {
    combo <- empty_plot("Доля нелегального оборота еще не рассчитана", x = 0.5, y=0.5) %>%  layout(plot_bgcolor  = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)",
                                                                                                   autosize = T, dragmode=FALSE,  
                                                                                                   xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
    combo %>% config(displayModeBar = F)
  
  }
}
# colours=c("#e83131", "#ffbfbf", "dark gray", "#c3ffbf","#3AE831"),
