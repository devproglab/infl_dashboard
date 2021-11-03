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

using("data.table", "readxl", "dplyr", "xts", "plotly", "stats", "writexl",
      "lubridate", "ggplot2", "shiny", "scales", "flexdashboard", "RColorBrewer", "clock",
      "downloadthis")


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
    upper_lim <- max(graph_material[,-ncol(graph_material)])*1.5
    lower_lim <- min(graph_material[,-ncol(graph_material)])*1.5
    ncolors <- ncol(decomp2) - 1
    if (ncolors<=9) {
      colors <- adjustcolor(brewer.pal(n = 9, name = 'Set1'), alpha.f=0.6)
    } else {
      colors <- adjustcolor(brewer.pal(n = 12, name = 'Set3'), alpha.f=1)
    }
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
    if (!n.ahead==0) {
      p <- p + 
        geom_ribbon(data = forecast2, aes(x = date, ymin = lower, ymax = upper), alpha=0.5, fill='#72bcd4') +
        geom_line(data = forecast2, aes(x = date,  y = value, text = paste('Прогноз, ', paste(day(as.Date(date)), labelset[month(as.Date(date))], year(as.Date(date)), sep=' '), ' г/г: ', round(value,2), '%<br>', 'Нижняя граница: ', round(lower, 2), '%<br>', 'Верхняя граница: ', round(upper, 2), '%', sep='')), size = ifelse(weekly, 0.7, 1), color = "blue", group = 1)
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
    upper_lim <- max(graph_material[,-ncol(graph_material)])*1.3
    lower_lim <- min(graph_material[,-ncol(graph_material)])*1.3
    ncolors <- ncol(decomp) - 1
    if (ncolors<=9) {
      colors <- adjustcolor(brewer.pal(n = 9, name = 'Set1'), alpha.f=0.6)
    } else {
      colors <- adjustcolor(brewer.pal(n = 12, name = 'Set3'), alpha.f=1)
    }
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
      if (!n.ahead==0) {
          p <- p + geom_ribbon(data = forecast, aes(x = date, ymin = lower, ymax = upper), alpha=0.5, fill='#72bcd4') +
            geom_line(data = forecast, aes(x = date,  y = value, text = paste('Прогноз, ', paste(day(as.Date(date)), labelset[month(as.Date(date))], year(as.Date(date)), sep=' '), ' м/м: ', round(value,2), '%<br>', 'Нижняя граница: ', round(lower, 2), '%<br>', 'Верхняя граница: ', round(upper, 2), '%', sep='')), size = ifelse(weekly, 0.7, 1), color = "blue", group = 1)
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

plotimpacts <- function(estimated, par) {
  
 shares <- as.data.frame(round(estimated$shares,2))
 impacts <- as.data.frame(round(estimated$cump_impact,2))
 final <- cbind(shares, impacts)
 other <- last(final)
 
 final <- final[-nrow(final),]
 final <- final[order(final[,1], decreasing=TRUE),]
 final <- rbind(final, other)
  
  names(final) <- c('Объясняющаяя способность', 'Накопленный вклад в изменение цены с 2015 года')
  fig <- plot_ly(
    type = 'table',
    header = list(
      values = c("<b>Фактор</b>", paste('<b>', names(final), '</b>', sep='')),
      align = c('center', rep('center', ncol(final)))
    ),
    cells = list(
      values = rbind(
        rownames(final), 
        t(paste(as.matrix(unname(final[,1])), '%', sep='')),
        t(paste(as.matrix(unname(final[,2])), ' руб.', sep=''))
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
loadR <- function(fileName){
  #loads an RData file, and returns it
  fileName <- paste('data_output/models/', fileName, '.RData', sep='')
  load(fileName)
  get(ls()[ls() != "fileName"])
}

