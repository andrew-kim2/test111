# -------------------------------------------------------------------------- 
# calendarheatmap Plotting function
# -------------------------------------------------------------------------- 
eda_calendarheatmap <- function(
  data, 
  date_column, 
  value_column, 
  aggr_func, 
  plot_filename=NULL, 
  ncolors = 99, 
  color = "r2g", 
  varname = "Values",
  date.form = "%Y-%m-%d", 
  color.reverse = F, 
  ...)
{
  require(lattice)
  require(grid)
  require(chron)
  
  args <- list(...)
  
  get_arg <- function(arg_name, args) {
    if (arg_name %in% names(args)) {
      arg <- args[[arg_name]]
      return(arg)
    } else {
      return(null)
    }
  }
  
  date_column_name <- as.list(substitute(date_column))[[1]]
  value_column_name <- as.list(substitute(value_column))[[1]]
  
  if (is.name(date_column_name)) {
    if (exists(as.character(date_column_name))) {
      date_column_name <- eval(date_column_name)
    } else {
      date_column_name <- as.character(date_column_name)
    }
  } else {
    date_column_name <- date_column
  }
  
  if (is.name(value_column_name)) {
    if (exists(as.character(value_column_name))) {
      value_column_name <- eval(value_column_name)
    } else {
      value_column_name <- as.character(value_column_name)
    }
  } else {
    value_column_name <- value_column
  }
  
  formula <- as.formula(paste(value_column_name, " ~ ", date_column_name, sep="", collapse=""))
  temp.df <- aggregate(as.formula(formula), data=data, aggr_func)
  colnames(temp.df) <- c("dates", "values")
  
  dates <- temp.df$dates
  values <- temp.df$values
  
  if (class(dates) == "integer") {
    dates <- as.character(dates)
  }
  
  if (class(dates) == "character" | class(dates) == "factor") {
    dates <- strptime(dates, date.form)
  }
  
  caldat <- data.frame(value = values, dates = dates)
  
  min.date <- as.Date(paste(format(min(dates), "%Y"), "-1-1", sep = ""))
  max.date <- as.Date(paste(format(max(dates), "%Y"), "-12-31", sep = ""))
  
  dates.f <- data.frame(date.seq = seq(min.date, max.date, by = "days"))
  caldat <- data.frame(date.seq = seq(min.date, max.date, by = "days"), value = NA)
  
  dates <- as.Date(dates)
  caldat$value[match(dates, caldat$date.seq)] <- values
  caldat$dotw <- as.numeric(format(caldat$date.seq, "%w"))
  caldat$woty <- as.numeric(format(caldat$date.seq, "%U")) + 1
  caldat$yr <- as.factor(format(caldat$date.seq, "%Y"))
  caldat$month <- as.numeric(format(caldat$date.seq, "%m"))
  
  yrs <- as.character(unique(caldat$yr))
  d.loc <- as.numeric()
  
  for (m in min(yrs):max(yrs)) {
    d.subset <- which(caldat$yr == m)
    sub.seq <- seq(1, length(d.subset))
    d.loc <- c(d.loc, sub.seq)
  }
  
  caldat <- cbind(caldat, seq=d.loc)
  
  r2b <- c("#0571B0", "#92C5DE", "#F7F7F7", "#F4A582", "#CA0020")
  r2g <- c("#D61818", "#FFAE63", "#FFFFBD", "#B5E384")
  w2b <- c("#045A8D", "#2B8CBE", "#74A9CF", "#BDC9E1", "#F1EEF6")
  
  colorset <- get(color)
  
  if (color.reverse == T) {
    colorset <- rev(colorset)
  }
  
  assign("col.sty", colorset)
  
  calendar.pal <- colorRampPalette((col.sty), space = "Lab")
  
  def.theme <- lattice.getOption("default.theme")
  
  cal.theme <- function() {
    theme <- list(strip.background = list(col = "transparent"),
                  strip.border = list(col = "transparent"), axis.line = list(col = "transparent"),
                  par.strip.text = list(cex = 0.8))
  }
  
  lattice.options(default.theme = cal.theme)
  yrs <- (unique(caldat$yr))
  nyr <- length(yrs)
  
  main.title <- ""
  main.title <- get_arg("main.title", args)
  
  cal.plot <- levelplot(value ~ woty * dotw | yr,
                        data = caldat,
                        as.table = TRUE,
                        aspect = 0.12,
                        layout = c(1, nyr%%8),
                        between = list(x = 0, y = c(1, 1)),
                        strip = TRUE,
                        main = main.title,
                        scales = list(x = list(at = c(seq(2.9, 52, by = 4.42)),
                                               labels = paste(1:12, "월", sep=""),
                                               alternating = c(1, rep(0, (nyr - 1))),
                                               tck = 0,
                                               cex = 0.7),
                                      y = list(at = c(0, 1, 2, 3, 4, 5, 6),
                                               #labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
                                               labels = c("일", "월", "화", "수", "목", "금", "토"),
                                               alternating = 1,
                                               cex = 0.6,
                                               tck = 0)),
                        xlim = c(0.4, 54.6),
                        ylim = c(6.6, -0.6),
                        cuts = ncolors - 1,
                        col.regions = (calendar.pal(ncolors)),
                        xlab = "",
                        ylab = "",
                        colorkey = list(col = calendar.pal(ncolors), width = 0.6, height = 0.5),
                        subscripts = TRUE)
  
  if (!is.null(plot_filename)) {
    png(plot_filename, width = 1400, height = 400)
  }
  
  print(cal.plot)
  
  panel.locs <- trellis.currentLayout()
  
  for (row in 1:nrow(panel.locs)) {
    for (column in 1:ncol(panel.locs)) {
      if (panel.locs[row, column] > 0) {
        trellis.focus("panel", row = row, column = column, highlight = FALSE)
        
        xyetc <- trellis.panelArgs()
        subs <- caldat[xyetc$subscripts, ]
        dates.fsubs <- caldat[caldat$yr == unique(subs$yr), ]
        y.start <- dates.fsubs$dotw[1]
        y.end <- dates.fsubs$dotw[nrow(dates.fsubs)]
        dates.len <- nrow(dates.fsubs)
        adj.start <- dates.fsubs$woty[1]
        
        for (k in 0:6) {
          if (k < y.start) {
            x.start <- adj.start + 0.5
          }
          else {
            x.start <- adj.start - 0.5
          }
          
          if (k > y.end) {
            x.finis <- dates.fsubs$woty[nrow(dates.fsubs)] - 0.5
          }
          else {
            x.finis <- dates.fsubs$woty[nrow(dates.fsubs)] + 0.5
          }
          
          grid.lines(x = c(x.start, x.finis),
                     y = c(k - 0.5, k - 0.5),
                     default.units = "native",
                     gp = gpar(col = "grey", lwd = 1))
        }
        
        if (adj.start < 2) {
          grid.lines(x = c(0.5, 0.5),
                     y = c(6.5, y.start - 0.5),
                     default.units = "native",
                     gp = gpar(col = "grey", lwd = 1))
          grid.lines(x = c(1.5, 1.5),
                     y = c(6.5, -0.5),
                     default.units = "native",
                     gp = gpar(col = "grey", lwd = 1))
          grid.lines(x = c(x.finis, x.finis),
                     y = c(dates.fsubs$dotw[dates.len] - 0.5, -0.5),
                     default.units = "native",
                     gp = gpar(col = "grey", lwd = 1))
          
          if (dates.fsubs$dotw[dates.len] != 6) {
            grid.lines(x = c(x.finis + 1, x.finis + 1),
                       y = c(dates.fsubs$dotw[dates.len] - 0.5, -0.5),
                       default.units = "native",
                       gp = gpar(col = "grey",
                                 lwd = 1))
          }
          
          grid.lines(x = c(x.finis, x.finis),
                     y = c(dates.fsubs$dotw[dates.len] - 0.5, -0.5),
                     default.units = "native",
                     gp = gpar(col = "grey",
                               lwd = 1))
        }
        
        for (n in 1:51) {
          grid.lines(x = c(n + 1.5, n + 1.5),
                     y = c(-0.5, 6.5),
                     default.units = "native",
                     gp = gpar(col = "grey", lwd = 1))
        }
        
        x.start <- adj.start - 0.5
        
        if (y.start > 0) {
          grid.lines(x = c(x.start, x.start + 1),
                     y = c(y.start - 0.5, y.start - 0.5),
                     default.units = "native",
                     gp = gpar(col = "black", lwd = 1.75))
          grid.lines(x = c(x.start + 1, x.start + 1),
                     y = c(y.start - 0.5, -0.5),
                     default.units = "native",
                     gp = gpar(col = "black", lwd = 1.75))
          grid.lines(x = c(x.start, x.start),
                     y = c(y.start - 0.5, 6.5),
                     default.units = "native",
                     gp = gpar(col = "black", lwd = 1.75))
          
          if (y.end < 6) {
            grid.lines(x = c(x.start + 1, x.finis + 1),
                       y = c(-0.5, -0.5),
                       default.units = "native",
                       gp = gpar(col = "black", lwd = 1.75))
            
            grid.lines(x = c(x.start, x.finis),
                       y = c(6.5, 6.5),
                       default.units = "native",
                       gp = gpar(col = "black", lwd = 1.75))
          }
          else {
            grid.lines(x = c(x.start + 1, x.finis),
                       y = c(-0.5, -0.5),
                       default.units = "native",
                       gp = gpar(col = "black", lwd = 1.75))
            
            grid.lines(x = c(x.start, x.finis),
                       y = c(6.5, 6.5),
                       default.units = "native",
                       gp = gpar(col = "black", lwd = 1.75))
          }
        }
        else {
          grid.lines(x = c(x.start, x.start),
                     y = c(-0.5, 6.5),
                     default.units = "native",
                     gp = gpar(col = "black", lwd = 1.75))
        }
        
        if (y.start == 0) {
          if (y.end < 6) {
            grid.lines(x = c(x.start, x.finis + 1),
                       y = c(-0.5, -0.5),
                       default.units = "native",
                       gp = gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.start, x.finis),
                       y = c(6.5, 6.5),
                       default.units = "native",
                       gp = gpar(col = "black", lwd = 1.75))
          }
          else {
            grid.lines(x = c(x.start + 1, x.finis),
                       y = c(-0.5, -0.5),
                       default.units = "native",
                       gp = gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.start, x.finis),
                       y = c(6.5, 6.5),
                       default.units = "native",
                       gp = gpar(col = "black", lwd = 1.75))
          }
        }
        
        for (j in 1:12) {
          last.month <- max(dates.fsubs$seq[dates.fsubs$month == j])
          x.last.m <- dates.fsubs$woty[last.month] + 0.5
          y.last.m <- dates.fsubs$dotw[last.month] + 0.5
          
          grid.lines(x = c(x.last.m, x.last.m),
                     y = c(-0.5, y.last.m),
                     default.units = "native",
                     gp = gpar(col = "black", lwd = 1.75))
          
          if ((y.last.m) < 6) {
            grid.lines(x = c(x.last.m, x.last.m - 1),
                       y = c(y.last.m, y.last.m), default.units = "native",
                       gp = gpar(col = "black", lwd = 1.75))
            grid.lines(x = c(x.last.m - 1, x.last.m - 1),
                       y = c(y.last.m, 6.5),
                       default.units = "native",
                       gp = gpar(col = "black", lwd = 1.75))
          }
          else {
            grid.lines(x = c(x.last.m, x.last.m), y = c(-0.5,
                                                        6.5), default.units = "native", gp = gpar(col = "black",
                                                                                                  lwd = 1.75))
          }
        }
      }
    }
    
    trellis.unfocus()
  }
  
  lattice.options(default.theme = def.theme)
  
  if (!is.null(plot_filename)) {
    dev.off()
  }
  rm(temp.df)
}
# --------------------------------------------------------------------------
# data load
# --------------------------------------------------------------------------
sample_data <- read.csv("C:/Users/Andrew/Desktop/Logs_2014_07_23_15_29.csv", header = T)
head(sample_data)

sample_data1 <- sample_data[, c(2, 4:12)]
sample_data1$rcd <- substring(sample_data$reportCreatedAt, 1, 10)
sample_data1$rud <- substring(sample_data$reportUpdatedAt, 1, 10)
sample_data1$rlcd <- substring(sample_data$reportLogCretatedAt, 1, 10)
sample_data1$rlud <- substring(sample_data$reportLogUpdatedAt, 1, 10)
head(sample_data1)

# --------------------------------------------------------------------------
# 선릉KT
# --------------------------------------------------------------------------
seonreung <- subset(sample_data1, placeName == "선릉KT")
seonreung <- seonreung[order(seonreung$facilitiesName, seonreung$facilitiesSubName, seonreung$categoryName, seonreung$measurementName),]
head(seonreung, 10)
names(seonreung)
dim(seonreung) # 3899

###########################################################################
# 각 항목별 (measurementMame별) 일별 시간축에 따른 추이 (일 평균)
# 항목 List: 총 305가지
###########################################################################
seonreung$device <- paste0(seonreung$facilitiesName, "_", seonreung$facilitiesSubName, "_", seonreung$categoryName, "_", seonreung$measurementName)
seonreung_m_name <- unique(seonreung$device)
seonreung_m_name_table <- as.data.frame(table(seonreung$device))
colnames(seonreung_m_name_table) <- c("device_name", "freq")

for(i in 1:length(seonreung_m_name)){
  seonreung_test_set <- subset(seonreung, device == seonreung_m_name[i])
  seonreung_m_name_table$total[i] <- nrow(seonreung_test_set)
  seonreung_m_name_table$null[i] <- sum(seonreung_test_set$value == "") + sum(seonreung_test_set$value == " ") + sum(seonreung_test_set$value == "   ")
  seonreung_m_name_table$null_p[i] <- round(seonreung_m_name_table$null[i]/seonreung_m_name_table$total[i]*100, 2)
}

head(seonreung_m_name_table)
tail(seonreung_m_name_table)

###########################################################################
# 결측치 70%이상, 데이터 수 10개 이하 제외
# 분석 가능한 device: 총 58개 (전체 305개 중)
###########################################################################
seonreung_use_device_temp1 <- subset(seonreung_m_name_table, null_p <= 30)
seonreung_use_device_temp2 <- subset(seonreung_use_device_temp1, total > 10)
seonreung_use_device_name <- seonreung_use_device_temp2$device_name # 58

seonreung_use_device_data <- seonreung_use_device_temp2

###########################################################################
# 일별 평균 및 Trend Time Series Chart
# 일별 평균: seonreung_device_day_mean_list
###########################################################################
seonreung_device_day_mean_list <- c()

for(i in 1:length(seonreung_use_device_name)){
  seonreung_device_temp <- as.data.frame(subset(seonreung, device == seonreung_use_device_name[i]))
  seonreung_device_temp$value <- as.numeric(as.character(seonreung_device_temp$value))
  if(sum(is.na(seonreung_device_temp$value)) != length(seonreung_device_temp$value)){
    seonreung_device_day_mean <- aggregate(value ~ rcd, data = seonreung_device_temp, FUN = mean)
    colnames(seonreung_device_day_mean) <- c("day", "mean")
    
    seonreung_device_day_mean_list[[i]] <- list(device_name = as.character(seonreung_use_device_name[i]), day_mean = seonreung_device_day_mean)
    
    seonreung_plot_title <- paste("Time Plot (by Day):", seonreung_use_device_name[i])
    
    seonreung_time_chart <- ggplot(data = seonreung_device_day_mean, aes(x = as.Date(day), y = mean))
    seonreung_time_chart <- seonreung_time_chart + geom_line()
    seonreung_time_chart <- seonreung_time_chart + xlab("Date") + ylab("Mean")
    seonreung_time_chart <- seonreung_time_chart + labs(title = seonreung_plot_title)
    seonreung_time_chart <- seonreung_time_chart + theme(axis.text.x = element_text(angle=70, vjust=0.5), plot.title = element_text(lineheight=.8, face="bold"))
    print(seonreung_time_chart)
  }
}

###########################################################################
# 각 항목별 Calendar heatmap
###########################################################################
for(i in 1:length(seonreung_use_device_name)){
  seonreung_device_temp <- as.data.frame(subset(seonreung, device == seonreung_use_device_name[i]))
  seonreung_device_temp$value <- as.numeric(as.character(seonreung_device_temp$value))
  if(sum(is.na(seonreung_device_temp$value)) != length(seonreung_device_temp$value)){
    seonreung_plot_title <- paste("Calendar heatmap:", seonreung_use_device_name[i])
    eda_calendarheatmap(seonreung_device_day_mean_list[[i]]$day_mean, "day", "mean", sum, date.form="%Y-%m-%d", main.title=seonreung_plot_title, color.reverse=T)  
  }
}






# --------------------------------------------------------------------------
# 우면센터
# --------------------------------------------------------------------------
umyeon <- subset(sample_data1, placeName == "우면센터")
umyeon <- umyeon[order(umyeon$facilitiesName, umyeon$facilitiesSubName, umyeon$categoryName, umyeon$measurementName),]

###########################################################################
# 각 항목별 (measurementMame별) 일별 시간축에 따른 추이 (일 평균)
# 항목 List: 총 101가지
###########################################################################
umyeon$device <- paste0(umyeon$facilitiesName, "_", umyeon$facilitiesSubName, "_", umyeon$categoryName, "_", umyeon$measurementName)
umyeon_m_name <- unique(umyeon$device)
umyeon_m_name_table <- as.data.frame(table(umyeon$device))
colnames(umyeon_m_name_table) <- c("device_name", "freq")

for(i in 1:length(umyeon_m_name)){
  umyeon_test_set <- subset(umyeon, device == umyeon_m_name[i])
  umyeon_m_name_table$total[i] <- nrow(umyeon_test_set)
  umyeon_m_name_table$null[i] <- sum(umyeon_test_set$value == "") + sum(umyeon_test_set$value == " ") + sum(umyeon_test_set$value == "   ")
  umyeon_m_name_table$null_p[i] <- round(umyeon_m_name_table$null[i]/umyeon_m_name_table$total[i]*100, 2)
}

###########################################################################
# 결측치 70%이상, 데이터 수 10개 이하 제외
# 분석 가능한 device: 총 22개 (전체 101개 중)
###########################################################################
umyeon_use_device_temp1 <- subset(umyeon_m_name_table, null_p <= 30)
umyeon_use_device_temp2 <- subset(umyeon_use_device_temp1, total > 10)
umyeon_use_device_name <- umyeon_use_device_temp2$device_name # 22

umyeon_use_device_data <- umyeon_use_device_temp2

###########################################################################
# 일별 평균 및 Trend Time Series Chart
# 일별 평균: umyeon_device_day_mean_list
###########################################################################
umyeon_device_day_mean_list <- c()

for(i in 1:length(umyeon_use_device_name)){
  umyeon_device_temp <- as.data.frame(subset(umyeon, device == umyeon_use_device_name[i]))
  umyeon_device_temp$value <- as.numeric(as.character(umyeon_device_temp$value))
  if(sum(is.na(umyeon_device_temp$value)) != length(umyeon_device_temp$value)){
    umyeon_device_day_mean <- aggregate(value ~ rcd, data = umyeon_device_temp, FUN = mean)
    colnames(umyeon_device_day_mean) <- c("day", "mean")
    
    umyeon_device_day_mean_list[[i]] <- list(device_name = as.character(umyeon_use_device_name[i]), day_mean = umyeon_device_day_mean)
    
    umyeon_plot_title <- paste("Time Plot (by Day):", umyeon_use_device_name[i])
    
    umyeon_time_chart <- ggplot(data = umyeon_device_day_mean, aes(x = as.Date(day), y = mean))
    umyeon_time_chart <- umyeon_time_chart + geom_line()
    umyeon_time_chart <- umyeon_time_chart + xlab("Date") + ylab("Mean")
    umyeon_time_chart <- umyeon_time_chart + labs(title = umyeon_plot_title)
    umyeon_time_chart <- umyeon_time_chart + theme(axis.text.x = element_text(angle=70, vjust=0.5), plot.title = element_text(lineheight=.8, face="bold"))
    print(umyeon_time_chart)    
  }  
}

###########################################################################
# 각 항목별 Calendar heatmap
###########################################################################
for(i in 1:length(umyeon_use_device_name)){
  umyeon_device_temp <- as.data.frame(subset(umyeon, device == umyeon_use_device_name[i]))
  umyeon_device_temp$value <- as.numeric(as.character(umyeon_device_temp$value))
  if(sum(is.na(umyeon_device_temp$value)) != length(umyeon_device_temp$value)){
    umyeon_plot_title <- paste("Calendar heatmap:", umyeon_use_device_name[i])
    eda_calendarheatmap(umyeon_device_day_mean_list[[i]]$day_mean, "day", "mean", sum, date.form="%Y-%m-%d", main.title=umyeon_plot_title, color.reverse=T)  
  }
}
