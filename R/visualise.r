library(ggplot2)
library(scales)    # Para formatos de fecha y hora en ggplot2
library(lubridate) # Para manipulación de fechas

# Configuración global de tema
paper_theme <- theme_minimal() +
  theme(
    axis.text.x = element_text(color = "black", face = "plain", angle = 0, hjust = 0.5, vjust = 0.5, size = 12, margin = margin(t = 5, b = 5)),
    axis.text.x.top = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "plain", color = "black", margin = margin(r = 5, l = 5)),
    axis.title = element_text(size = 14, face = "plain"),
    axis.ticks = element_line(size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.5)
  )

# Función para calcular los tamaños de los elementos gráficos
calculate_sizes1 <- function(n) {
  sizes <- list()
  if (n <= 24) {
    sizes$line = 0.75
    sizes$external_point = 4.5
    sizes$internal_point = 3
    sizes$num_xticks = 2
    sizes$days = 5
  } else if (n <= 24 * 4) {
    sizes$line = 0.5
    sizes$external_point = 3.5
    sizes$internal_point = 2
    sizes$num_xticks = 4
    sizes$days = 5
  } else if (n <= 24 * 7) {
    sizes$line = 0.25
    sizes$external_point = 1
    sizes$internal_point = 0
    sizes$num_xticks = 2
    sizes$days = 4
  } else if (n <= 24 * 30) {
    sizes$line = 0.25
    sizes$external_point = 1
    sizes$internal_point = 0
    sizes$num_xticks = 1
  } else if (n <= 24 * 30 * 3) {
    sizes$line = 0.25
    sizes$external_point = 0
    sizes$internal_point = 0
    sizes$num_xticks = 7
  } else {
    sizes$line = 0.25
    sizes$external_point = 0
    sizes$internal_point = 0
    sizes$num_xticks = 15
  }
  return(sizes)
}

calculate_sizes <- function(n) {
  # Definir una base para la escala de tamaños
  base_sizes <- list(
    line = 0.25,  # Tamaño base para la línea
    external_point = 1,  # Tamaño base para los puntos externos
    internal_point = 0,  # Tamaño base para los puntos internos
    num_xticks = 1,  # Cantidad de etiquetas en el eje X
    days = 4  # Tamaño de las etiquetas de días
  )

  # Ajustar tamaños basados en `n` (número de observaciones)
  scale_factor <- min(1, n / (24 * 30))  # Factor de escala que depende de la cantidad de datos

  sizes <- list(
    line = base_sizes$line * scale_factor,
    external_point = base_sizes$external_point * scale_factor,
    internal_point = base_sizes$internal_point * scale_factor,
    num_xticks = max(1, round(n / 24)),  # Un número mínimo de ticks para grandes conjuntos de datos
    days = max(2, base_sizes$days * scale_factor)  # Asegurarse de que el tamaño de los días no sea demasiado pequeño
  )

  return(sizes)
}


# Función para crear un gráfico con las líneas y puntos especificados
create_plot <- function(df, sizes, title, big_size_chart) {
  g <- ggplot(df, aes(x = date_time, y = metric)) +
    geom_line(color = "black", linewidth = sizes$line) +
    labs(title = title, x = '', y = 'RH %') +
    scale_y_continuous(limits = c(min(df$metric, na.rm = TRUE), max(df$metric, na.rm = TRUE))) +
    theme_minimal()
  
  # Agregar puntos y señales
  g <- g + geom_point(shape = 21, size = sizes$external_point, color = "white", fill = "white") +
           geom_point(shape = 21, size = sizes$internal_point, stroke = 1, color = "black", fill = "white") +
           geom_point(data = df[df$signals == 1, ], aes(x = date_time, y = metric), size = 3, pch = 4, col = "red") +
           geom_point(data = df[df$signals == 1, ], aes(x = date_time, y = metric), size = 2, pch = 16, col = "#FF000055")
  
  return(g)
}

add_limit_lines <- function(g, UL, LL, mean) {
  if (any(!is.na(UL))) {
    g <- g + geom_line(aes(y = UL), color = "red", linewidth = 1, linetype = "dashed")
  }
  if (any(!is.na(LL))) {
    g <- g + geom_line(aes(y = LL), color = "red", linewidth = 1, linetype = "dashed")
  }
  if (any(!is.na(mean))) {
    g <- g + geom_line(aes(y = mean), color = "blue", linewidth = 1, linetype = "dashed")
  }
  
  return(g)
}

configure_x_axis_big <- function(g, df, sizes) {
  multiples_breaks <- function(x) {
    from <- floor_date(min(x), "day")
    to <- ceiling_date(max(x), "day")
    all_dates <- seq(from, to, by = "day")
    multiples <- all_dates[day(all_dates) %% sizes$num_xticks == 0]
    return(multiples)
  }
  
  g <- g + scale_x_datetime(breaks = multiples_breaks, date_labels = "%d") +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
  
  unique_months <- unique(format(df$date_time, "%Y-%m-01"))
  unique_months <- unique_months[-1] # Eliminar el primer mes
  for (month in unique_months) {
    month_start <- as.POSIXct(month)
    g <- g + annotate("text", x = month_start, y = -Inf, label = format(month_start, "%b"),
                      vjust = -0.4, size = 5, color = "black")
  }
  
  return(g)
}

configure_x_axis_small <- function(g, df, sizes) {
  total_date_times <- length(unique(df$date_time))
  interval_hours <- max(floor(total_date_times / 12), 1)
  g <- g + scale_x_datetime(date_breaks = paste(interval_hours, "hour"), date_labels = "%H") +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
  
  unique_days <- unique(as.Date(df$date_time))
  unique_days <- unique_days[-1] # Eliminar el primer día
  for (day in unique_days) {
    day <- as.POSIXct(paste(as.Date(day), "00:00:00"))
    g <- g + annotate("text", x = day, y = -Inf, label = format(day, "%b %d"),
                      vjust = -0.4, size = sizes$days, color = "black")
  }
  
  return(g)
}

plot_graph <- function(metric, date_time, mean, UL, LL, signals, width_ratio = 7, height_ratio = 4,
                       path = NULL, file_name = "plot_graph.pdf", title = "Chart") {
  
  df <- data.frame(
    date_time = as.POSIXct(date_time, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"),
    metric = metric,
    mean = ifelse(is.na(mean), rep(mean, length(date_time)), mean),
    UL = ifelse(is.na(UL), rep(UL, length(date_time)), UL),
    LL = ifelse(is.na(LL), rep(LL, length(date_time)), LL),
    signals = signals
  )
  
  sizes <- calculate_sizes(nrow(df))
  big_size_chart <- (nrow(df) > 24 * 30)
  
  g <- create_plot(df, sizes, title, big_size_chart)
  g <- add_limit_lines(g, df$UL, df$LL, df$mean)
  
  if (big_size_chart) {
    g <- configure_x_axis_big(g, df, sizes)
  } else {
    g <- configure_x_axis_small(g, df, sizes)
  }

  if (is.null(path)) {
    print(g)
  } else {
    ggsave(paste(path, file_name, sep = "/"), plot = g,
           width = width_ratio, height = height_ratio, units = "in")
  }

  return(g)
}