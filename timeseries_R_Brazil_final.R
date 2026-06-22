# clear memory
rm(list = ls())
Sys.setlocale("LC_TIME", "C")

# main data
library(readxl)
data_temporal <- read_excel("Procellariiformes_Brazil_LauraBaes et al. 2026.xlsx")


#### summary script ####
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(plyr)
library(dplyr)

# data for Brazilian map and coastline
brasil <- ne_countries(scale = "small", country = "Brazil", returnclass = "sf")
coast_110 <- ne_coastline(scale = "small", returnclass = "sf")

# calling brazilian states for further graphical images during the analysis
estados <- ne_states(country = "Brazil", returnclass = "sf")
regioes_df <- data.frame(
  name = c("Rio Grande do Sul","Santa Catarina","Paraná",
           "São Paulo","Rio de Janeiro","Espírito Santo","Minas Gerais",
           "Bahia","Sergipe","Alagoas","Pernambuco","Paraíba",
           "Rio Grande do Norte","Ceará","Piauí","Maranhão"),
  
  regiao = c(rep("Sul",3),
             rep("Sudeste",4),
             rep("Nordeste",9))
)

estados_reg <- estados  %>%
  left_join(regioes_df, by = "name") %>%
  filter(!is.na(regiao))

regioes_sf <- estados_reg %>%
  group_by(regiao) %>%
  summarise(geometry = st_union(geometry))

# creating standardized themes
library(ggplot2)

# temas stl
tema_stl <- theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, size = 11, face = "plain"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "black", size = 0.5),
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 10),
    plot.margin = margin(t = 5, r = 10, b = 5, l = 10),
    axis.text.x = element_blank(),
    axis.title.x = element_blank()
  )

tema_stl_ultimo <- tema_stl +
  theme(
    axis.text.x = element_text(size = 9),
    axis.title.x = element_text(size = 10)
  )

#### summary epidemiology ####

data_temporal$Female <- ifelse(data_temporal$Sex == "female", 1, 0)
data_temporal$Male   <- ifelse(data_temporal$Sex == "male", 1, 0)

data_temporal$Juvenile <- ifelse(data_temporal$Stage == "juvenile", 1, 0)
data_temporal$Adult    <- ifelse(data_temporal$Stage == "adult", 1, 0)

data_temporal$Alive <- ifelse(data_temporal$Animal == "alive", 1, 0)
data_temporal$Dead  <- ifelse(data_temporal$Animal == "dead", 1, 0)

summary1 <- ddply(data_temporal, c("Family", "Species"), summarise,
                  n = length(Species),
                  fem = sum(Female, na.rm = TRUE),
                  male = sum(Male, na.rm = TRUE),
                  juvenile = sum(Juvenile, na.rm = TRUE),
                  adult = sum(Adult, na.rm = TRUE),
                  alive = sum(Alive, na.rm = TRUE),
                  dead = sum(Dead, na.rm = TRUE))

summary1$SexNA <- summary1$n - summary1$fem - summary1$male
summary1$StageNA <- summary1$n - summary1$adult - summary1$juvenile

summary1$sex<- paste(summary1$fem, summary1$male, summary1$SexNA, sep = "/")
summary1$stage<- paste(summary1$juvenile, summary1$adult, summary1$StageNA, sep = "/")
summary1$condition <- paste(summary1$alive, summary1$dead, sep="/")

tot_n        <- sum(summary1$n)
tot_fem      <- sum(summary1$fem)
tot_male     <- sum(summary1$male)
tot_SexNA    <- sum(summary1$SexNA)

tot_juvenile <- sum(summary1$juvenile)
tot_adult    <- sum(summary1$adult)
tot_StageNA  <- sum(summary1$StageNA)

tot_alive    <- sum(summary1$alive)
tot_dead     <- sum(summary1$dead)

linha_total <- data.frame(
  Family = "Total",
  Species = "",
  n = tot_n,
  sex = paste0("(", tot_fem, "/", tot_male, "/", tot_SexNA, ")"),
  stage = paste0("(", tot_juvenile, "/", tot_adult, "/", tot_StageNA, ")"),
  condition = paste0("(", tot_alive, "/", tot_dead, ")")
)

summary1_clean <- summary1[, !(names(summary1) %in% 
                                 c("fem", "male", "juvenile", "adult",
                                   "alive", "dead", "SexNA", "StageNA"))]

#### temporal analysis - all species - Brazil #####

data_month <- ddply(data_temporal, c("Year", "Month"), summarise, 
                    n = sum(n))

attach(data_month)

serie <- ts(n,start=c(2017,1),frequency=12) # transforming data into a temporal series

plot(serie)

up <- stl(serie,"periodic")
plot(up)
summary(up)

seasonal <- up$time.series[, "seasonal"] 
seasonal # seasonal values

# finding outliers from the remainder
remainder <- up$time.series[, "remainder"] 

# IQR Method
Q1 <- quantile(remainder, 0.25, na.rm = TRUE)
Q3 <- quantile(remainder, 0.75, na.rm = TRUE)
IQR_valor <- Q3 - Q1

# different thresholds criteria
lim_1   <- Q3 + (1   * IQR_valor)
lim_15  <- Q3 + (1.5 * IQR_valor)
lim_3   <- Q3 + (3   * IQR_valor)

# thresholds index
out_1   <- which(remainder > lim_1)
out_15  <- which(remainder > lim_15)
out_3   <- which(remainder > lim_3)

df_outliers_all <- rbind(
  data.frame(tempo = time(serie)[out_1],
             remainder = remainder[out_1],
             criterio = "1xIQR"),
  
  data.frame(tempo = time(serie)[out_15],
             remainder = remainder[out_15],
             criterio = "1.5xIQR"),
  
  data.frame(tempo = time(serie)[out_3],
             remainder = remainder[out_3],
             criterio = "3xIQR")
)

df_outliers_all <- df_outliers_all %>%
  mutate(
    ano  = floor(tempo),
    mes  = round((tempo - ano) * 12) + 1,
    mes  = ifelse(mes == 13, 12, mes)
  )

# cheack all outliers detected from the IQR methods for further analysis
df_outliers_all_2 <- df_outliers_all[, !(names(df_outliers_all) %in% 
                                 c("tempo"))]
df_outliers_all_2

# creating final graphic for temporal analysis
dados_originais <- as.numeric(serie)
componente_sazonal <- as.numeric(up$time.series[,"seasonal"])
componente_tendencia <- as.numeric(up$time.series[,"trend"])
componente_residual <- as.numeric(up$time.series[,"remainder"])

tempo <- as.numeric(time(serie))

df_decomp <- data.frame(
  tempo = tempo,
  data = dados_originais,
  seasonal = componente_sazonal,
  trend = componente_tendencia,
  remainder = componente_residual
)

y_max <- max(df_decomp$remainder, na.rm = TRUE)
y_min <- min(df_decomp$remainder, na.rm = TRUE)

df_outliers_all$nivel <- ifelse(df_outliers_all$criterio == "1xIQR",  y_max + 40,
                                ifelse(df_outliers_all$criterio == "1.5xIQR", y_max + 100,
                                       y_max + 160))

# graphic 1 original data
p1 <- ggplot(df_decomp, aes(x = tempo, y = data)) +
  geom_line(color = "black", size = 1) +
  labs(title = "", x = "", y = "data") +
  tema_stl +
  scale_x_continuous(breaks = seq(2017, 2024, 0.7)) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 11))

# graphic 2: seasonal component
p2 <- ggplot(df_decomp, aes(x = tempo, y = seasonal)) +
  geom_line(color = "blue", size = 0.7) +
  labs(title = "", x = "", y = "seasonal") +
  tema_stl +
  scale_x_continuous(breaks = seq(2017, 2024, 1)) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 11))

# graphic 3: temporal trend
p3 <- ggplot(df_decomp, aes(x = tempo, y = trend)) +
  geom_line(color = "darkgreen", size = 0.7) +
  labs(title = "", x = "", y = "trend") +
  tema_stl +
  scale_x_continuous(breaks = seq(2017, 2024, 1)) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 11))

# Ggraphical 4: reaminder component
p4 <- ggplot(df_decomp, aes(x = tempo, y = remainder)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  geom_segment(aes(x = tempo, xend = tempo, y = 0, yend = remainder), 
               color = "red", size = 1) +
  geom_point(data = df_outliers_all,
             aes(x = tempo,
                 y = nivel,
                 color = criterio),
             size = 2) +
  scale_color_manual(values = c("1xIQR" = "#FFB000",
                                "1.5xIQR" = "#377EB8",
                                "3xIQR" = "#CC79A7"),
                     breaks = c("3xIQR", "1.5xIQR", "1xIQR"),
                     labels = c("3 x IQR",
                                "1.5 x IQR",
                                "1 x IQR")) +
  coord_cartesian(ylim = c(y_min, y_max + 160)) +
  labs(x = "time", y = "remainder") +
  tema_stl_ultimo +
  scale_x_continuous(breaks = seq(2017, 2024, 1)) +
  theme(
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.3, "cm")
  ) +
  guides(color = guide_legend(override.aes = list(size = 2)))

library(gridExtra)
stl_Brazil <- grid.arrange(p1, p2, p3, p4, ncol = 1, heights = c(1, 1, 1, 1))

# testing temporal trend
library(lme4)

length(n)
index <- 1:96
time <- index/12
yr <- factor(Year)
tapply(n,yr, mean)

model2 <- lmer(n~index+sin(time*2*pi)+cos(time*2*pi)+(1|factor(yr)), REML=FALSE)
model3 <- lmer(n~sin(time*2*pi)+cos(time*2*pi)+(1|factor(yr)), REML=FALSE)

anova(model2,model3)

#### Deposition rates for each month ####
library(tidyr)
library(lubridate)

# creating a dataset with all sampling days including days without a seabird stranding
ocorrencias_por_dia_all <- data_temporal %>%
  group_by(Date) %>%
  summarise(ocorrencias = n())

todas_datas_all <- data.frame(Date = seq(as.Date("2017-01-01"), as.Date("2024-12-31"), by = "day"))

ocorrencias_por_dia_all <- todas_datas_all %>%
  left_join(ocorrencias_por_dia_all, by = "Date") %>%
  mutate(ocorrencias = replace_na(ocorrencias, 0))

# creating exploratory parameters for deposition rates
medias_mensais <- ocorrencias_por_dia_all %>%
  mutate(month = month(Date)) %>%   # extrair o mês (1 a 12)
  group_by(month) %>%
  summarise(
    diary_deposition_rate = mean(ocorrencias, na.rm = TRUE),
    sd = sd(ocorrencias, na.rm = TRUE),
    mean_2SD = mean(ocorrencias, na.rm = TRUE) + 2*sd(ocorrencias, na.rm = TRUE)
  ) %>%
  arrange(month)

medias_mensais

seasonal_values <- as.numeric(seasonal[1:12])

seasonal_tibble <- tibble(
  month = 1:12,
  seasonality = seasonal_values
)

seasonal_tibble

merged_tibble <- left_join(seasonal_tibble,medias_mensais, by = "month")

merged_tibble

#### changepoint function ####
library(changepoint)

analise_changepoint <- function(data, ano, mes, medias_mensais) {
  
  # selecting a specific year and month for analysis
  dados_filtrados <- data %>%
    filter(Year == ano, Month == mes)
  
  # assuring that the data is in a date format
  dados_filtrados$Date <- as.Date(dados_filtrados$Date)
  
  # counting occurrences per day
  ocorrencias_por_dia <- dados_filtrados %>%
    group_by(Date) %>%
    summarise(ocorrencias = n(), .groups = "drop")
  
  # creating a sequence of datas for the whole month
  todas_datas <- data.frame(
    Date = seq(
      as.Date(paste0(ano, "-", mes, "-01")),
      as.Date(paste0(ano, "-", mes, "-", days_in_month(as.Date(paste0(ano, "-", mes, "-01"))))),
      by = "day"
    )
  )
  
  # completing with zeros when there is no occurrence 
  ocorrencias_por_dia <- todas_datas %>%
    left_join(ocorrencias_por_dia, by = "Date") %>%
    mutate(ocorrencias = replace_na(ocorrencias, 0))
  
  # salving objects in  global environment
  assign("dados_filtrados", dados_filtrados, envir = .GlobalEnv)
  assign("ocorrencias_por_dia", ocorrencias_por_dia, envir = .GlobalEnv)
  
  # changepoint analysis
  cpt <- cpt.meanvar(ocorrencias_por_dia$ocorrencias, method = "PELT", penalty = "BIC")
  
  # extracting thresholds from deposition rates
  diaria_mes <- medias_mensais %>%
    filter(month == mes) %>%
    pull(diary_deposition_rate)
  
  mean2sd_mes <- medias_mensais %>%
    filter(month == mes) %>%
    pull(mean_2SD)
  
  #graphics
  library(ggplot2)
  
  dias <- 1:length(ocorrencias_por_dia$ocorrencias)
  
  seg_means <- rep(param.est(cpt)$mean, diff(c(0, cpts(cpt), length(dias))))
  
  df_plot <- data.frame(
    dia = dias,
    ocorrencias = ocorrencias_por_dia$ocorrencias,
    media_segmento = seg_means
  )
  
  df_cpts <- data.frame(cp = cpts(cpt))
  
  p <- ggplot(df_plot, aes(x = dia, y = ocorrencias)) +
    
    geom_line(color = "black", linewidth = 0.6) +
    
    # Segment mean
    geom_step(aes(y = media_segmento),
              color = "red",
              linewidth = 0.7,
              show.legend = FALSE) +
    
    # Daily mean
    geom_hline(aes(yintercept = diaria_mes,
                   color = "Daily mean",
                   linetype = "Daily mean"),
               linewidth = 0.7) +
    
    # Mean + 2SD
    geom_hline(aes(yintercept = mean2sd_mes,
                   color = "Mean + 2SD",
                   linetype = "Mean + 2SD"),
               linewidth = 0.7) +
  
    geom_vline(data = df_cpts,
               aes(xintercept = cp,
                   color = "Changepoint",
                   linetype = "Changepoint"),
               linewidth = 0.7) +
    
    scale_color_manual(
      name = NULL,
      values = c("Segment mean" = "red",
                 "Daily mean" = "red",
                 "Mean + 2SD" = "blue",
                 "Changepoint" = "darkgreen")
    ) +
  
    scale_linetype_manual(
      name = NULL,
      values = c("Segment mean" = "solid",
                 "Daily mean" = "dashed",
                 "Mean + 2SD" = "dashed",
                 "Changepoint" = "dotted")
    ) +
    
    labs(x = NULL,
         y = NULL,
         title = paste(month.name[mes], ano)) +
    
    scale_x_continuous(
      breaks = seq(0, 32, by = 5)
    ) +
    
    theme_classic(base_size = 10) +
    
    theme(
      panel.border = element_rect(color = "black", fill = NA),
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        size = 16
      ),
      legend.position = "right",
      legend.justification = "top",
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.text = element_text(size = 8),
      
      axis.title = element_text(size = 11),
      axis.text  = element_text(size = 9)
    )
  
  print(p)
  
  # === CHangepoijtn segments ===
  changepoints <- cpts(cpt)
  
  # number of total observations
  n_obs <- length(ocorrencias_por_dia$ocorrencias)
  
  # start and end of each detected segment
  start_indices <- c(1, changepoints + 1)
  end_indices   <- c(changepoints, n_obs)
  lengths <- end_indices - start_indices + 1
  
  datas <- ocorrencias_por_dia$Date
  
  segmentos <- data.frame(
    Inicio = datas[start_indices],
    Fim    = datas[end_indices],
    Tamanho = lengths
  )
  
  print(p)
  print(segmentos)
  return(p)
}

#### creating a linearized coastline for Brazil ####

library(lwgeom)

data_temporal <- data_temporal%>%
  filter(!is.na(Long),
         !is.na(Lat))

# converting to a spacial object (WGS84).
data_all_sf <- st_as_sf(data_temporal,
                        coords = c("Long", "Lat"),
                        crs = 4326)

# transforming to UTM (SIRGAS 2000 / Brazil Albers Equal Area).
data_all_utm <- st_transform(data_all_sf, 5880)

brasil_utm <- st_transform(brasil, 5880)
coast_br <- st_boundary(brasil_utm)
coast_lines <- st_cast(coast_br, "LINESTRING")

lengths <- st_length(coast_lines)

full_coast <- coast_lines[which.max(lengths), ]

start_ll <- st_transform(st_startpoint(full_coast), 4326)
end_ll   <- st_transform(st_endpoint(full_coast), 4326)

lat_start <- st_coordinates(start_ll)[2]
lat_end   <- st_coordinates(end_ll)[2]

if(lat_start < lat_end){
  full_coast <- st_reverse(full_coast)
}

# projecting all strandings into the coastline
loc_all <- st_line_project(
  st_geometry(full_coast),
  st_geometry(data_all_utm)
)

dist_full_km <- as.numeric(loc_all) / 1000

min_km <- min(dist_full_km, na.rm = TRUE)
max_km <- max(dist_full_km, na.rm = TRUE)

min_km
max_km

total_length_km <- as.numeric(st_length(full_coast)) / 1000

start_prop <- min_km / total_length_km
end_prop   <- max_km / total_length_km

main_coast_dom <- st_linesubstring(
  full_coast,
  from = start_prop,
  to   = end_prop
)

loc_dom <- st_line_project(
  st_geometry(main_coast_dom),
  st_geometry(data_all_utm)
)

data_temporal$dist_km_dom <- as.numeric(loc_dom) / 1000

# transforming to latitude/longitude
main_coast_dom_ll <- st_transform(main_coast_dom, 4326)

data_all_ll <- st_transform(data_all_utm, 4326)

start_dom_ll <- st_transform(st_startpoint(main_coast_dom), 4326)
end_dom_ll   <- st_transform(st_endpoint(main_coast_dom), 4326)

ggplot() +
  geom_sf(data = brasil, fill = "gray95", color = "black") +
  geom_sf(data = main_coast_dom_ll, color = "blue", linewidth = 1) +
  geom_sf(data = data_all_ll, color = "red", size = 0.6, alpha = 0.6) +
  geom_sf(data = start_dom_ll, color = "green", size = 3) +
  geom_sf(data = end_dom_ll, color = "purple", size = 3) +
  theme_minimal()


#### isopleth function for hotspots ####

calc_isopleth_metrics <- function(dens_obj, evento_df, coast_line, level, event_name){
  
  dx <- diff(dens_obj$x)[1]
  mass <- dens_obj$y * dx
  
  ord <- order(dens_obj$y, decreasing = TRUE)
  cum_mass <- cumsum(mass[ord])
  
  idx <- ord[cum_mass <= level]
  
  x_iso <- dens_obj$x[idx]
  
  min_km <- min(x_iso)
  max_km <- max(x_iso)
  extent_km <- max_km - min_km
  
  # number of birds
  n_aves <- evento_df %>%
    dplyr::filter(dist_km_dom >= min_km &
                    dist_km_dom <= max_km) %>%
    nrow()
  
  aves_por_km <- n_aves / extent_km
  
  line_sfc <- st_geometry(coast_line)
  
  total_length <- as.numeric(st_length(line_sfc))
  
  prop_min <- (min_km * 1000) / total_length
  prop_max <- (max_km * 1000) / total_length
  
  segment <- lwgeom::st_linesubstring(
    line_sfc,
    from = prop_min,
    to   = prop_max
  )
  
  pt_min <- lwgeom::st_startpoint(segment)
  pt_max <- lwgeom::st_endpoint(segment)
  
  pts_sfc <- st_sfc(pt_min[[1]], pt_max[[1]], crs = st_crs(coast_line))
  pts_ll  <- st_transform(pts_sfc, 4326)
  
  coords <- st_coordinates(pts_ll)
  
  return(data.frame(
    event = event_name,
    isopleth = level * 100,
    km_min = min_km,
    km_max = max_km,
    extent_km = extent_km,
    n_aves = n_aves,
    aves_por_km = aves_por_km,
    long_min = coords[1,1],
    lat_min = coords[1,2],
    long_max = coords[2,1],
    lat_max = coords[2,2]
  ))
}

#### kernel density function for heatmap plot ####

calc_kde_coast <- function(evento, coast_line){
  
  evento_sf <- st_as_sf(
    evento,
    coords = c("Long", "Lat"),
    crs = 4326
  )
  
  evento_utm <- st_transform(evento_sf, 5880)
  
  loc_dom <- st_line_project(
    st_geometry(coast_line),
    st_geometry(evento_utm)
  )
  
  evento$dist_km_dom <- as.numeric(loc_dom) / 1000
  
  total_length_km <- as.numeric(
    st_length(st_geometry(coast_line))
  ) / 1000
  
  # kernel density
  dens <- density(evento$dist_km_dom, bw = 50)
  
  prop <- dens$x / total_length_km
  prop[prop < 0] <- 0
  prop[prop > 1] <- 1
  
  pts_line <- st_line_sample(
    st_geometry(coast_line),
    sample = prop
  )
  
  pts_line <- st_cast(pts_line, "POINT")
  
  pts_sf <- st_as_sf(
    data.frame(dens = dens$y),
    geometry = pts_line
  )
  
  pts_ll <- st_transform(pts_sf, 4326)
  
  return(list(
    pts_ll = pts_ll,
    dens = dens,
    evento = evento
  ))
}

#### IQR dropdowns ####
library(ggtext)
titulo_IQR1 <- "<span style='color:#FFB000'>●</span>"
titulo_IQR15 <- paste0(
  "<span style='color:#FFB000'>●</span> ",
  "<span style='color:#377EB8'>●</span>"
)
titulo_IQR3 <- paste0(
  "<span style='color:#FFB000'>●</span> ",
  "<span style='color:#377EB8'>●</span> ",
  "<span style='color:#CC79A7'>●</span>"
)
#### starting the analysis for each remainder outlier ####
#### Brazil - 2017-06 - MME 1 ####

p <- analise_changepoint(data_temporal, ano = 2017, mes = 6, medias_mensais)

#selecting only the period correspondig for the mortality peak for graphical purpose and further kernel density analysis
segmentos_destacados <- data.frame(
  xmin = as.Date(c("2017-06-07")),
  xmax = as.Date(c("2017-06-15"))
)

xmin_idx <- which(ocorrencias_por_dia$Date == segmentos_destacados$xmin)
xmax_idx <- which(ocorrencias_por_dia$Date == segmentos_destacados$xmax)

library(ggtext)

changepoint_MME_1 <- p + annotate("rect",
                                   xmin = xmin_idx,
                                   xmax = xmax_idx,
                                   ymin = -Inf,
                                   ymax = Inf,
                                   fill = "red",
                                   alpha = 0.05) +
  labs(title = paste0("June 2017 ", titulo_IQR15)) +
  theme(plot.title = element_markdown(hjust = 0.5))

changepoint_MME_1

# density analysis
evento <- dados_filtrados %>%
  filter(Date >= segmentos_destacados$xmin &
           Date <= segmentos_destacados$xmax)

evento <- evento %>%
  filter(!is.na(Long),
         !is.na(Lat))

summary(evento$dist_km_dom)

dens <- density(evento$dist_km_dom, bw = 50)

plot(dens,
     main = paste0("Kernel density along coast - June 2017\nBandwidth = ",
                   round(dens$bw, 2), " km"),
     xlab = "Distance along coast (km)")
rug(evento$dist_km_dom)

# density plot

dens_df <- data.frame(
  x = dens$x,
  y = dens$y
)

pico_idx <- which.max(dens_df$y)
pico_y   <- dens_df$y[pico_idx]

limiar <- 0.05 * pico_y

left_idx <- max(which(dens_df$y[1:pico_idx] < limiar))

right_idx <- pico_idx + 
  min(which(dens_df$y[pico_idx:length(dens_df$y)] < limiar)) - 1

x_inicio <- dens_df$x[left_idx]
x_fim    <- dens_df$x[right_idx]

x_inicio
x_fim

density_MME_1 <- ggplot(dens_df, aes(x = x, y = y)) +
                    geom_line(color = "black", linewidth = 0.4) +
                    geom_rug(data = evento,
                             aes(x = dist_km_dom),
                             sides = "b",
                             inherit.aes = FALSE) +
                    geom_vline(xintercept = c(x_inicio, x_fim),
                               color = "red",
                               linewidth = 0.4,
                               lty= 2) +
                    geom_hline(yintercept = 0,
                               color = "grey70",
                               linewidth = 0.4) +
                    labs(
                      title = paste0("June 2017"),
                      x = NULL,
                      y = NULL
                    ) +
                    scale_x_continuous(
                      expand = expansion(mult = 0.04),
                      breaks = seq(
                        floor(min(dens_df$x) / 500) * 500,
                        ceiling(max(dens_df$x) / 500) * 500,
                        by = 500
                      )
                    ) +
                    scale_y_continuous(
                      expand = expansion(mult = c(0.04, 0.04))
                    ) +
                    theme(
                      panel.background = element_rect(fill = "white"),
                      plot.background  = element_rect(fill = "white"),
                      panel.border = element_rect(color = "black", fill = NA),
                      panel.grid = element_blank(),
                      axis.line = element_blank(),
                      plot.title = element_text(hjust = 0.5, size = 14),
                      axis.title = element_text(size = 12),
                      axis.text = element_text(size = 10),
                      axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5)
                    ) +
  labs(title = paste0("June 2017 ", titulo_IQR15)) +
  theme(plot.title = element_markdown(hjust = 0.5))

density_MME_1

# temporal plot

brazil_june_2017_n <- ggplot(ocorrencias_por_dia, aes(x = Date, y = ocorrencias)) +
  geom_rect(data = segmentos_destacados,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "skyblue", alpha = 0.4, inherit.aes = FALSE) +
  geom_line(color = "red", size = 1.0) +
  geom_point(color = "red", size = 1.5) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
  labs(title = "", x = "", y = "") +
  theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(size = 9, hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA)
  )

brazil_june_2017_n

# heatmap plot
res_kde <- calc_kde_coast(evento, main_coast_dom)

pts_ll <- res_kde$pts_ll

line_sfc <- st_geometry(main_coast_dom)

total_length_km <- as.numeric(st_length(line_sfc)) / 1000

prop_min <- x_inicio / total_length_km
prop_max <- x_fim / total_length_km

p_inicio <- st_line_sample(line_sfc, sample = prop_min)
p_fim    <- st_line_sample(line_sfc, sample = prop_max)

p_inicio <- st_cast(p_inicio, "POINT") |> st_sf()
p_fim    <- st_cast(p_fim, "POINT") |> st_sf()

p_inicio <- st_transform(p_inicio, 4326)
p_fim    <- st_transform(p_fim, 4326)

p_inicio <- evento %>%
  slice(which.min(abs(dist_km_dom - x_inicio))) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

p_fim <- evento %>%
  slice(which.min(abs(dist_km_dom - x_fim))) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

brazil_june_2017 <- ggplot() +
  geom_sf(data = brasil, fill = "white", color = "black", size = 0.75) +
  geom_sf(data = regioes_sf,
          aes(fill = regiao),
          color = NA,
          alpha = 0.25) +
  annotate("text",
           x = -48, y = -29,
           label = paste0("bw = ", round(dens$bw, 2), " km"),
           hjust = 0,
           size = 2.5) +
  annotate("text", x = -41, y = -9,
           label = "NE",
           fontface = "italic",
           size = 3,
           color = "grey30") +
  annotate("text", x = -45, y = -19,
           label = "SE",
           fontface = "italic",
           size = 3,
           color = "grey30") +
  annotate("text", x = -51, y = -26,
           label = "S",
           fontface = "italic",
           size = 3,
           color = "grey30") +
  geom_sf(data = pts_ll,
          aes(color = dens),
          linewidth = 1) +
  geom_sf(data = p_inicio,
          shape = 21,
          fill = NA,
          color = "black",
          size = 2) +
  geom_sf(data = p_fim,
          shape = 21,
          fill = NA,
          color = "black",
          size = 2) +
  scale_fill_manual(values = c("Sul" = "#2C7BB6",
                               "Sudeste" = "#4DAF4A",
                               "Nordeste" = "#984EA3")) +
  scale_color_viridis_c(option = "inferno",
                        direction = -1,
                        name = "density") +
  guides(fill = "none") +
  scale_x_continuous(breaks = seq(-50, -35, by = 5))+
  coord_sf(xlim = c(-53,-34.5), ylim = c(-30, -3.5),
           expand = FALSE
  ) +
  theme_minimal() +
  labs(
    title = "June 2017",
    x = "",
    y = ""
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right",
    legend.justification = c("right", "top"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.4, "cm"),
    legend.spacing.y = unit(0.1, "cm"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  labs(title = paste0("June 2017 ", titulo_IQR15)) +
  theme(plot.title = element_markdown(hjust = 0.5))

brazil_june_2017

library(patchwork)
brazil_MME_1 <- brazil_june_2017 +
  inset_element(
    brazil_june_2017_n,
    left = 0.75,
    bottom = -0.05,
    right = 1.6,
    top = 0.60
  ) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = "white", color = NA)
    )
  )

brazil_MME_1

evento_pico <- evento %>%
  filter(dist_km_dom >= x_inicio &
           dist_km_dom <= x_fim)

dens <- density(evento_pico$dist_km_dom, bw = 50)
plot(dens)

levels <- seq(0.50, 1, by = 0.05)

results_table_MME_1 <- do.call(rbind,
                         lapply(levels, function(l)
                           calc_isopleth_metrics(
                             dens_obj = dens,
                             evento_df = evento_pico,
                             coast_line = main_coast_dom,
                             level = l,
                             event_name = "June 2017"
                           )
                         )
)

results_table_MME_1

iso95 <- results_table_MME_1 %>%
  filter(isopleth == 95)

min95 <- iso95$km_min
max95 <- iso95$km_max

evento_95 <- evento %>%
  filter(dist_km_dom >= min95 &
           dist_km_dom <= max95)

analise_changepoint(evento_95, ano = 2017, mes = 6, medias_mensais)

summary_epidemio <- ddply(evento_95, c("Species"), summarise,
                          n = length(Species),
                          fem = sum(Female, na.rm = TRUE),
                          male = sum(Male, na.rm = TRUE),
                          juvenile = sum(Juvenile, na.rm = TRUE),
                          adult = sum(Adult, na.rm = TRUE),
                          alive = sum(Alive, na.rm = TRUE),
                          dead = sum(Dead, na.rm = TRUE))

summary_epidemio$SexNA <- summary_epidemio$n - summary_epidemio$fem - summary_epidemio$male
summary_epidemio$StageNA <- summary_epidemio$n - summary_epidemio$adult - summary_epidemio$juvenile
summary_epidemio$sex<- paste(summary_epidemio$fem, summary_epidemio$male, summary_epidemio$SexNA, sep = "/")
summary_epidemio$stage<- paste(summary_epidemio$juvenile, summary_epidemio$adult, summary_epidemio$StageNA, sep = "/")
summary_epidemio$condition <- paste(summary_epidemio$alive, summary_epidemio$dead, sep="/")

tot_n        <- sum(summary_epidemio$n)
tot_fem      <- sum(summary_epidemio$fem)
tot_male     <- sum(summary_epidemio$male)
tot_SexNA    <- sum(summary_epidemio$SexNA)

tot_juvenile <- sum(summary_epidemio$juvenile)
tot_adult    <- sum(summary_epidemio$adult)
tot_StageNA  <- sum(summary_epidemio$StageNA)

tot_alive    <- sum(summary_epidemio$alive)
tot_dead     <- sum(summary_epidemio$dead)

linha_total <- data.frame(
  Species = "Total",
  n = tot_n,
  sex = paste0("(", tot_fem, "/", tot_male, "/", tot_SexNA, ")"),
  stage = paste0("(", tot_juvenile, "/", tot_adult, "/", tot_StageNA, ")"),
  condition = paste0("(", tot_alive, "/", tot_dead, ")")
)

summary_epidemio_clean <- summary_epidemio[, !(names(summary_epidemio) %in% 
                                                 c("fem", "male", "juvenile",
                                                   "adult", "alive", "dead",
                                                   "SexNA", "StageNA"))]

summary_epidemio_final <- rbind(summary_epidemio_clean, linha_total)

#### Brazil - 2018-07 and 08 and 10 - brazil 1 ####

p <- analise_changepoint(data_temporal, ano = 2018, mes = 7, medias_mensais)

segmentos_destacados <- data.frame(
  xmin = as.Date(c("2018-07-04", "2018-07-10", "2018-07-18")),
  xmax = as.Date(c("2018-07-07", "2018-07-12", "2018-07-31"))
)

xmin_idx <- match(segmentos_destacados$xmin, ocorrencias_por_dia$Date)
xmax_idx <- match(segmentos_destacados$xmax, ocorrencias_por_dia$Date)

changepoint_brazil_1 <- p +
  annotate(
    "rect",
    xmin = xmin_idx,
    xmax = xmax_idx,
    ymin = -Inf,
    ymax = Inf,
    fill = "red",
    alpha = 0.05
  )+
  labs(title = paste0("July 2018 ", titulo_IQR1)) +
  theme(plot.title = element_markdown(hjust = 0.5))

changepoint_brazil_1

p <- analise_changepoint(data_temporal, ano = 2018, mes = 8, medias_mensais)

segmentos_destacados <- data.frame(
  xmin = as.Date(c("2018-08-01", "2018-08-16")),
  xmax = as.Date(c("2018-08-12", "2018-08-31"))
)

xmin_idx <- match(segmentos_destacados$xmin, ocorrencias_por_dia$Date)
xmax_idx <- match(segmentos_destacados$xmax, ocorrencias_por_dia$Date)

changepoint_brazil_2 <- p +
  annotate(
    "rect",
    xmin = xmin_idx,
    xmax = xmax_idx,
    ymin = -Inf,
    ymax = Inf,
    fill = "red",
    alpha = 0.05
  )+
  labs(title = paste0("August 2018 ", titulo_IQR15)) +
  theme(plot.title = element_markdown(hjust = 0.5))

changepoint_brazil_2

dados_filtrados <- data_temporal %>%
  filter(Year == 2018,
         Month %in% c(7, 8))

dados_filtrados$Date <- as.Date(dados_filtrados$Date)

ocorrencias_por_dia <- dados_filtrados %>%
  group_by(Date) %>%
  summarise(ocorrencias = n())

todas_datas <- data.frame(Date = seq(as.Date("2018-07-01"), as.Date("2018-08-31"), by = "day"))

ocorrencias_por_dia <- todas_datas %>%
  left_join(ocorrencias_por_dia, by = "Date") %>%
  mutate(ocorrencias = replace_na(ocorrencias, 0))

segmentos_destacados <- data.frame(
  xmin = as.Date(c("2018-07-04", "2018-07-10", "2018-07-18", "2018-08-16")),
  xmax = as.Date(c("2018-07-07", "2018-07-12", "2018-08-12", "2018-08-31"))
)

evento <- dados_filtrados %>%
  rowwise() %>%
  filter(any(Date >= segmentos_destacados$xmin &
               Date <= segmentos_destacados$xmax)) %>%
  ungroup()

evento <- evento %>%
  filter(!is.na(Long),
         !is.na(Lat))

summary(evento$dist_km_dom)

dens <- density(evento$dist_km_dom, bw = 50)
plot(dens)

dens_df <- data.frame(
  x = dens$x,
  y = dens$y
)

pico_idx <- which.max(dens_df$y)
pico_y   <- dens_df$y[pico_idx]

limiar <- 0.05 * pico_y

left_idx <- max(which(dens_df$y[1:pico_idx] < limiar))

right_idx <- pico_idx + 
  min(which(dens_df$y[pico_idx:length(dens_df$y)] < limiar)) - 1

x_inicio <- dens_df$x[left_idx]
x_fim    <- dens_df$x[right_idx]

x_inicio
x_fim

density_brazil_1 <- ggplot(dens_df, aes(x = x, y = y)) +
  geom_line(color = "black", linewidth = 0.4) +
  geom_rug(data = evento,
           aes(x = dist_km_dom),
           sides = "b",
           inherit.aes = FALSE) +
  geom_vline(xintercept = c(x_inicio, x_fim),
             color = "red",
             linewidth = 0.4,
             lty= 2) +
  geom_hline(yintercept = 0,
             color = "grey70",
             linewidth = 0.4) +
  labs(
    title = paste0("July and August 2018"),
    x = NULL,
    y = NULL
  ) +
  scale_x_continuous(
    expand = expansion(mult = 0.04),
    breaks = seq(
      floor(min(dens_df$x) / 500) * 500,
      ceiling(max(dens_df$x) / 500) * 500,
      by = 500
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.04, 0.04))
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background  = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", fill = NA),
    panel.grid = element_blank(),
    axis.line = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5)
  ) +
  labs(title = paste0("July and August 2018", titulo_IQR15)) +
  theme(plot.title = element_markdown(hjust = 0.5))

density_brazil_1

brazil_july_aug_2018_n <- ggplot(ocorrencias_por_dia, aes(x = Date, y = ocorrencias)) +
  geom_rect(data = segmentos_destacados,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "skyblue", alpha = 0.4, inherit.aes = FALSE) +
  geom_line(color = "red", size = 0.8) +
  geom_point(color = "red", size = 1.3) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
  labs(title = "", x = "", y = "") +
  theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(size = 9, hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA)
  )

brazil_july_aug_2018_n

res_kde <- calc_kde_coast(evento, main_coast_dom)

pts_ll <- res_kde$pts_ll

line_sfc <- st_geometry(main_coast_dom)

total_length_km <- as.numeric(st_length(line_sfc)) / 1000

prop_min <- x_inicio / total_length_km
prop_max <- x_fim / total_length_km

p_inicio <- st_line_sample(line_sfc, sample = prop_min)
p_fim    <- st_line_sample(line_sfc, sample = prop_max)

p_inicio <- st_cast(p_inicio, "POINT") |> st_sf()
p_fim    <- st_cast(p_fim, "POINT") |> st_sf()

p_inicio <- st_transform(p_inicio, 4326)
p_fim    <- st_transform(p_fim, 4326)

p_inicio <- evento %>%
  slice(which.min(abs(dist_km_dom - x_inicio))) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

p_fim <- evento %>%
  slice(which.min(abs(dist_km_dom - x_fim))) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

brazil_july_aug_2018 <- ggplot() +
  geom_sf(data = brasil, fill = "white", color = "black", size = 0.75) +
  geom_sf(data = regioes_sf,
          aes(fill = regiao),
          color = NA,
          alpha = 0.25) +
  annotate("text", x = -41, y = -9,
           label = "NE",
           fontface = "italic",
           size = 3,
           color = "grey30") +
  annotate("text", x = -45, y = -19,
           label = "SE",
           fontface = "italic",
           size = 3,
           color = "grey30") +
  
  annotate("text", x = -51, y = -26,
           label = "S",
           fontface = "italic",
           size = 3,
           color = "grey30") +
  annotate("text",
           x = -48, y = -29,
           label = paste0("bw = ", round(dens$bw, 2), " km"),
           hjust = 0,
           size = 2.5) +
  scale_fill_manual(values = c("Sul" = "#2C7BB6",
                               "Sudeste" = "#4DAF4A",
                               "Nordeste" = "#984EA3")) +
  guides(fill = "none") +
  geom_sf(data = pts_ll,
          aes(color = dens),
          linewidth = 1) +
  geom_sf(data = p_inicio,
          shape = 21,
          fill = NA,
          color = "black",
          size = 2) +
  geom_sf(data = p_fim,
          shape = 21,
          fill = NA,
          color = "black",
          size = 2) +
  scale_color_viridis_c(option = "inferno",
                        direction = -1,
                        name = "density") +
  scale_x_continuous(breaks = seq(-50, -35, by = 5))+
  coord_sf(xlim = c(-53,-34.5), ylim = c(-30, -3.5),
           expand = FALSE
  ) +
  theme_minimal() +
  labs(
    title = paste0("July and August 2018"),
    x = "",
    y = ""
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right",
    legend.justification = c("right", "top"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.4, "cm"),
    legend.spacing.y = unit(0.1, "cm"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA)
  )+
  labs(title = paste0("July and August 2018 ", titulo_IQR15)) +
  theme(plot.title = element_markdown(hjust = 0.5))


brazil_july_aug_2018

library(patchwork)
brazil_plot1 <- brazil_july_aug_2018  +
  inset_element(
    brazil_july_aug_2018_n,
    left = 0.75,
    bottom = -0.05,
    right = 1.85,
    top = 0.65) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = "white", color = NA)
    )
  )

brazil_plot1

evento_pico <- evento %>%
  filter(dist_km_dom >= x_inicio &
           dist_km_dom <= x_fim)

dens <- density(evento_pico$dist_km_dom, bw = 50)
plot(dens)

levels <- seq(0.50, 1, by = 0.05)

results_table <- do.call(rbind,
                               lapply(levels, function(l)
                                 calc_isopleth_metrics(
                                   dens_obj = dens,
                                   evento_df = evento_pico,
                                   coast_line = main_coast_dom,
                                   level = l,
                                   event_name = "July and August 2018"
                                 )
                               )
)

results_table

iso95 <- results_table %>%
  filter(isopleth == 95)

min95 <- iso95$km_min
max95 <- iso95$km_max

evento_95 <- evento %>%
  filter(dist_km_dom >= min95 &
           dist_km_dom <= max95)

analise_changepoint(evento_95, ano = 2018, mes = 7, medias_mensais)
analise_changepoint(evento_95, ano = 2018, mes = 8, medias_mensais)

summary_epidemio <- ddply(evento_95, c("Species"), summarise,
                          n = length(Species),
                          fem = sum(Female, na.rm = TRUE),
                          male = sum(Male, na.rm = TRUE),
                          juvenile = sum(Juvenile, na.rm = TRUE),
                          adult = sum(Adult, na.rm = TRUE),
                          alive = sum(Alive, na.rm = TRUE),
                          dead = sum(Dead, na.rm = TRUE))

summary_epidemio$SexNA <- summary_epidemio$n - summary_epidemio$fem - summary_epidemio$male
summary_epidemio$StageNA <- summary_epidemio$n - summary_epidemio$adult - summary_epidemio$juvenile
summary_epidemio$sex<- paste(summary_epidemio$fem, summary_epidemio$male, summary_epidemio$SexNA, sep = "/")
summary_epidemio$stage<- paste(summary_epidemio$juvenile, summary_epidemio$adult, summary_epidemio$StageNA, sep = "/")
summary_epidemio$condition <- paste(summary_epidemio$alive, summary_epidemio$dead, sep="/")

tot_n        <- sum(summary_epidemio$n)
tot_fem      <- sum(summary_epidemio$fem)
tot_male     <- sum(summary_epidemio$male)
tot_SexNA    <- sum(summary_epidemio$SexNA)

tot_juvenile <- sum(summary_epidemio$juvenile)
tot_adult    <- sum(summary_epidemio$adult)
tot_StageNA  <- sum(summary_epidemio$StageNA)

tot_alive    <- sum(summary_epidemio$alive)
tot_dead     <- sum(summary_epidemio$dead)

linha_total <- data.frame(
  Species = "Total",
  n = tot_n,
  sex = paste0("(", tot_fem, "/", tot_male, "/", tot_SexNA, ")"),
  stage = paste0("(", tot_juvenile, "/", tot_adult, "/", tot_StageNA, ")"),
  condition = paste0("(", tot_alive, "/", tot_dead, ")")
)

summary_epidemio_clean <- summary_epidemio[, !(names(summary_epidemio) %in% 
                                                 c("fem", "male", "juvenile",
                                                   "adult", "alive", "dead",
                                                   "SexNA", "StageNA"))]

summary_epidemio_final <- rbind(summary_epidemio_clean, linha_total)

#### Brazil - 2018-09 - MME 2 ####

p <- analise_changepoint(data_temporal, ano = 2018, mes = 9, medias_mensais)

segmentos_destacados <- data.frame(
  xmin = as.Date(c("2018-09-11")),
  xmax = as.Date(c("2018-09-19"))
)

xmin_idx <- which(ocorrencias_por_dia$Date == segmentos_destacados$xmin)
xmax_idx <- which(ocorrencias_por_dia$Date == segmentos_destacados$xmax)

changepoint_MME_2 <- p +
  annotate(
    "rect",
    xmin = xmin_idx,
    xmax = xmax_idx,
    ymin = -Inf,
    ymax = Inf,
    fill = "red",
    alpha = 0.05
  )+
  labs(title = paste0("September 2018 ", titulo_IQR15)) +
  theme(plot.title = element_markdown(hjust = 0.5))

changepoint_MME_2

evento <- dados_filtrados %>%
  filter(Date >= segmentos_destacados$xmin &
           Date <= segmentos_destacados$xmax)

evento <- evento %>%
  filter(!is.na(Long),
         !is.na(Lat))

summary(evento$dist_km_dom)

dens <- density(evento$dist_km_dom, bw = 50)

plot(dens,
     main = paste0("Kernel density along coast - September 2018\nBandwidth = ",
                   round(dens$bw, 2), " km"),
     xlab = "Distance along coast (km)")

dens_df <- data.frame(
  x = dens$x,
  y = dens$y
)

pico_idx <- which.max(dens_df$y)
pico_y   <- dens_df$y[pico_idx]

limiar <- 0.05 * pico_y

left_idx <- max(which(dens_df$y[1:pico_idx] < limiar))
right_idx <- pico_idx +
  min(which(dens_df$y[pico_idx:length(dens_df$y)] < limiar)) - 1

xmin <- dens_df$x[left_idx]
xmax <- dens_df$x[right_idx]

density_MME_2 <- ggplot(dens_df, aes(x = x, y = y)) +
  geom_line(color = "black", linewidth = 0.4) +
  geom_rug(data = evento,
           aes(x = dist_km_dom),
           sides = "b",
           inherit.aes = FALSE) +
  geom_vline(xintercept = c(xmin, xmax),
             color = "red",
             linewidth = 0.4,
             lty= 2) +
  geom_hline(yintercept = 0,
             color = "grey70",
             linewidth = 0.4) +
  labs(
    title = paste0("September 2018"),
    x = NULL,
    y = NULL
  ) +
  scale_x_continuous(
    expand = expansion(mult = 0.04),
    breaks = seq(
      floor(min(dens_df$x) / 500) * 500,
      ceiling(max(dens_df$x) / 500) * 500,
      by = 500
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.04, 0.04))
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background  = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", fill = NA),
    panel.grid = element_blank(),
    axis.line = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5)
  )+
  labs(title = paste0("September 2018 ", titulo_IQR15)) +
  theme(plot.title = element_markdown(hjust = 0.5))

density_MME_2

brazil_sept_2018_n <- ggplot(ocorrencias_por_dia, aes(x = Date, y = ocorrencias)) +
  geom_rect(data = segmentos_destacados,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "skyblue", alpha = 0.4, inherit.aes = FALSE) +
  geom_line(color = "red", size = 1.0) +
  geom_point(color = "red", size = 1.5) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
  labs(title = "", x = "", y = "") +
  theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(size = 9, hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA)
  )

brazil_sept_2018_n

res_kde <- calc_kde_coast(evento, main_coast_dom)

pts_ll <- res_kde$pts_ll

line_sfc <- st_geometry(main_coast_dom)

total_length_km <- as.numeric(st_length(line_sfc)) / 1000

prop_min <- xmin / total_length_km
prop_max <- xmax / total_length_km

p_inicio <- st_line_sample(line_sfc, sample = prop_min)
p_fim    <- st_line_sample(line_sfc, sample = prop_max)

p_inicio <- st_cast(p_inicio, "POINT") |> st_sf()
p_fim    <- st_cast(p_fim, "POINT") |> st_sf()

p_inicio <- st_transform(p_inicio, 4326)
p_fim    <- st_transform(p_fim, 4326)

p_inicio <- evento %>%
  slice(which.min(abs(dist_km_dom - xmin))) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

p_fim <- evento %>%
  slice(which.min(abs(dist_km_dom - xmax))) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

brazil_sept_2018 <- ggplot() +
  geom_sf(data = brasil, fill = "white", color = "black", size = 0.75) +
  geom_sf(data = regioes_sf,
          aes(fill = regiao),
          color = NA,
          alpha = 0.25) +
  annotate("text", x = -41, y = -9,
           label = "NE",
           fontface = "italic",
           size = 3,
           color = "grey30") +
  annotate("text", x = -45, y = -19,
           label = "SE",
           fontface = "italic",
           size = 3,
           color = "grey30") +
  
  annotate("text", x = -51, y = -26,
           label = "S",
           fontface = "italic",
           size = 3,
           color = "grey30") +
  annotate("text",
           x = -48, y = -29,
           label = paste0("bw = ", round(dens$bw, 2), " km"),
           hjust = 0,
           size = 2.5) +
  geom_sf(data = pts_ll,
          aes(color = dens),
          linewidth = 1) +
  geom_sf(data = p_inicio,
          shape = 21,
          fill = NA,
          color = "black",
          size = 2) +
  geom_sf(data = p_fim,
          shape = 21,
          fill = NA,
          color = "black",
          size = 2) +
  scale_fill_manual(values = c("Sul" = "#2C7BB6",
                               "Sudeste" = "#4DAF4A",
                               "Nordeste" = "#984EA3")) +
  scale_color_viridis_c(option = "inferno",
                        direction = -1,
                        name = "density") +
  guides(fill = "none") +
  scale_x_continuous(breaks = seq(-50, -35, by = 5))+
  coord_sf(xlim = c(-53,-34.5), ylim = c(-30, -3.5),
           expand = FALSE
  ) +
  theme_minimal() +
  labs(
    title = paste0("September 2018"),
    x = "",
    y = ""
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right",
    legend.justification = c("right", "top"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.4, "cm"),
    legend.spacing.y = unit(0.1, "cm"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  labs(title = paste0("September 2018 ", titulo_IQR15)) +
  theme(plot.title = element_markdown(hjust = 0.5))

brazil_sept_2018

library(patchwork)
brazil_MME_2 <- brazil_sept_2018 +
  inset_element(
    brazil_sept_2018_n,
    left = 0.75,
    bottom = -.05,
    right = 1.6,
    top = 0.65
  ) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = "white", color = NA)
    )
  )

brazil_MME_2

evento_pico <- evento %>%
  filter(dist_km_dom >= xmin &
           dist_km_dom <= xmax)

dens <- density(evento_pico$dist_km_dom, bw = 50)
plot(dens)

levels <- seq(0.50, 1, by = 0.05)

results_table_MME_2 <- do.call(rbind,
                         lapply(levels, function(l)
                           calc_isopleth_metrics(
                             dens_obj = dens,
                             evento_df = evento_pico,
                             coast_line = main_coast_dom,
                             level = l,
                             event_name = "September 2018"
                           )
                         )
)

results_table_MME_2

iso95 <- results_table_MME_2 %>%
  filter(isopleth == 95)

min95 <- iso95$km_min
max95 <- iso95$km_max

evento_95 <- evento %>%
  filter(dist_km_dom >= min95 &
           dist_km_dom <= max95)

analise_changepoint(evento_95, ano = 2018, mes = 9, medias_mensais)

summary_epidemio <- ddply(evento_95, c("Species"), summarise,
                          n = length(Species),
                          fem = sum(Female, na.rm = TRUE),
                          male = sum(Male, na.rm = TRUE),
                          juvenile = sum(Juvenile, na.rm = TRUE),
                          adult = sum(Adult, na.rm = TRUE),
                          alive = sum(Alive, na.rm = TRUE),
                          dead = sum(Dead, na.rm = TRUE))

summary_epidemio$SexNA <- summary_epidemio$n - summary_epidemio$fem - summary_epidemio$male
summary_epidemio$StageNA <- summary_epidemio$n - summary_epidemio$adult - summary_epidemio$juvenile
summary_epidemio$sex<- paste(summary_epidemio$fem, summary_epidemio$male, summary_epidemio$SexNA, sep = "/")
summary_epidemio$stage<- paste(summary_epidemio$juvenile, summary_epidemio$adult, summary_epidemio$StageNA, sep = "/")
summary_epidemio$condition <- paste(summary_epidemio$alive, summary_epidemio$dead, sep="/")

tot_n        <- sum(summary_epidemio$n)
tot_fem      <- sum(summary_epidemio$fem)
tot_male     <- sum(summary_epidemio$male)
tot_SexNA    <- sum(summary_epidemio$SexNA)

tot_juvenile <- sum(summary_epidemio$juvenile)
tot_adult    <- sum(summary_epidemio$adult)
tot_StageNA  <- sum(summary_epidemio$StageNA)

tot_alive    <- sum(summary_epidemio$alive)
tot_dead     <- sum(summary_epidemio$dead)

linha_total <- data.frame(
  Species = "Total",
  n = tot_n,
  sex = paste0("(", tot_fem, "/", tot_male, "/", tot_SexNA, ")"),
  stage = paste0("(", tot_juvenile, "/", tot_adult, "/", tot_StageNA, ")"),
  condition = paste0("(", tot_alive, "/", tot_dead, ")")
)

summary_epidemio_clean <- summary_epidemio[, !(names(summary_epidemio) %in% 
                                                 c("fem", "male", "juvenile",
                                                   "adult", "alive", "dead",
                                                   "SexNA", "StageNA"))]

summary_epidemio_final <- rbind(summary_epidemio_clean, linha_total)

#### Brazil - 2019-05 - MME 3 ####

p <- analise_changepoint(data_temporal, ano = 2019, mes = 5, medias_mensais)

segmentos_destacados <- data.frame(
  xmin = as.Date(c("2019-05-16")),
  xmax = as.Date(c("2019-05-18"))
)

xmin_idx <- which(ocorrencias_por_dia$Date == segmentos_destacados$xmin)
xmax_idx <- which(ocorrencias_por_dia$Date == segmentos_destacados$xmax)

changepoint_MME_3 <- p +
  annotate(
    "rect",
    xmin = xmin_idx,
    xmax = xmax_idx,
    ymin = -Inf,
    ymax = Inf,
    fill = "red",
    alpha = 0.05
  )+
  labs(title = paste0("May 2019 ", titulo_IQR1)) +
  theme(plot.title = element_markdown(hjust = 0.5))

changepoint_MME_3

evento <- dados_filtrados %>%
  filter(Date >= segmentos_destacados$xmin &
           Date <= segmentos_destacados$xmax)

evento <- evento %>%
  filter(!is.na(Long),
         !is.na(Lat))

summary(evento$dist_km_dom)

dens <- density(evento$dist_km_dom, bw = 50)

plot(dens,
     main = paste0("Kernel density along coast - May 2019\nBandwidth = ",
                   round(dens$bw, 2), " km"),
     xlab = "Distance along coast (km)")

dens_df <- data.frame(
  x = dens$x,
  y = dens$y
)

pico_idx <- which.max(dens_df$y)
pico_y   <- dens_df$y[pico_idx]

limiar <- 0.05 * pico_y

left_idx <- max(which(dens_df$y[1:pico_idx] < limiar))
right_idx <- pico_idx +
  min(which(dens_df$y[pico_idx:length(dens_df$y)] < limiar)) - 1

xmin <- dens_df$x[left_idx]
xmax <- dens_df$x[right_idx]

density_MME_3 <- ggplot(dens_df, aes(x = x, y = y)) +
  geom_line(color = "black", linewidth = 0.4) +
  geom_rug(data = evento,
           aes(x = dist_km_dom),
           sides = "b",
           inherit.aes = FALSE) +
  geom_vline(xintercept = c(xmin, xmax),
             color = "red",
             linewidth = 0.4,
             lty= 2) +
  geom_hline(yintercept = 0,
             color = "grey70",
             linewidth = 0.4) +
  labs(
    title = paste0("May 2019"),
    x = NULL,
    y = NULL
  ) +
  scale_x_continuous(
    expand = expansion(mult = 0.04),
    breaks = seq(
      floor(min(dens_df$x) / 500) * 500,
      ceiling(max(dens_df$x) / 500) * 500,
      by = 500
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.04, 0.04))
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background  = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", fill = NA),
    panel.grid = element_blank(),
    axis.line = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5)
  ) +
  labs(title = paste0("May 2019 ", titulo_IQR1)) +
  theme(plot.title = element_markdown(hjust = 0.5))

density_MME_3

brazil_may_2019_n <- ggplot(ocorrencias_por_dia, aes(x = Date, y = ocorrencias)) +
  geom_rect(data = segmentos_destacados,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "skyblue", alpha = 0.4, inherit.aes = FALSE) +
  geom_line(color = "red", size = 1.0) +
  geom_point(color = "red", size = 1.5) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
  labs(title = "", x = "", y = "") +
  theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(size = 9, hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA)
  )

brazil_may_2019_n

res_kde <- calc_kde_coast(evento, main_coast_dom)

pts_ll <- res_kde$pts_ll

line_sfc <- st_geometry(main_coast_dom)

total_length_km <- as.numeric(st_length(line_sfc)) / 1000

prop_min <- xmin / total_length_km
prop_max <- xmax / total_length_km

p_inicio <- st_line_sample(line_sfc, sample = prop_min)
p_fim    <- st_line_sample(line_sfc, sample = prop_max)

p_inicio <- st_cast(p_inicio, "POINT") |> st_sf()
p_fim    <- st_cast(p_fim, "POINT") |> st_sf()

p_inicio <- st_transform(p_inicio, 4326)
p_fim    <- st_transform(p_fim, 4326)

brazil_may_2019 <- ggplot() +
  geom_sf(data = brasil, fill = "white", color = "black", size = 0.75) +
  geom_sf(data = regioes_sf,
          aes(fill = regiao),
          color = NA,
          alpha = 0.25) +
  annotate("text", x = -41, y = -9,
           label = "NE",
           fontface = "italic",
           size = 3,
           color = "grey30") +
  annotate("text", x = -45, y = -19,
           label = "SE",
           fontface = "italic",
           size = 3,
           color = "grey30") +
  
  annotate("text", x = -51, y = -26,
           label = "S",
           fontface = "italic",
           size = 3,
           color = "grey30") +
  annotate("text",
           x = -48, y = -29,
           label = paste0("bw = ", round(dens$bw, 2), " km"),
           hjust = 0,
           size = 2.5) +
  scale_fill_manual(values = c("Sul" = "#2C7BB6",
                               "Sudeste" = "#4DAF4A",
                               "Nordeste" = "#984EA3")) +
  guides(fill = "none") +
  geom_sf(data = pts_ll,
          aes(color = dens),
          linewidth = 1) +
  geom_sf(data = p_inicio,
          shape = 21,
          fill = NA,
          color = "black",
          size = 2) +
  geom_sf(data = p_fim,
          shape = 21,
          fill = NA,
          color = "black",
          size = 2) +
  scale_color_viridis_c(option = "inferno",
                        direction = -1,
                        name = "density") +
  scale_x_continuous(breaks = seq(-50, -35, by = 5))+
  coord_sf(xlim = c(-53,-34.5), ylim = c(-30, -3.5),
           expand = FALSE
  ) +
  theme_minimal() +
  labs(
    title = paste0("May 2019"),
    x = "",
    y = ""
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right",
    legend.justification = c("right", "top"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.4, "cm"),
    legend.spacing.y = unit(0.1, "cm"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA)
  )+
  labs(title = paste0("May 2019 ", titulo_IQR1)) +
  theme(plot.title = element_markdown(hjust = 0.5))

brazil_may_2019

library(patchwork)
brazil_MME_3 <- brazil_may_2019 +
  inset_element(
    brazil_may_2019_n,
    left = 0.75,
    bottom = -0.05,
    right = 1.6,
    top = 0.65
  ) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = "white", color = NA)
    )
  )

brazil_MME_3

evento_pico <- evento %>%
  filter(dist_km_dom >= xmin &
           dist_km_dom <= xmax)

dens <- density(evento_pico$dist_km_dom, bw = 50)
plot(dens)

dens_df <- data.frame(
  x = dens$x,
  y = dens$y
)

picos <- which(diff(sign(diff(dens_df$y))) == -2) + 1

dens_df$x[picos]

ord <- order(dens_df$y[picos], decreasing = TRUE)

pico1 <- picos[ord[1]]
pico2 <- picos[ord[2]]


idx_min <- min(pico1, pico2)
idx_max <- max(pico1, pico2)

vale_idx <- idx_min + which.min(dens_df$y[idx_min:idx_max]) - 1

x_vale <- dens_df$x[vale_idx]

ggplot(dens_df, aes(x = x, y = y)) +
  geom_line() +
  geom_vline(xintercept = x_vale, col = "blue", lty = 2)

results_table_MME_3 <- do.call(rbind,
                          lapply(levels, function(l)
                            calc_isopleth_metrics(
                              dens_obj = dens,
                              evento_df = evento_pico,
                              coast_line = main_coast_dom,
                              level = l,
                              event_name = "May 2019"
                            )
                          )
)

results_table_MME_3

evento_sub1 <- evento_pico %>%
  filter(dist_km_dom <= x_vale)

dens <- density(evento_sub1$dist_km_dom, bw = 50)

plot(dens,
     main = paste0("Kernel density along coast - May 2019 - hotspot 1\nBandwidth = ",
                   round(dens$bw, 2), " km"),
     xlab = "Distance along coast (km)")

levels <- seq(0.50, 0.95, by = 0.05)

results_table_MME_3_1 <- do.call(rbind,
                          lapply(levels, function(l)
                            calc_isopleth_metrics(
                              dens_obj = dens,
                              evento_df = evento_sub1,
                              coast_line = main_coast_dom,
                              level = l,
                              event_name = "May 2019 - 1"
                            )
                          )
)

results_table_MME_3_1

evento_sub2 <- evento_pico %>%
  filter(dist_km_dom >  x_vale)

dens <- density(evento_sub2$dist_km_dom, bw = 50)

plot(dens,
     main = paste0("Kernel density along coast - May 2019 - hotspot 2\nBandwidth = ",
                   round(dens$bw, 2), " km"),
     xlab = "Distance along coast (km)")

levels <- seq(0.50, 0.95, by = 0.05)

results_table_MME_3_2<- do.call(rbind,
                           lapply(levels, function(l)
                             calc_isopleth_metrics(
                               dens_obj = dens,
                               evento_df = evento_sub2,
                               coast_line = main_coast_dom,
                               level = l,
                               event_name = "May 2019 - 2"
                             )
                           )
)

results_table_MME_3_2

iso95 <- results_table_MME_3 %>%
  filter(isopleth == 95)

min95 <- iso95$km_min
max95 <- iso95$km_max

evento_95 <- evento %>%
  filter(dist_km_dom >= min95 &
           dist_km_dom <= max95)

analise_changepoint(evento_95, ano = 2019, mes = 5, medias_mensais)

summary_epidemio <- ddply(evento_95, c("Species"), summarise,
                          n = length(Species),
                          fem = sum(Female, na.rm = TRUE),
                          male = sum(Male, na.rm = TRUE),
                          juvenile = sum(Juvenile, na.rm = TRUE),
                          adult = sum(Adult, na.rm = TRUE),
                          alive = sum(Alive, na.rm = TRUE),
                          dead = sum(Dead, na.rm = TRUE))

summary_epidemio$SexNA <- summary_epidemio$n - summary_epidemio$fem - summary_epidemio$male
summary_epidemio$StageNA <- summary_epidemio$n - summary_epidemio$adult - summary_epidemio$juvenile
summary_epidemio$sex<- paste(summary_epidemio$fem, summary_epidemio$male, summary_epidemio$SexNA, sep = "/")
summary_epidemio$stage<- paste(summary_epidemio$juvenile, summary_epidemio$adult, summary_epidemio$StageNA, sep = "/")
summary_epidemio$condition <- paste(summary_epidemio$alive, summary_epidemio$dead, sep="/")

tot_n        <- sum(summary_epidemio$n)
tot_fem      <- sum(summary_epidemio$fem)
tot_male     <- sum(summary_epidemio$male)
tot_SexNA    <- sum(summary_epidemio$SexNA)

tot_juvenile <- sum(summary_epidemio$juvenile)
tot_adult    <- sum(summary_epidemio$adult)
tot_StageNA  <- sum(summary_epidemio$StageNA)

tot_alive    <- sum(summary_epidemio$alive)
tot_dead     <- sum(summary_epidemio$dead)

linha_total <- data.frame(
  Species = "Total",
  n = tot_n,
  sex = paste0("(", tot_fem, "/", tot_male, "/", tot_SexNA, ")"),
  stage = paste0("(", tot_juvenile, "/", tot_adult, "/", tot_StageNA, ")"),
  condition = paste0("(", tot_alive, "/", tot_dead, ")")
)

summary_epidemio_clean <- summary_epidemio[, !(names(summary_epidemio) %in% 
                                                 c("fem", "male", "juvenile",
                                                   "adult", "alive", "dead",
                                                   "SexNA", "StageNA"))]

summary_epidemio_final <- rbind(summary_epidemio_clean, linha_total)

#### Brazil - 2020-09 and 10 - MME 4 ####

p <- analise_changepoint(data_temporal, ano = 2020, mes = 9, medias_mensais)

segmentos_destacados <- data.frame(
  xmin = as.Date(c("2020-09-21")),
  xmax = as.Date(c("2020-09-30"))
)

xmin_idx <- which(ocorrencias_por_dia$Date == segmentos_destacados$xmin)
xmax_idx <- which(ocorrencias_por_dia$Date == segmentos_destacados$xmax)

changepoint_MME_4 <- p + annotate("rect",
                                  xmin = xmin_idx,
                                  xmax = xmax_idx,
                                  ymin = -Inf,
                                  ymax = Inf,
                                  fill = "red",
                                  alpha = 0.05)+
  labs(title = paste0("September 2020  ", titulo_IQR1)) +
  theme(plot.title = element_markdown(hjust = 0.5))

changepoint_MME_4

p <- analise_changepoint(data_temporal, ano = 2020, mes = 10, medias_mensais)

segmentos_destacados <- data.frame(
  xmin = as.Date(c("2020-10-01")),
  xmax = as.Date(c("2020-10-24"))
)

xmin_idx <- which(ocorrencias_por_dia$Date == segmentos_destacados$xmin)
xmax_idx <- which(ocorrencias_por_dia$Date == segmentos_destacados$xmax)

changepoint_MME_5 <- p +
  annotate(
    "rect",
    xmin = xmin_idx,
    xmax = xmax_idx,
    ymin = -Inf,
    ymax = Inf,
    fill = "red",
    alpha = 0.05
  )+
  labs(title = paste0("October 2020 ", titulo_IQR3)) +
  theme(plot.title = element_markdown(hjust = 0.5))

changepoint_MME_5

dados_filtrados <- data_temporal %>%
  filter(Year == 2020,
         Month %in% c(9, 10))

dados_filtrados$Date <- as.Date(dados_filtrados$Date)

ocorrencias_por_dia <- dados_filtrados %>%
  group_by(Date) %>%
  summarise(ocorrencias = n())

todas_datas <- data.frame(Date = seq(as.Date("2020-09-01"), as.Date("2020-10-31"), by = "day"))

ocorrencias_por_dia <- todas_datas %>%
  left_join(ocorrencias_por_dia, by = "Date") %>%
  mutate(ocorrencias = replace_na(ocorrencias, 0))

segmentos_destacados <- data.frame(
  xmin = as.Date(c("2020-09-21")),
  xmax = as.Date(c("2020-10-24"))
)

evento <- dados_filtrados %>%
  filter(Date >= segmentos_destacados$xmin &
           Date <= segmentos_destacados$xmax)

evento <- evento %>%
  filter(!is.na(Long),
         !is.na(Lat))

summary(evento$dist_km_dom)

dens <- density(evento$dist_km_dom, bw = 50)
plot(dens)

dens_df <- data.frame(
  x = dens$x,
  y = dens$y
)

pico_idx <- which.max(dens_df$y)
pico_y   <- dens_df$y[pico_idx]

limiar <- 0.05 * pico_y

left_idx <- max(which(dens_df$y[1:pico_idx] < limiar))

right_idx <- pico_idx + 
  min(which(dens_df$y[pico_idx:length(dens_df$y)] < limiar)) - 1

x_inicio <- dens_df$x[left_idx]
x_fim    <- dens_df$x[right_idx]

x_inicio
x_fim

density_MME_4 <- ggplot(dens_df, aes(x = x, y = y)) +
  geom_line(color = "black", linewidth = 0.4) +
  geom_rug(data = evento,
           aes(x = dist_km_dom),
           sides = "b",
           inherit.aes = FALSE) +
  geom_vline(xintercept = c(x_inicio, x_fim),
             color = "red",
             linewidth = 0.4,
             lty= 2) +
  geom_hline(yintercept = 0,
             color = "grey70",
             linewidth = 0.4) +
  labs(
    title = paste0("September and October 2020"),
    x = NULL,
    y = NULL
  ) +
  scale_x_continuous(
    expand = expansion(mult = 0.04),
    breaks = seq(
      floor(min(dens_df$x) / 500) * 500,
      ceiling(max(dens_df$x) / 500) * 500,
      by = 500
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.04, 0.04))
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background  = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", fill = NA),
    panel.grid = element_blank(),
    axis.line = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5)
  ) +
  labs(title = paste0("September and October 2020 ", titulo_IQR3)) +
  theme(plot.title = element_markdown(hjust = 0.5))

density_MME_4

brazil_sept_oct_2020_n <- ggplot(ocorrencias_por_dia, aes(x = Date, y = ocorrencias)) +
  geom_rect(data = segmentos_destacados,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "skyblue", alpha = 0.4, inherit.aes = FALSE) +
  geom_line(color = "red", size = 1.0) +
  geom_point(color = "red", size = 1.5) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
  labs(title = "", x = "", y = "") +
  theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(size = 9, hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA)
  )

brazil_sept_oct_2020_n

res_kde <- calc_kde_coast(evento, main_coast_dom)

pts_ll <- res_kde$pts_ll

line_sfc <- st_geometry(main_coast_dom)

total_length_km <- as.numeric(st_length(line_sfc)) / 1000

prop_min <- x_inicio / total_length_km
prop_max <- x_fim / total_length_km

p_inicio <- st_line_sample(line_sfc, sample = prop_min)
p_fim    <- st_line_sample(line_sfc, sample = prop_max)

p_inicio <- st_cast(p_inicio, "POINT") |> st_sf()
p_fim    <- st_cast(p_fim, "POINT") |> st_sf()

p_inicio <- st_transform(p_inicio, 4326)
p_fim    <- st_transform(p_fim, 4326)

p_inicio <- evento %>%
  slice(which.min(abs(dist_km_dom - x_inicio))) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

p_fim <- evento %>%
  slice(which.min(abs(dist_km_dom - x_fim))) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

brazil_sept_oct_2020 <- ggplot() +
  geom_sf(data = brasil, fill = "white", color = "black", size = 0.75) +
  geom_sf(data = regioes_sf,
          aes(fill = regiao),
          color = NA,
          alpha = 0.25) +
  annotate("text", x = -41, y = -9,
           label = "NE",
           fontface = "italic",
           size = 3,
           color = "grey30") +
  annotate("text", x = -45, y = -19,
           label = "SE",
           fontface = "italic",
           size = 3,
           color = "grey30") +
  
  annotate("text", x = -51, y = -26,
           label = "S",
           fontface = "italic",
           size = 3,
           color = "grey30") +
  annotate("text",
           x = -48, y = -29,
           label = paste0("bw = ", round(dens$bw, 2), " km"),
           hjust = 0,
           size = 2.5) +
  scale_fill_manual(values = c("Sul" = "#2C7BB6",
                               "Sudeste" = "#4DAF4A",
                               "Nordeste" = "#984EA3")) +
  guides(fill = "none") +
  geom_sf(data = pts_ll,
          aes(color = dens),
          linewidth = 1) +
  geom_sf(data = p_inicio,
          shape = 21,
          fill = NA,
          color = "black",
          size = 2) +
  geom_sf(data = p_fim,
          shape = 21,
          fill = NA,
          color = "black",
          size = 2) +
  scale_color_viridis_c(option = "inferno",
                        direction = -1,
                        name = "density") +
  scale_x_continuous(breaks = seq(-50, -35, by = 5))+
  coord_sf(xlim = c(-53,-34.5), ylim = c(-30, -3.5),
           expand = FALSE
  ) +
  theme_minimal() +
  labs(
    title = paste0("September and October 2020"),
    x = "",
    y = ""
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right",
    legend.justification = c("right", "top"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.4, "cm"),
    legend.spacing.y = unit(0.1, "cm"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA)
  )+
  labs(title = paste0("September and October 2020 ", titulo_IQR3)) +
  theme(plot.title = element_markdown(hjust = 0.5))


brazil_sept_oct_2020

library(patchwork)
brazil_MME_4 <- brazil_sept_oct_2020  +
  inset_element(
    brazil_sept_oct_2020_n,
    left = 0.75,
    bottom = -0.05,
    right = 1.85,
    top = 0.65) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = "white", color = NA)
    )
  )

brazil_MME_4

evento_pico <- evento %>%
  filter(dist_km_dom >= x_inicio &
           dist_km_dom <= x_fim)

dens <- density(evento_pico$dist_km_dom, bw = 50)
plot(dens)

levels <- seq(0.50, 1, by = 0.05)

results_table_MME_4 <- do.call(rbind,
                               lapply(levels, function(l)
                                 calc_isopleth_metrics(
                                   dens_obj = dens,
                                   evento_df = evento_pico,
                                   coast_line = main_coast_dom,
                                   level = l,
                                   event_name = "September and October 2020"
                                 )
                               )
)

results_table_MME_4

iso95 <- results_table_MME_4 %>%
  filter(isopleth == 95)

min95 <- iso95$km_min
max95 <- iso95$km_max

evento_95 <- evento %>%
  filter(dist_km_dom >= min95 &
           dist_km_dom <= max95)

analise_changepoint(evento_95, ano = 2020, mes = 9, medias_mensais)
analise_changepoint(evento_95, ano = 2020, mes = 10, medias_mensais)

summary_epidemio <- ddply(evento_95, c("Species"), summarise,
                          n = length(Species),
                          fem = sum(Female, na.rm = TRUE),
                          male = sum(Male, na.rm = TRUE),
                          juvenile = sum(Juvenile, na.rm = TRUE),
                          adult = sum(Adult, na.rm = TRUE),
                          alive = sum(Alive, na.rm = TRUE),
                          dead = sum(Dead, na.rm = TRUE))

summary_epidemio$SexNA <- summary_epidemio$n - summary_epidemio$fem - summary_epidemio$male
summary_epidemio$StageNA <- summary_epidemio$n - summary_epidemio$adult - summary_epidemio$juvenile
summary_epidemio$sex<- paste(summary_epidemio$fem, summary_epidemio$male, summary_epidemio$SexNA, sep = "/")
summary_epidemio$stage<- paste(summary_epidemio$juvenile, summary_epidemio$adult, summary_epidemio$StageNA, sep = "/")
summary_epidemio$condition <- paste(summary_epidemio$alive, summary_epidemio$dead, sep="/")

tot_n        <- sum(summary_epidemio$n)
tot_fem      <- sum(summary_epidemio$fem)
tot_male     <- sum(summary_epidemio$male)
tot_SexNA    <- sum(summary_epidemio$SexNA)

tot_juvenile <- sum(summary_epidemio$juvenile)
tot_adult    <- sum(summary_epidemio$adult)
tot_StageNA  <- sum(summary_epidemio$StageNA)

tot_alive    <- sum(summary_epidemio$alive)
tot_dead     <- sum(summary_epidemio$dead)

linha_total <- data.frame(
  Species = "Total",
  n = tot_n,
  sex = paste0("(", tot_fem, "/", tot_male, "/", tot_SexNA, ")"),
  stage = paste0("(", tot_juvenile, "/", tot_adult, "/", tot_StageNA, ")"),
  condition = paste0("(", tot_alive, "/", tot_dead, ")")
)

summary_epidemio_clean <- summary_epidemio[, !(names(summary_epidemio) %in% 
                                                 c("fem", "male", "juvenile",
                                                   "adult", "alive", "dead",
                                                   "SexNA", "StageNA"))]

summary_epidemio_final <- rbind(summary_epidemio_clean, linha_total)

#### Brazil - 2021-12 ####

p <- analise_changepoint(data_temporal, ano = 2021, mes = 12, medias_mensais)

segmentos_destacados <- data.frame(
  xmin = as.Date(c("2021-12-01")),
  xmax = as.Date(c("2021-12-27"))
)

xmin_idx <- match(segmentos_destacados$xmin, ocorrencias_por_dia$Date)
xmax_idx <- match(segmentos_destacados$xmax, ocorrencias_por_dia$Date)

changepoint_brazil_3 <- p +
  annotate(
    "rect",
    xmin = xmin_idx,
    xmax = xmax_idx,
    ymin = -Inf,
    ymax = Inf,
    fill = "red",
    alpha = 0.05
  )+
  labs(title = paste0("December 2021 ", titulo_IQR1)) +
  theme(plot.title = element_markdown(hjust = 0.5))

changepoint_brazil_3

evento <- purrr::map2_dfr(
  segmentos_destacados$xmin,
  segmentos_destacados$xmax,
  ~ dados_filtrados %>%
    filter(Date >= .x & Date <= .y)
)

evento <- evento %>%
  filter(!is.na(Long),
         !is.na(Lat))

summary(evento$dist_km_dom)

dens <- density(evento$dist_km_dom, bw = 50)

plot(dens,
     main = paste0("Kernel density along coast - December 2021\nBandwidth = ",
                   round(dens$bw, 2), " km"),
     xlab = "Distance along coast (km)")

dens_df <- data.frame(
  x = dens$x,
  y = dens$y
)

pico_idx <- which.max(dens_df$y)
pico_y   <- dens_df$y[pico_idx]

limiar <- 0.05 * pico_y

left_idx <- max(which(dens_df$y[1:pico_idx] < limiar))

right_idx <- pico_idx + 
  min(which(dens_df$y[pico_idx:length(dens_df$y)] < limiar)) - 1

x_inicio <- dens_df$x[left_idx]
x_fim    <- dens_df$x[right_idx]

x_inicio
x_fim

density_brazil_2 <- ggplot(dens_df, aes(x = x, y = y)) +
  geom_line(color = "black", linewidth = 0.4) +
  geom_rug(data = evento,
           aes(x = dist_km_dom),
           sides = "b",
           inherit.aes = FALSE) +
  geom_vline(xintercept = c(x_inicio, x_fim),
             color = "red",
             linewidth = 0.4,
             lty= 2) +
  geom_hline(yintercept = 0,
             color = "grey70",
             linewidth = 0.4) +
  labs(
    title = paste0("August 2018"),
    x = NULL,
    y = NULL
  ) +
  scale_x_continuous(
    expand = expansion(mult = 0.04),
    breaks = seq(
      floor(min(dens_df$x) / 500) * 500,
      ceiling(max(dens_df$x) / 500) * 500,
      by = 500
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.04, 0.04))
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background  = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", fill = NA),
    panel.grid = element_blank(),
    axis.line = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5)
  ) +
  labs(title = paste0("December 2021 ", titulo_IQR1)) +
  theme(plot.title = element_markdown(hjust = 0.5))

density_brazil_2

brazil_dec_2021_n <- ggplot(ocorrencias_por_dia, aes(x = Date, y = ocorrencias)) +
  geom_rect(data = segmentos_destacados,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "skyblue", alpha = 0.3, inherit.aes = FALSE) +
  geom_line(color = "red", size = 1.0) +
  geom_point(color = "red", size = 1.5) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
  labs(title = "", x = "", y = "") +
  theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(size = 9, hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA)
  )

brazil_dec_2021_n

res_kde <- calc_kde_coast(evento, main_coast_dom)

pts_ll <- res_kde$pts_ll

line_sfc <- st_geometry(main_coast_dom)

total_length_km <- as.numeric(st_length(line_sfc)) / 1000

prop_min <- x_inicio / total_length_km
prop_max <- x_fim / total_length_km

p_inicio <- st_line_sample(line_sfc, sample = prop_min)
p_fim    <- st_line_sample(line_sfc, sample = prop_max)

p_inicio <- st_cast(p_inicio, "POINT") |> st_sf()
p_fim    <- st_cast(p_fim, "POINT") |> st_sf()

p_inicio <- st_transform(p_inicio, 4326)
p_fim    <- st_transform(p_fim, 4326)

p_inicio <- evento %>%
  slice(which.min(abs(dist_km_dom - x_inicio))) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

p_fim <- evento %>%
  slice(which.min(abs(dist_km_dom - x_fim))) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

brazil_dec_2021 <- ggplot() +
  geom_sf(data = brasil, fill = "white", color = "black", size = 0.75) +
  geom_sf(data = regioes_sf,
          aes(fill = regiao),
          color = NA,
          alpha = 0.25) +
  annotate("text", x = -41, y = -9,
           label = "NE",
           fontface = "italic",
           size = 3,
           color = "grey30") +
  annotate("text", x = -45, y = -19,
           label = "SE",
           fontface = "italic",
           size = 3,
           color = "grey30") +
  
  annotate("text", x = -51, y = -26,
           label = "S",
           fontface = "italic",
           size = 3,
           color = "grey30") +
  annotate("text",
           x = -48, y = -29,
           label = paste0("bw = ", round(dens$bw, 2), " km"),
           hjust = 0,
           size = 2.5) +
  scale_fill_manual(values = c("Sul" = "#2C7BB6",
                               "Sudeste" = "#4DAF4A",
                               "Nordeste" = "#984EA3")) +
  guides(fill = "none") +
  geom_sf(data = pts_ll,
          aes(color = dens),
          linewidth = 1) +
  geom_sf(data = p_inicio,
          shape = 21,
          fill = NA,
          color = "black",
          size = 2) +
  geom_sf(data = p_fim,
          shape = 21,
          fill = NA,
          color = "black",
          size = 2) +
  scale_color_viridis_c(option = "inferno",
                        direction = -1,
                        name = "density") +
  scale_x_continuous(breaks = seq(-50, -35, by = 5))+
  coord_sf(xlim = c(-53,-34.5), ylim = c(-30, -3.5),
           expand = FALSE
  ) +
  theme_minimal() +
  labs(
    title = paste0("December 2021 (n = ", nrow(evento), ")"),
    x = "",
    y = ""
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right",
    legend.justification = c("right", "top"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.4, "cm"),
    legend.spacing.y = unit(0.1, "cm"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA)
  )+
  labs(title = paste0("December 2021 ", titulo_IQR1)) +
  theme(plot.title = element_markdown(hjust = 0.5))

brazil_dec_2021

library(patchwork)
brazil_plot2 <- brazil_dec_2021 +
  inset_element(
    brazil_dec_2021_n,
    left = 0.75,
    bottom = -0.05,
    right = 1.6,
    top = 0.60
  ) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = "white", color = NA)
    )
  )

brazil_plot2

evento_pico <- evento %>%
  filter(dist_km_dom >= x_inicio &
           dist_km_dom <= x_fim)

dens <- density(evento_pico$dist_km_dom, bw = 50)
plot(dens)

levels <- seq(0.50, 0.95, by = 0.05)

results_table2 <- do.call(rbind,
                          lapply(levels, function(l)
                            calc_isopleth_metrics(
                              dens_obj = dens,
                              evento_df = evento_pico,
                              coast_line = main_coast_dom,
                              level = l,
                              event_name = "December 2021"
                            )
                          )
)

results_table2

iso95 <- results_table2 %>%
  filter(isopleth == 95)

min95 <- iso95$km_min
max95 <- iso95$km_max

evento_95 <- evento %>%
  filter(dist_km_dom >= min95 &
           dist_km_dom <= max95)

analise_changepoint(evento_95, ano = 2021, mes = 12, medias_mensais)

summary_epidemio <- ddply(evento_95, c("Species"), summarise,
                          n = length(Species),
                          fem = sum(Female, na.rm = TRUE),
                          male = sum(Male, na.rm = TRUE),
                          juvenile = sum(Juvenile, na.rm = TRUE),
                          adult = sum(Adult, na.rm = TRUE),
                          alive = sum(Alive, na.rm = TRUE),
                          dead = sum(Dead, na.rm = TRUE))

summary_epidemio$SexNA <- summary_epidemio$n - summary_epidemio$fem - summary_epidemio$male
summary_epidemio$StageNA <- summary_epidemio$n - summary_epidemio$adult - summary_epidemio$juvenile
summary_epidemio$sex<- paste(summary_epidemio$fem, summary_epidemio$male, summary_epidemio$SexNA, sep = "/")
summary_epidemio$stage<- paste(summary_epidemio$juvenile, summary_epidemio$adult, summary_epidemio$StageNA, sep = "/")
summary_epidemio$condition <- paste(summary_epidemio$alive, summary_epidemio$dead, sep="/")

tot_n        <- sum(summary_epidemio$n)
tot_fem      <- sum(summary_epidemio$fem)
tot_male     <- sum(summary_epidemio$male)
tot_SexNA    <- sum(summary_epidemio$SexNA)

tot_juvenile <- sum(summary_epidemio$juvenile)
tot_adult    <- sum(summary_epidemio$adult)
tot_StageNA  <- sum(summary_epidemio$StageNA)

tot_alive    <- sum(summary_epidemio$alive)
tot_dead     <- sum(summary_epidemio$dead)

linha_total <- data.frame(
  Species = "Total",
  n = tot_n,
  sex = paste0("(", tot_fem, "/", tot_male, "/", tot_SexNA, ")"),
  stage = paste0("(", tot_juvenile, "/", tot_adult, "/", tot_StageNA, ")"),
  condition = paste0("(", tot_alive, "/", tot_dead, ")")
)

summary_epidemio_clean <- summary_epidemio[, !(names(summary_epidemio) %in% 
                                                 c("fem", "male", "juvenile",
                                                   "adult", "alive", "dead",
                                                   "SexNA", "StageNA"))]

summary_epidemio_final <- rbind(summary_epidemio_clean, linha_total)

#### Brazil - 2022-10 and 11 - MME 5 ####

p <- analise_changepoint(data_temporal, ano = 2022, mes = 10, medias_mensais)

segmentos_destacados <- data.frame(
  xmin = as.Date(c("2022-10-15")),
  xmax = as.Date(c("2022-10-31"))
)

xmin_idx <- which(ocorrencias_por_dia$Date == segmentos_destacados$xmin)
xmax_idx <- which(ocorrencias_por_dia$Date == segmentos_destacados$xmax)

changepoint_MME_6 <- p + annotate("rect",
                                  xmin = xmin_idx,
                                  xmax = xmax_idx,
                                  ymin = -Inf,
                                  ymax = Inf,
                                  fill = "red",
                                  alpha = 0.05)+
  labs(title = paste0("October 2022 ", titulo_IQR1)) +
  theme(plot.title = element_markdown(hjust = 0.5))

changepoint_MME_6

p <- analise_changepoint(data_temporal, ano = 2022, mes = 11, medias_mensais)

segmentos_destacados <- data.frame(
  xmin = as.Date(c("2022-11-01")),
  xmax = as.Date(c("2022-11-07")))

xmin_idx <- which(ocorrencias_por_dia$Date == segmentos_destacados$xmin)
xmax_idx <- which(ocorrencias_por_dia$Date == segmentos_destacados$xmax)

changepoint_MME_7 <- p + annotate("rect",
                                  xmin = xmin_idx,
                                  xmax = xmax_idx,
                                  ymin = -Inf,
                                  ymax = Inf,
                                  fill = "red",
                                  alpha = 0.05)+
  labs(title = paste0("November 2022 ", titulo_IQR15)) +
  theme(plot.title = element_markdown(hjust = 0.5))

changepoint_MME_7

dados_filtrados <- data_temporal %>%
  filter(Year == 2022,
         Month %in% c(10, 11))

dados_filtrados$Date <- as.Date(dados_filtrados$Date)

ocorrencias_por_dia <- dados_filtrados %>%
  group_by(Date) %>%
  summarise(ocorrencias = n())

todas_datas <- data.frame(Date = seq(as.Date("2022-10-01"), as.Date("2022-11-30"), by = "day"))

ocorrencias_por_dia <- todas_datas %>%
  left_join(ocorrencias_por_dia, by = "Date") %>%
  mutate(ocorrencias = replace_na(ocorrencias, 0))

segmentos_destacados <- data.frame(
  xmin = as.Date(c("2022-10-15")),
  xmax = as.Date(c("2022-11-07"))
)

evento <- dados_filtrados %>%
  filter(Date >= segmentos_destacados$xmin &
           Date <= segmentos_destacados$xmax)

evento <- evento %>%
  filter(!is.na(Long),
         !is.na(Lat))

summary(evento$dist_km_dom)

dens <- density(evento$dist_km_dom, bw = 50)
plot(dens)

dens_df <- data.frame(
  x = dens$x,
  y = dens$y
)

pico_idx <- which.max(dens_df$y)
pico_y   <- dens_df$y[pico_idx]

limiar <- 0.05 * pico_y

left_idx <- max(which(dens_df$y[1:pico_idx] < limiar))

right_idx <- pico_idx + 
  min(which(dens_df$y[pico_idx:length(dens_df$y)] < limiar)) - 1

x_inicio <- dens_df$x[left_idx]
x_fim    <- dens_df$x[right_idx]

x_inicio
x_fim

density_MME_5 <- ggplot(dens_df, aes(x = x, y = y)) +
  geom_line(color = "black", linewidth = 0.4) +
  geom_rug(data = evento,
           aes(x = dist_km_dom),
           sides = "b",
           inherit.aes = FALSE) +
  geom_vline(xintercept = c(x_inicio, x_fim),
             color = "red",
             linewidth = 0.4,
             lty= 2) +
  geom_hline(yintercept = 0,
             color = "grey70",
             linewidth = 0.4) +
  labs(
    title = paste0("October and November 2022"),
    x = NULL,
    y = NULL
  ) +
  scale_x_continuous(
    expand = expansion(mult = 0.04),
    breaks = seq(
      floor(min(dens_df$x) / 500) * 500,
      ceiling(max(dens_df$x) / 500) * 500,
      by = 500
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.04, 0.04))
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background  = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", fill = NA),
    panel.grid = element_blank(),
    axis.line = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5)
  )+
  labs(title = paste0("October and November 2022 ", titulo_IQR15)) +
  theme(plot.title = element_markdown(hjust = 0.5))

density_MME_5

brazil_oct_nov_2022_n <- ggplot(ocorrencias_por_dia, aes(x = Date, y = ocorrencias)) +
  geom_rect(data = segmentos_destacados,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "skyblue", alpha = 0.4, inherit.aes = FALSE) +
  geom_line(color = "red", size = 1.0) +
  geom_point(color = "red", size = 1.5) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
  labs(title = "", x = "", y = "") +
  theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(size = 9, hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA)
  )

brazil_oct_nov_2022_n

res_kde <- calc_kde_coast(evento, main_coast_dom)

pts_ll <- res_kde$pts_ll

line_sfc <- st_geometry(main_coast_dom)

total_length_km <- as.numeric(st_length(line_sfc)) / 1000

prop_min <- x_inicio / total_length_km
prop_max <- x_fim / total_length_km

p_inicio <- st_line_sample(line_sfc, sample = prop_min)
p_fim    <- st_line_sample(line_sfc, sample = prop_max)

p_inicio <- st_cast(p_inicio, "POINT") |> st_sf()
p_fim    <- st_cast(p_fim, "POINT") |> st_sf()

p_inicio <- st_transform(p_inicio, 4326)
p_fim    <- st_transform(p_fim, 4326)

p_inicio <- evento %>%
  slice(which.min(abs(dist_km_dom - x_inicio))) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

p_fim <- evento %>%
  slice(which.min(abs(dist_km_dom - x_fim))) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

brazil_oct_nov_2022 <- ggplot() +
  geom_sf(data = brasil, fill = "white", color = "black", size = 0.75) +
  geom_sf(data = regioes_sf,
          aes(fill = regiao),
          color = NA,
          alpha = 0.25) +
  annotate("text", x = -41, y = -9,
           label = "NE",
           fontface = "italic",
           size = 3,
           color = "grey30") +
  annotate("text", x = -45, y = -19,
           label = "SE",
           fontface = "italic",
           size = 3,
           color = "grey30") +
  
  annotate("text", x = -51, y = -26,
           label = "S",
           fontface = "italic",
           size = 3,
           color = "grey30") +
  annotate("text",
           x = -48, y = -29,
           label = paste0("bw = ", round(dens$bw, 2), " km"),
           hjust = 0,
           size = 2.5) +
  scale_fill_manual(values = c("Sul" = "#2C7BB6",
                               "Sudeste" = "#4DAF4A",
                               "Nordeste" = "#984EA3")) +
  guides(fill = "none") +
  geom_sf(data = pts_ll,
          aes(color = dens),
          linewidth = 1) +
  geom_sf(data = p_inicio,
          shape = 21,
          fill = NA,
          color = "black",
          size = 2) +
  geom_sf(data = p_fim,
          shape = 21,
          fill = NA,
          color = "black",
          size = 2) +
  scale_color_viridis_c(option = "inferno",
                        direction = -1,
                        name = "density") +
  scale_x_continuous(breaks = seq(-50, -35, by = 5))+
  coord_sf(xlim = c(-53,-34.5), ylim = c(-30, -3.5),
           expand = FALSE
  ) +
  theme_minimal() +
  labs(
    title = paste0("October and November 2022"),
    x = "",
    y = ""
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right",
    legend.justification = c("right", "top"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.4, "cm"),
    legend.spacing.y = unit(0.1, "cm"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA)
  )+
  labs(title = paste0("October and November 2022 ", titulo_IQR15)) +
  theme(plot.title = element_markdown(hjust = 0.5))

brazil_oct_nov_2022

library(patchwork)
brazil_MME_5 <- brazil_oct_nov_2022  +
  inset_element(
    brazil_oct_nov_2022_n,
    left = 0.75,
    bottom = -0.05,
    right = 1.85,
    top = 0.65) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = "white", color = NA)
    )
  )

brazil_MME_5

evento_pico <- evento %>%
  filter(dist_km_dom >= x_inicio &
           dist_km_dom <= x_fim)

dens <- density(evento_pico$dist_km_dom, bw = 50)
plot(dens)

levels <- seq(0.50, 1, by = 0.05)

results_table_MME_5 <- do.call(rbind,
                               lapply(levels, function(l)
                                 calc_isopleth_metrics(
                                   dens_obj = dens,
                                   evento_df = evento_pico,
                                   coast_line = main_coast_dom,
                                   level = l,
                                   event_name = "October and November 2022"
                                 )
                               )
)

results_table_MME_5

iso95 <- results_table_MME_5 %>%
  filter(isopleth == 95)

min95 <- iso95$km_min
max95 <- iso95$km_max

evento_95 <- evento %>%
  filter(dist_km_dom >= min95 &
           dist_km_dom <= max95)

analise_changepoint(evento_95, ano = 2022, mes = 10, medias_mensais)
analise_changepoint(evento_95, ano = 2022, mes = 11, medias_mensais)

summary_epidemio <- ddply(evento_95, c("Species"), summarise,
                          n = length(Species),
                          fem = sum(Female, na.rm = TRUE),
                          male = sum(Male, na.rm = TRUE),
                          juvenile = sum(Juvenile, na.rm = TRUE),
                          adult = sum(Adult, na.rm = TRUE),
                          alive = sum(Alive, na.rm = TRUE),
                          dead = sum(Dead, na.rm = TRUE))

summary_epidemio$SexNA <- summary_epidemio$n - summary_epidemio$fem - summary_epidemio$male
summary_epidemio$StageNA <- summary_epidemio$n - summary_epidemio$adult - summary_epidemio$juvenile
summary_epidemio$sex<- paste(summary_epidemio$fem, summary_epidemio$male, summary_epidemio$SexNA, sep = "/")
summary_epidemio$stage<- paste(summary_epidemio$juvenile, summary_epidemio$adult, summary_epidemio$StageNA, sep = "/")
summary_epidemio$condition <- paste(summary_epidemio$alive, summary_epidemio$dead, sep="/")

tot_n        <- sum(summary_epidemio$n)
tot_fem      <- sum(summary_epidemio$fem)
tot_male     <- sum(summary_epidemio$male)
tot_SexNA    <- sum(summary_epidemio$SexNA)

tot_juvenile <- sum(summary_epidemio$juvenile)
tot_adult    <- sum(summary_epidemio$adult)
tot_StageNA  <- sum(summary_epidemio$StageNA)

tot_alive    <- sum(summary_epidemio$alive)
tot_dead     <- sum(summary_epidemio$dead)

linha_total <- data.frame(
  Species = "Total",
  n = tot_n,
  sex = paste0("(", tot_fem, "/", tot_male, "/", tot_SexNA, ")"),
  stage = paste0("(", tot_juvenile, "/", tot_adult, "/", tot_StageNA, ")"),
  condition = paste0("(", tot_alive, "/", tot_dead, ")")
)

summary_epidemio_clean <- summary_epidemio[, !(names(summary_epidemio) %in% 
                                                 c("fem", "male", "juvenile",
                                                   "adult", "alive", "dead",
                                                   "SexNA", "StageNA"))]

summary_epidemio_final <- rbind(summary_epidemio_clean, linha_total)

#### Brazil - 2023-05 - MME 6 ####

p <- analise_changepoint(data_temporal, ano = 2023, mes = 5, medias_mensais)

segmentos_destacados <- data.frame(
  xmin = as.Date(c("2023-05-18")),
  xmax = as.Date(c("2023-05-25"))
)

xmin_idx <- which(ocorrencias_por_dia$Date == segmentos_destacados$xmin)
xmax_idx <- which(ocorrencias_por_dia$Date == segmentos_destacados$xmax)

changepoint_MME_8 <- p +
  annotate(
    "rect",
    xmin = xmin_idx,
    xmax = xmax_idx,
    ymin = -Inf,
    ymax = Inf,
    fill = "red",
    alpha = 0.05
  )+
  labs(title = paste0("May 2023 ", titulo_IQR1)) +
  theme(plot.title = element_markdown(hjust = 0.5))

changepoint_MME_8

evento <- dados_filtrados %>%
  filter(Date >= segmentos_destacados$xmin &
           Date <= segmentos_destacados$xmax)

evento <- evento %>%
  filter(!is.na(Long),
         !is.na(Lat))

summary(evento$dist_km_dom)

dens <- density(evento$dist_km_dom, bw = 50)

plot(dens,
     main = paste0("Kernel density along coast - May 2023\nBandwidth = ",
                   round(dens$bw, 2), " km"),
     xlab = "Distance along coast (km)")

dens_df <- data.frame(
  x = dens$x,
  y = dens$y
)

pico_idx <- which.max(dens_df$y)
pico_y   <- dens_df$y[pico_idx]

limiar <- 0.05 * pico_y

left_idx <- max(which(dens_df$y[1:pico_idx] < limiar))

right_idx <- pico_idx + 
  min(which(dens_df$y[pico_idx:length(dens_df$y)] < limiar)) - 1

x_inicio <- dens_df$x[left_idx]
x_fim    <- dens_df$x[right_idx]

x_inicio
x_fim

density_MME_6 <- ggplot(dens_df, aes(x = x, y = y)) +
  geom_line(color = "black", linewidth = 0.4) +
  geom_rug(data = evento,
           aes(x = dist_km_dom),
           sides = "b",
           inherit.aes = FALSE) +
  geom_vline(xintercept = c(x_inicio, x_fim),
             color = "red",
             linewidth = 0.4,
             lty= 2) +
  geom_hline(yintercept = 0,
             color = "grey70",
             linewidth = 0.4) +
  labs(
    title = paste0("May 2023"),
    x = NULL,
    y = NULL
  ) +
  scale_x_continuous(
    expand = expansion(mult = 0.04),
    breaks = seq(
      floor(min(dens_df$x) / 500) * 500,
      ceiling(max(dens_df$x) / 500) * 500,
      by = 500
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.04, 0.04))
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background  = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", fill = NA),
    panel.grid = element_blank(),
    axis.line = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5)
  )+
  labs(title = paste0("May 2023 ", titulo_IQR1)) +
  theme(plot.title = element_markdown(hjust = 0.5))

density_MME_6

brazil_may_2023_n <- ggplot(ocorrencias_por_dia, aes(x = Date, y = ocorrencias)) +
  geom_rect(data = segmentos_destacados,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "skyblue", alpha = 0.4, inherit.aes = FALSE) +
  geom_line(color = "red", size = 1.0) +
  geom_point(color = "red", size = 1.5) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
  labs(title = "", x = "", y = "") +
  theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(size = 9, hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA)
  )

brazil_may_2023_n

res_kde <- calc_kde_coast(evento, main_coast_dom)

pts_ll <- res_kde$pts_ll

line_sfc <- st_geometry(main_coast_dom)

total_length_km <- as.numeric(st_length(line_sfc)) / 1000

prop_min <- x_inicio / total_length_km
prop_max <- x_fim / total_length_km

p_inicio <- st_line_sample(line_sfc, sample = prop_min)
p_fim    <- st_line_sample(line_sfc, sample = prop_max)

p_inicio <- st_cast(p_inicio, "POINT") |> st_sf()
p_fim    <- st_cast(p_fim, "POINT") |> st_sf()

p_inicio <- st_transform(p_inicio, 4326)
p_fim    <- st_transform(p_fim, 4326)

p_inicio <- evento %>%
  slice(which.min(abs(dist_km_dom - x_inicio))) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

p_fim <- evento %>%
  slice(which.min(abs(dist_km_dom - x_fim))) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

brazil_may_2023 <- ggplot() +
  geom_sf(data = brasil, fill = "white", color = "black", size = 0.75) +
  geom_sf(data = regioes_sf,
          aes(fill = regiao),
          color = NA,
          alpha = 0.25) +
  annotate("text",
           x = -48, y = -29,
           label = paste0("bw = ", round(dens$bw, 2), " km"),
           hjust = 0,
           size = 2.5) +
  annotate("text", x = -41, y = -9,
           label = "NE",
           fontface = "italic",
           size = 3,
           color = "grey30") +
  annotate("text", x = -45, y = -19,
           label = "SE",
           fontface = "italic",
           size = 3,
           color = "grey30") +
  annotate("text", x = -51, y = -26,
           label = "S",
           fontface = "italic",
           size = 3,
           color = "grey30") +
  geom_sf(data = pts_ll,
          aes(color = dens),
          linewidth = 1) +
  geom_sf(data = p_inicio,
          shape = 21,
          fill = NA,
          color = "black",
          size = 2) +
  geom_sf(data = p_fim,
          shape = 21,
          fill = NA,
          color = "black",
          size = 2) +
  scale_fill_manual(values = c("Sul" = "#2C7BB6",
                               "Sudeste" = "#4DAF4A",
                               "Nordeste" = "#984EA3")) +
  scale_color_viridis_c(option = "inferno",
                        direction = -1,
                        name = "density") +
  guides(fill = "none") +
  scale_x_continuous(breaks = seq(-50, -35, by = 5))+
  coord_sf(xlim = c(-53,-34.5), ylim = c(-30, -3.5),
           expand = FALSE
  ) +
  theme_minimal() +
  labs(
    title = "May 2023",
    x = "",
    y = ""
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right",
    legend.justification = c("right", "top"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.4, "cm"),
    legend.spacing.y = unit(0.1, "cm"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA)
  )+
  labs(title = paste0("May 2023 ", titulo_IQR1)) +
  theme(plot.title = element_markdown(hjust = 0.5))

brazil_may_2023

library(patchwork)
brazil_MME_6 <- brazil_may_2023 +
  inset_element(
    brazil_may_2023_n,
    left = 0.75,
    bottom = -0.05,
    right = 1.6,
    top = 0.60
  ) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = "white", color = NA)
    )
  )

brazil_MME_6

evento_pico <- evento %>%
  filter(dist_km_dom >= x_inicio &
           dist_km_dom <= x_fim)

dens <- density(evento_pico$dist_km_dom, bw = 50)
plot(dens)

levels <- seq(0.50, 0.95, by = 0.05)

results_table_MME_6 <- do.call(rbind,
                          lapply(levels, function(l)
                            calc_isopleth_metrics(
                              dens_obj = dens,
                              evento_df = evento_pico,
                              coast_line = main_coast_dom,
                              level = l,
                              event_name = "May 2023"
                            )
                          )
)

results_table_MME_6


iso95 <- results_table_MME_6 %>%
  filter(isopleth == 95)

min95 <- iso95$km_min
max95 <- iso95$km_max

evento_95 <- evento %>%
  filter(dist_km_dom >= min95 &
           dist_km_dom <= max95)

analise_changepoint(evento_95, ano = 2023, mes = 5, medias_mensais)

summary_epidemio <- ddply(evento_95, c("Species"), summarise,
                          n = length(Species),
                          fem = sum(Female, na.rm = TRUE),
                          male = sum(Male, na.rm = TRUE),
                          juvenile = sum(Juvenile, na.rm = TRUE),
                          adult = sum(Adult, na.rm = TRUE),
                          alive = sum(Alive, na.rm = TRUE),
                          dead = sum(Dead, na.rm = TRUE))

summary_epidemio$SexNA <- summary_epidemio$n - summary_epidemio$fem - summary_epidemio$male
summary_epidemio$StageNA <- summary_epidemio$n - summary_epidemio$adult - summary_epidemio$juvenile
summary_epidemio$sex<- paste(summary_epidemio$fem, summary_epidemio$male, summary_epidemio$SexNA, sep = "/")
summary_epidemio$stage<- paste(summary_epidemio$juvenile, summary_epidemio$adult, summary_epidemio$StageNA, sep = "/")
summary_epidemio$condition <- paste(summary_epidemio$alive, summary_epidemio$dead, sep="/")

tot_n        <- sum(summary_epidemio$n)
tot_fem      <- sum(summary_epidemio$fem)
tot_male     <- sum(summary_epidemio$male)
tot_SexNA    <- sum(summary_epidemio$SexNA)

tot_juvenile <- sum(summary_epidemio$juvenile)
tot_adult    <- sum(summary_epidemio$adult)
tot_StageNA  <- sum(summary_epidemio$StageNA)

tot_alive    <- sum(summary_epidemio$alive)
tot_dead     <- sum(summary_epidemio$dead)

linha_total <- data.frame(
  Species = "Total",
  n = tot_n,
  sex = paste0("(", tot_fem, "/", tot_male, "/", tot_SexNA, ")"),
  stage = paste0("(", tot_juvenile, "/", tot_adult, "/", tot_StageNA, ")"),
  condition = paste0("(", tot_alive, "/", tot_dead, ")")
)

summary_epidemio_clean <- summary_epidemio[, !(names(summary_epidemio) %in% 
                                                 c("fem", "male", "juvenile",
                                                   "adult", "alive", "dead",
                                                   "SexNA", "StageNA"))]

summary_epidemio_final <- rbind(summary_epidemio_clean, linha_total)

#### Brazil - 2024-05 and 06 - MME 7 ####

p <- analise_changepoint(data_temporal, ano = 2024, mes = 5, medias_mensais)

segmentos_destacados <- data.frame(
  xmin = as.Date(c("2024-05-21")),
  xmax = as.Date(c("2024-05-31"))
)

xmin_idx <- which(ocorrencias_por_dia$Date == segmentos_destacados$xmin)
xmax_idx <- which(ocorrencias_por_dia$Date == segmentos_destacados$xmax)

changepoint_MME_9 <- p + annotate("rect",
                                  xmin = xmin_idx,
                                  xmax = xmax_idx,
                                  ymin = -Inf,
                                  ymax = Inf,
                                  fill = "red",
                                  alpha = 0.05)+
  labs(title = paste0("May 2024 ", titulo_IQR3)) +
  theme(plot.title = element_markdown(hjust = 0.5))

changepoint_MME_9

p <- analise_changepoint(data_temporal, ano = 2024, mes = 6, medias_mensais)

segmentos_destacados <- data.frame(
  xmin = as.Date(c("2024-06-01")),
  xmax = as.Date(c("2024-06-14"))
)

xmin_idx <- which(ocorrencias_por_dia$Date == segmentos_destacados$xmin)
xmax_idx <- which(ocorrencias_por_dia$Date == segmentos_destacados$xmax)

changepoint_MME_10 <- p + annotate("rect",
                                  xmin = xmin_idx,
                                  xmax = xmax_idx,
                                  ymin = -Inf,
                                  ymax = Inf,
                                  fill = "red",
                                  alpha = 0.05)+
  labs(title = paste0("June 2024 ", titulo_IQR3)) +
  theme(plot.title = element_markdown(hjust = 0.5))

changepoint_MME_10

dados_filtrados <- data_temporal %>%
  filter(Year == 2024,
         Month %in% c(5, 6))

dados_filtrados$Date <- as.Date(dados_filtrados$Date)

ocorrencias_por_dia <- dados_filtrados %>%
  group_by(Date) %>%
  summarise(ocorrencias = n())

todas_datas <- data.frame(Date = seq(as.Date("2024-05-01"), as.Date("2024-06-30"), by = "day"))

ocorrencias_por_dia <- todas_datas %>%
  left_join(ocorrencias_por_dia, by = "Date") %>%
  mutate(ocorrencias = replace_na(ocorrencias, 0))

segmentos_destacados <- data.frame(
  xmin = as.Date(c("2024-05-21")),
  xmax = as.Date(c("2024-06-14"))
)

evento <- dados_filtrados %>%
  filter(Date >= segmentos_destacados$xmin &
           Date <= segmentos_destacados$xmax)

evento <- evento %>%
  filter(!is.na(Long),
         !is.na(Lat))

summary(evento$dist_km_dom)

dens <- density(evento$dist_km_dom, bw = 50)

plot(dens,
     main = paste0("Kernel density along coast - May and June 2024\nBandwidth = ",
                   round(dens$bw, 2), " km"),
     xlab = "Distance along coast (km)")

dens_df <- data.frame(
  x = dens$x,
  y = dens$y
)

pico_idx <- which.max(dens_df$y)
pico_y   <- dens_df$y[pico_idx]

limiar <- 0.05 * pico_y

left_idx <- max(which(dens_df$y[1:pico_idx] < limiar))

right_idx <- pico_idx + 
  min(which(dens_df$y[pico_idx:length(dens_df$y)] < limiar)) - 1

x_inicio <- dens_df$x[left_idx]
x_fim    <- dens_df$x[right_idx]

x_inicio
x_fim

density_MME_7 <- ggplot(dens_df, aes(x = x, y = y)) +
  geom_line(color = "black", linewidth = 0.4) +
  geom_rug(data = evento,
           aes(x = dist_km_dom),
           sides = "b",
           inherit.aes = FALSE) +
  geom_vline(xintercept = c(x_inicio, x_fim),
             color = "red",
             linewidth = 0.4,
             lty= 2) +
  geom_hline(yintercept = 0,
             color = "grey70",
             linewidth = 0.4) +
  labs(
    title = paste0("May and June 2024"),
    x = NULL,
    y = NULL
  ) +
  scale_x_continuous(
    expand = expansion(mult = 0.04),
    breaks = seq(
      floor(min(dens_df$x) / 500) * 500,
      ceiling(max(dens_df$x) / 500) * 500,
      by = 500
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.04, 0.04))
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background  = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", fill = NA),
    panel.grid = element_blank(),
    axis.line = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.text.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5)
  )+
  labs(title = paste0("May and June 2024 ", titulo_IQR3)) +
  theme(plot.title = element_markdown(hjust = 0.5))

density_MME_7

brazil_may_june_2024_n <- ggplot(ocorrencias_por_dia, aes(x = Date, y = ocorrencias)) +
  geom_rect(data = segmentos_destacados,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "skyblue", alpha = 0.4, inherit.aes = FALSE) +
  geom_line(color = "red", size = 1.0) +
  geom_point(color = "red", size = 1.5) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
  labs(title = "", x = "", y = "") +
  theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(size = 9, hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA)
  )

brazil_may_june_2024_n

res_kde <- calc_kde_coast(evento, main_coast_dom)

pts_ll <- res_kde$pts_ll

line_sfc <- st_geometry(main_coast_dom)

total_length_km <- as.numeric(st_length(line_sfc)) / 1000

prop_min <- x_inicio / total_length_km
prop_max <- x_fim / total_length_km

p_inicio <- st_line_sample(line_sfc, sample = prop_min)
p_fim    <- st_line_sample(line_sfc, sample = prop_max)

p_inicio <- st_cast(p_inicio, "POINT") |> st_sf()
p_fim    <- st_cast(p_fim, "POINT") |> st_sf()

p_inicio <- st_transform(p_inicio, 4326)
p_fim    <- st_transform(p_fim, 4326)

p_inicio <- evento %>%
  slice(which.min(abs(dist_km_dom - x_inicio))) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

p_fim <- evento %>%
  slice(which.min(abs(dist_km_dom - x_fim))) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

brazil_may_june_2024 <- ggplot() +
  geom_sf(data = brasil, fill = "white", color = "black", size = 0.75) +
  geom_sf(data = regioes_sf,
          aes(fill = regiao),
          color = NA,
          alpha = 0.25) +
  annotate("text", x = -41, y = -9,
           label = "NE",
           fontface = "italic",
           size = 3,
           color = "grey30") +
  annotate("text", x = -45, y = -19,
           label = "SE",
           fontface = "italic",
           size = 3,
           color = "grey30") +
  
  annotate("text", x = -51, y = -26,
           label = "S",
           fontface = "italic",
           size = 3,
           color = "grey30") +
  annotate("text",
           x = -48, y = -29,
           label = paste0("bw = ", round(dens$bw, 2), " km"),
           hjust = 0,
           size = 2.5) +
  scale_fill_manual(values = c("Sul" = "#2C7BB6",
                               "Sudeste" = "#4DAF4A",
                               "Nordeste" = "#984EA3")) +
  guides(fill = "none") +
  geom_sf(data = pts_ll,
          aes(color = dens),
          linewidth = 1) +
  geom_sf(data = p_inicio,
          shape = 21,
          fill = NA,
          color = "black",
          size = 2) +
  geom_sf(data = p_fim,
          shape = 21,
          fill = NA,
          color = "black",
          size = 2) +
  scale_color_viridis_c(option = "inferno",
                        direction = -1,
                        name = "density") +
  scale_x_continuous(breaks = seq(-50, -35, by = 5))+
  coord_sf(xlim = c(-53,-34.5), ylim = c(-30, -3.5),
           expand = FALSE
  ) +
  theme_minimal() +
  labs(
    title = paste0("May and June 2024"),
    x = "",
    y = ""
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right",
    legend.justification = c("right", "top"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.4, "cm"),
    legend.spacing.y = unit(0.1, "cm"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA)
  )+
  labs(title = paste0("May and June 2024 ", titulo_IQR3)) +
  theme(plot.title = element_markdown(hjust = 0.5))

brazil_may_june_2024

library(patchwork)
brazil_MME_7 <- brazil_may_june_2024  +
  inset_element(
    brazil_may_june_2024_n,
    left = 0.75,
    bottom = -0.05,
    right = 1.85,
    top = 0.65) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = "white", color = NA)
    )
  )

brazil_MME_7

evento_pico <- evento %>%
  filter(dist_km_dom >= x_inicio &
           dist_km_dom <= x_fim)

dens <- density(evento_pico$dist_km_dom, bw = 50)
plot(dens)

levels <- seq(0.50, 0.95, by = 0.05)

results_table_MME_7 <- do.call(rbind,
                               lapply(levels, function(l)
                                 calc_isopleth_metrics(
                                   dens_obj = dens,
                                   evento_df = evento_pico,
                                   coast_line = main_coast_dom,
                                   level = l,
                                   event_name = "May and June 2024"
                                 )
                               )
)

results_table_MME_7

iso95 <- results_table_MME_7 %>%
  filter(isopleth == 95)

min95 <- iso95$km_min
max95 <- iso95$km_max

evento_95 <- evento %>%
  filter(dist_km_dom >= min95 &
           dist_km_dom <= max95)

analise_changepoint(evento_95, ano = 2024, mes = 5, medias_mensais)
analise_changepoint(evento_95, ano = 2024, mes = 6, medias_mensais)

summary_epidemio <- ddply(evento_95, c("Species"), summarise,
                          n = length(Species),
                          fem = sum(Female, na.rm = TRUE),
                          male = sum(Male, na.rm = TRUE),
                          juvenile = sum(Juvenile, na.rm = TRUE),
                          adult = sum(Adult, na.rm = TRUE),
                          alive = sum(Alive, na.rm = TRUE),
                          dead = sum(Dead, na.rm = TRUE))

summary_epidemio$SexNA <- summary_epidemio$n - summary_epidemio$fem - summary_epidemio$male
summary_epidemio$StageNA <- summary_epidemio$n - summary_epidemio$adult - summary_epidemio$juvenile
summary_epidemio$sex<- paste(summary_epidemio$fem, summary_epidemio$male, summary_epidemio$SexNA, sep = "/")
summary_epidemio$stage<- paste(summary_epidemio$juvenile, summary_epidemio$adult, summary_epidemio$StageNA, sep = "/")
summary_epidemio$condition <- paste(summary_epidemio$alive, summary_epidemio$dead, sep="/")

tot_n        <- sum(summary_epidemio$n)
tot_fem      <- sum(summary_epidemio$fem)
tot_male     <- sum(summary_epidemio$male)
tot_SexNA    <- sum(summary_epidemio$SexNA)

tot_juvenile <- sum(summary_epidemio$juvenile)
tot_adult    <- sum(summary_epidemio$adult)
tot_StageNA  <- sum(summary_epidemio$StageNA)

tot_alive    <- sum(summary_epidemio$alive)
tot_dead     <- sum(summary_epidemio$dead)

linha_total <- data.frame(
  Species = "Total",
  n = tot_n,
  sex = paste0("(", tot_fem, "/", tot_male, "/", tot_SexNA, ")"),
  stage = paste0("(", tot_juvenile, "/", tot_adult, "/", tot_StageNA, ")"),
  condition = paste0("(", tot_alive, "/", tot_dead, ")")
)

summary_epidemio_clean <- summary_epidemio[, !(names(summary_epidemio) %in% 
                                                 c("fem", "male", "juvenile",
                                                   "adult", "alive", "dead",
                                                   "SexNA", "StageNA"))]

summary_epidemio_final <- rbind(summary_epidemio_clean, linha_total)

#### final plots ####
library(ggplot2)
library(patchwork)
library(grid)

#changepoint
y_label <- wrap_elements(
  grid::textGrob("Number of strandings", rot = 90, gp = gpar(fontsize = 13))
)

painel <- (changepoint_MME_1 | changepoint_brazil_1 | changepoint_brazil_2 | changepoint_MME_2) /
  (changepoint_MME_3 | changepoint_MME_4 | changepoint_MME_5 | changepoint_brazil_3) /
  (changepoint_MME_6 | changepoint_MME_7 | changepoint_MME_8 | changepoint_MME_9) /
  (changepoint_MME_10 | plot_spacer() | plot_spacer() | plot_spacer())

final_plot_changepoint <- (y_label | painel) +
  plot_layout(widths = c(0.03, 1), guides = "collect") +
  plot_annotation(caption = "Days") &
  theme(
    plot.caption = element_text(hjust = 0.5, size = 13),
    legend.position = "right",
    legend.justification = c("right", "top"),
    legend.margin = margin(0, 0, 0, 0),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    legend.key.size = unit(0.8, "cm"),
    legend.key.height = unit(0.8, "cm"),
    legend.key.width = unit(0.8, "cm"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA)
  )

final_plot_changepoint

#density

y_label_density <- wrap_elements(
  grid::textGrob("Density", rot = 90, gp = gpar(fontsize = 13))
)

painel_density <- (density_MME_1 | density_brazil_1 | density_MME_2) /
  (density_MME_3 | density_MME_4 | density_brazil_2 ) /
  (density_MME_5 | density_MME_6 | density_MME_7 )

final_plot_density <- (y_label_density | painel_density) +
  plot_layout(widths = c(0.03, 1), guides = "collect") +
  plot_annotation(caption = "Distance along the coast (km)") &
  theme(
    plot.caption = element_text(hjust = 0.5, size = 13),
    legend.position = "right",
    legend.justification = c("right", "top"),
    legend.margin = margin(0, 0, 0, 0),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    legend.key.size = unit(0.8, "cm"),
    legend.key.height = unit(0.8, "cm"),
    legend.key.width = unit(0.8, "cm"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA)
  )

final_plot_density

#MMEs

final_plot_MME <- (brazil_MME_1 | brazil_plot1 | brazil_MME_2) /
  (brazil_MME_3 | brazil_MME_4 | brazil_plot2) /
  (brazil_MME_5 | brazil_MME_6 | brazil_MME_7) +
  plot_layout(
    heights = c(1, 1, 1),   
    widths = c(1, 1, 1)     
  ) &
  theme(
    legend.position = "right",
    legend.justification = c("right", "top"),
    legend.margin = margin(0, 0, 0, 60),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.4, "cm"),
    legend.key.height = unit(0.4, "cm"),
    legend.key.width = unit(0.4, "cm"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA)
  )

final_plot_MME


#### Hotspots analysis ####

make_hotspot_plot <- function(data, isopleth_filter = NULL) {
  
  evento_nome <- unique(data$event)
  
  p <- ggplot(data, aes(x = isopleth, y = aves_por_km)) +
    
    geom_point(aes(color = "Observed density"), size = 2) +
    geom_line(aes(color = "Observed density"), linewidth = 0.8) +
    
    scale_color_manual(
      name = "",
      values = c("Observed density" = "black")
    ) +
    
    labs(
      x = "Isopleth (%)",
      y = "Density (birds/km)",
      title = evento_nome
    ) +
    
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(hjust = 0.5),
      legend.position = c(0.98, 0.98),
      legend.justification = c(1, 1),
      legend.background = element_rect(fill = "white", color = NA)
    )
  
  if (!is.null(isopleth_filter)) {
    hotspot_row <- data %>%
      filter(isopleth == isopleth_filter)
  } else {
    hotspot_row <- data %>%
      slice(1)
  }
  
  return(list(plot = p,
              hotspot = hotspot_row))
}

h1 <- make_hotspot_plot(results_table_MME_1, 50)
h2 <- make_hotspot_plot(results_table_MME_2, 50)
h3 <- make_hotspot_plot(results_table_MME_3_1, 50)
h4 <- make_hotspot_plot(results_table_MME_3_2, 50)
h5 <- make_hotspot_plot(results_table_MME_4, 50)
h6 <- make_hotspot_plot(results_table_MME_5, 50)
h7 <- make_hotspot_plot(results_table_MME_6, 50)
h8 <- make_hotspot_plot(results_table_MME_7, 50)

hotspots_all <- bind_rows(
  h1$hotspot,
  h2$hotspot,
  h3$hotspot,
  h4$hotspot,
  h5$hotspot,
  h6$hotspot,
  h7$hotspot,
  h8$hotspot
)

#hotspots plot

library(sf)
library(dplyr)
library(lwgeom)
library(ggplot2)


coast_proj <- st_transform(main_coast_dom, 5880)
total_length <- as.numeric(st_length(coast_proj))

lines_list <- lapply(seq_len(nrow(hotspots_all)), function(i) {
  
  prop_min <- (hotspots_all$km_min[i] * 1000) / total_length
  prop_max <- (hotspots_all$km_max[i] * 1000) / total_length
  
  seg <- st_linesubstring(
    coast_proj,
    from = prop_min,
    to   = prop_max
  )
  
  st_geometry(seg)[[1]]
})

hotspots_sf <- st_sf(
  hotspots_all,
  geometry = st_sfc(lines_list, crs = 5880)
)

event_colors <- c(
  "June 2017"                  = "#F94144",  
  "May 2023"                   = "#9D4EDD",  
  "May and June 2024"          = "#F8961E",  
  "May 2019 - 1"               = "#F15BB5",  
  "May 2019 - 2"               = "#F15BB5",  
  "September 2018"             = "#9C6644",  
  "September and October 2020" = "#3A86FF",  
  "October and November 2022"  = "#43AA8B"   
)

offset_table <- data.frame(
  event = c("June 2017",
            "May 2023",
            "May and June 2024",
            "May 2019 - 1",
            "May 2019 - 2",
            "September 2018",
            "September and October 2020",
            "October and November 2022"),
  
  dx = c( 0,        
          25000,    
          50000,    
          0,        
          0,
          25000,    
          50000,    
          75000   
          ),   
  
  dy = c( 0,
          -25000,
          -50000,
          0,
          0,
          -25000,
          -50000,
          -75000)
)

unique(hotspots_sf$event)

hotspots_proj <- hotspots_sf %>%
  st_transform(5880) %>%
  left_join(offset_table, by = "event")

lines_list <- lapply(seq_len(nrow(hotspots_proj)), function(i) {
  
  coords <- st_coordinates(hotspots_proj$geometry[i])[,1:2]
  
  coords[,1] <- coords[,1] + hotspots_proj$dx[i]
  coords[,2] <- coords[,2] + hotspots_proj$dy[i]
  
  st_linestring(coords)
})

hotspots_manual <- st_sf(
  hotspots_proj,
  geometry = st_sfc(lines_list, crs = 5880)
)

hotspots_final <- st_transform(hotspots_manual, 4326)

hotspots_final <- hotspots_final %>%
  mutate(event_legend = case_when(
    event %in% c("May 2019 - 1", "May 2019 - 2") ~ "May 2019",
    TRUE ~ event
  ))

hotspots_brazil <- ggplot() +
                      geom_sf(data = brasil, fill = "white", color = "black", size = 1) +
                      
                      geom_sf(data = regioes_sf,
                              aes(fill = regiao),
                              color = NA,
                              alpha = 0.25) +
                      annotate("text", x = -41, y = -9,
                               label = "NE",
                               fontface = "italic",
                               size = 4,
                               color = "grey30") +
                      annotate("text", x = -45, y = -19,
                               label = "SE",
                               fontface = "italic",
                               size = 4,
                               color = "grey30") +
                      
                      annotate("text", x = -51, y = -26,
                               label = "S",
                               fontface = "italic",
                               size = 4,
                               color = "grey30") +
                      scale_fill_manual(values = c("Sul" = "#2C7BB6",
                                                   "Sudeste" = "#4DAF4A",
                                                   "Nordeste" = "#984EA3")) +
                      guides(fill = "none") +
                      
  geom_sf(
    data = hotspots_final,
    aes(color = event_legend),
    linewidth = 2,
    size = 2,
    alpha = 1,
    lineend = "round",
    linejoin = "round"
  ) +
                      
  scale_color_manual(
    values = c(
      "June 2017" = "#F94144",
      "May 2023" = "#9D4EDD",
      "May and June 2024" = "#F8961E",
      "May 2019" = "#F15BB5",   
      "September 2018" = "#9C6644",
      "September and October 2020" = "#3A86FF",
      "October and November 2022" = "#43AA8B"
    )
  ) +
                      
                      coord_sf(xlim = c(-53,-34.5), ylim = c(-30, -3.5),
                               expand = FALSE
                      ) +
                      
                      labs(color = "Hotspot extent") +
                      
                      theme_minimal() +
                      
                      theme(
                        legend.justification = c("right", "bottom"),
                        legend.background = element_blank(),
                        legend.key = element_blank(),
                        legend.title = element_text(size = 12),
                        legend.text  = element_text(size = 12),
                        panel.grid = element_blank(),
                      )
                    
hotspots_brazil

#### bw test ####

x <- data_temporal$dist_km_dom
x <- x[!is.na(x)]

bw_nrd0 <- bw.nrd0(x)   # Silverman / normal reference
bw_ucv  <- bw.ucv(x)    # unbiased cross-validation
bw_bcv  <- bw.bcv(x)    # biased cross-validation

bw_nrd0
bw_ucv
bw_bcv

bw_vals <- seq(10,80,10)

par(mfrow=c(4,2))

for(bw in bw_vals){
  plot(density(x, bw=bw),
       main=paste("bw =", bw,"km"),
       xlab="Distance along coast (km)")
}

x <- data_temporal$dist_km_dom

x <- x[!is.na(x)]

count_peaks <- function(dens){
  sum(diff(sign(diff(dens$y))) == -2)
}

bw_vals <- seq(10,120,5)

n_peaks <- numeric(length(bw_vals))

for(i in seq_along(bw_vals)){
  
  dens <- density(x, bw=bw_vals[i])
  
  n_peaks[i] <- count_peaks(dens)
}

par(mfrow=c(1,1))

plot(bw_vals, n_peaks,
     type="b",
     pch=19,
     xlab="Bandwidth (km)",
     ylab="Number of peaks",
     main="Scale-space analysis")


