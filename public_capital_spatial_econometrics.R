#setwd("C:/Users/DIDIER/Downloads/Paper")


#0.######### LOADING LIBRARIES #########
# Basic data manipulation packages
library(dplyr)
library(tidyr)
library(readr)
library(tidyverse)

# Plotting packages
library(ggplot2)
library(ggpubr)
library(cowplot)
library(ggspatial)
#library(ggview)

# Spatial analysis packages
library(sp)
library(sf)
library(spdep)
library(splm)
library(spatialreg)
library(rgeoda)

# Statistical and model analysis packages
library(plm)
library(fixest)
library(ivreg)
library(summarytools)
library(modelsummary)
library(outreg)
library(stargazer)
library(texreg)
library(biscale)

# Tables and results packages
library(gt)
library(kableExtra)
library(writexl)
library(openxlsx)
library(xml2)
library(rvest)

# Additional color and visualization packages
library(RColorBrewer)
library(classInt)
library(lattice)
library(tidylog)

# Additional data visualization package
library(Ecdat)
library(ggview)

#1. ######### LOADING DATA #########
data(Produc, package = "plm")
data(usaww)
Producp <- pdata.frame(Produc, c("state","year"))

#1.0 DATA CLEANING AND VARIABLE CONSTRUCTION
Producp$state <- gsub("_", " ", Producp$state)
Producp$state <- gsub("TENNESSE", "TENNESSEE", Producp$state)

Producp$log_pcap       <- log(Producp$pcap)
Producp$L1_log_pcap    <- plm::lag(Producp$log_pcap, 1)
Producp$L2_log_pcap    <- plm::lag(Producp$log_pcap, 2)
Producp$L3_log_pcap    <- plm::lag(Producp$log_pcap, 3)
Producp$L4_log_pcap    <- plm::lag(Producp$log_pcap, 4)

Producp$D1_log_pcap    <- Producp$log_pcap - Producp$L1_log_pcap
Producp$L1_D1_log_pcap <- plm::lag(Producp$D1_log_pcap, 1)
Producp$L2_D1_log_pcap <- plm::lag(Producp$D1_log_pcap, 2)

Producp$log_hwy       <- log(Producp$hwy)
Producp$L1_log_hwy    <- plm::lag(Producp$log_hwy, 1)
Producp$L2_log_hwy    <- plm::lag(Producp$log_hwy, 2)
Producp$D1_log_hwy    <- Producp$log_hwy - Producp$L1_log_hwy
Producp$L1_D1_log_hwy <- plm::lag(Producp$D1_log_hwy, 1)
Producp$L2_D1_log_hwy <- plm::lag(Producp$D1_log_hwy, 2)

Producp$log_water       <- log(Producp$water)
Producp$L1_log_water    <- plm::lag(Producp$log_water, 1)
Producp$L2_log_water    <- plm::lag(Producp$log_water, 2)
Producp$D1_log_water    <- Producp$log_water - Producp$L1_log_water
Producp$L1_D1_log_water <- plm::lag(Producp$D1_log_water, 1)
Producp$L2_D1_log_water <- plm::lag(Producp$D1_log_water, 2)

Producp$log_util       <- log(Producp$util)
Producp$L1_log_util    <- plm::lag(Producp$log_util, 1)
Producp$L2_log_util    <- plm::lag(Producp$log_util, 2)
Producp$D1_log_util    <- Producp$log_util - Producp$L1_log_util
Producp$L1_D1_log_util <- plm::lag(Producp$D1_log_util, 1)
Producp$L2_D1_log_util <- plm::lag(Producp$D1_log_util, 2)

Producp$log_gsp       <- log(Producp$gsp)
Producp$L1_log_gsp    <- plm::lag(Producp$log_gsp, 1)
Producp$D1_log_gsp    <- Producp$log_gsp - Producp$L1_log_gsp

Producp$log_pc       <- log(Producp$pc)
Producp$L1_log_pc    <- plm::lag(Producp$log_pc, 1)
Producp$D1_log_pc    <- Producp$log_pc - Producp$L1_log_pc

Producp$log_emp       <- log(Producp$emp)
Producp$L1_log_emp    <- plm::lag(Producp$log_emp, 1)
Producp$D1_log_emp    <- Producp$log_emp - Producp$L1_log_emp

Producp$L1_unemp    <- plm::lag(Producp$unemp, 1)
Producp$D1_unemp    <- Producp$unemp - Producp$L1_unemp


summary(na.omit(Producp))
apply(Producp,2,sd)
### Map 1: Spatial distribution of GDP (left) and the stock of public capital (right)


# Loading the map 
us <- st_read("cb_2018_us_state_5m.shp",quiet = T) |> 
  filter(!is.na(STUSPS) & !STUSPS %in% c('AK','HI','GU','AS','PR','VI', 'MP')) |>  # Excluding missing values, Alaska, Hawaii, Guam, American Samoa, Puerto Rico, and the Virgin Islands
  mutate(fipsstat = as.numeric(STATEFP)) |> 
  arrange(fipsstat) |>  
  st_transform(crs = 4326)

names(us)[6]= 'state'


contiguous_fips <- c(
  "01", "04", "05", "06", "08", "09", "10", "12", "13", "16", "17", "18", "19", "20", 
  "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", 
  "35", "36", "37", "38", "39", "40", "41", "42", "44", "45", "46", "47", "48", "49", 
  "50", "51", "53", "54", "55", "56"
)

# Filter the shapefile to include only the contiguous 48 states
us <- us %>%
  filter(STATEFP %in% contiguous_fips)


#Standardizing state names
us$state <- toupper(us$state)



########################################################
#---------------------Mapa 1---------------------------#
########################################################

#-----1976 GSP
# Merging the data with the map using FIPS identifiers
Producp76 <- Producp |> 
  filter(year==1976)

us_merge <- merge(us,Producp76)|> 
  select(STUSPS, fipsstat, state, pcap, gsp)


map1_gsp <- ggplot(us_merge) + 
  geom_sf(aes(fill = gsp), color = "gray95", size = 0.05) +
  geom_sf_text(aes(label = STUSPS), size=1.5, colour = "black") +
  scale_fill_viridis_c(direction = -1) +
  labs(title="", fill="GSP in 1976") +
  theme_void() +
  theme(legend.position = c(0.1, 0.2),
        legend.key.size = unit(0.3,"cm"),
        legend.text=element_text(size=3),
        legend.title=element_text(size=5)) +
  # Add the scale bar
  annotation_scale(
    location = "br", # 'bl' means bottom-left
    width_hint = 0.1 # Controls the relative length of the scale bar
  )+
  # Add the north arrow
  annotation_north_arrow(
    location = "tr", # 'tr' means top-right
    which_north = "true", # Can be "true" or "magnetic" depending on the orientation
    height = unit(0.6, "cm"), # Adjusts the arrow height, e.g., 1 cm
    width = unit(0.4, "cm")   # Adjusts the arrow width, e.g., 1 cm
  )
 
#-----1976 PCAP
 
 
map1_pcap <- ggplot(us_merge) + 
  geom_sf(aes(fill = pcap), color = "gray95", size = 0.05) +
  geom_sf_text(aes(label = STUSPS), size=1.5, colour = "black") +
  scale_fill_viridis_c(direction = -1) +
  labs(title="", fill="PCAP in 1976") +
  theme_void() +
  theme(legend.position = c(.85,.2),
        legend.key.size = unit(0.3,"cm"),
        legend.text=element_text(size=3),
        legend.title=element_text(size=5)) +
  # Add the scale bar
  annotation_scale(
    location = "bl", # 'bl' means bottom-left
    width_hint = 0.1 # Controls the relative length of the scale bar
  )+
  # Add the north arrow
  annotation_north_arrow(
    location = "tr", # 'tr' means top-right
    which_north = "true", # Can be "true" or "magnetic" depending on the orientation
    height = unit(0.6, "cm"), # Adjusts the arrow height, e.g., 1 cm
    width = unit(0.4, "cm")   # Adjusts the arrow width, e.g., 1 cm
  )
 
 
map1 <- ggarrange( map1_gsp, map1_pcap,
                   ncol = 2, nrow = 1) + 
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # White panel background
    plot.background = element_rect(fill = "white", color = NA)    # White plot background
  )
 
print(map1)
 
#ggview(map1, units = "in", width = 14, height = 3, dpi = 300, bg="white")


### Map 2: Bivariate map of GDP and the stock of public capital.

# Create the bivariate map
map2 <- ggplot(us_merge_bi) + 
  geom_sf(aes(fill = bi_class), color = "gray95", size = 0.05) +
  bi_scale_fill(pal = "BlueYl", dim = 3) +  # Bivariate color scale (adjust colors if necessary)
  geom_sf_text(aes(label = STUSPS), size=1.5, colour = "black") +
  labs(title="",
       fill="") +
  theme_void() +
  theme(legend.position = "none") +
  # Add the scale bar
  annotation_scale(
    location = "br", # 'bl' means bottom-left
    width_hint = 0.3 # Controls the relative length of the scale bar
  )+
  # Add the north arrow
  annotation_north_arrow(
    location = "tr", # 'tr' means top-right
    which_north = "true", # Can be "true" or "magnetic" depending on the orientation
    height = unit(1, "cm"), # Adjusts the arrow height, e.g., 1 cm
    width = unit(1, "cm")   # Adjusts the arrow width, e.g., 1 cm
  )

# Create the bivariate legend
legend <- bi_legend(pal = "BlueYl",
                    dim = 3,
                    xlab = "Gross state product",   # X-axis label
                    ylab = "Public capital",           # Y-axis label
                    size = 8)                          # Legend size

# Combine the map and legend using plot_grid()
final_map2 <- plot_grid(
  legend, map2,  # Legend first, then map
  ncol = 2,                  # Arrange in two columns
  rel_widths = c(0.3, 1)     # Adjust relative widths of the legend and map
)

# Display the final plot
print(final_map2)
#ggview(final_map2, units = "in", width = 7, height = 4, dpi = 300, bg="white")


### Map 3: Local Indicators of Spatial Association (LISA) for GDP (left) and public capital (right)

#For GSP
Producp76 <- Producp |> 
  filter(year==1976)

us_merge <- merge(us,Producp76)|> 
  select(STUSPS, fipsstat, state, pcap, gsp)

us_merge <- us_merge |>  
  mutate(lgsp=log(us_merge$gsp))



w <- rgeoda::queen_weights(us_merge, order = 1)
lisa <- local_moran(w, us_merge["lgsp"], # Select only one data column
                    permutations = 999,
                    permutation_method = "complete",
                    significance_cutoff = 0.05,
                    cpu_threads = 2,
                    seed = 123456789)
us_merge$cluster = as.factor(lisa$GetClusterIndicators())
levels(us_merge$cluster) = lisa$GetLabels()

us_merge$cluster = factor(us_merge$cluster,
                          levels = c("High-High", 
                                     "High-Low",
                                     "Low-High",
                                     "Low-Low",
                                     "Not significant")) # Convert to factor


map3_gsp <- ggplot() + geom_sf(data=us_merge, aes(fill = cluster), color="grey95") +
  scale_fill_manual(values = c("red", "pink", "lightblue", "darkblue", "grey95"), drop = FALSE) + 
  labs(title="", fill = "LISA") +
  guides(fill=guide_legend(title.position = "top")) +
  theme(panel.background = element_rect(fill = "white"), #Plot background
        legend.position = c(0.22, 0.2), #Legend position within the plot
        legend.key.size = unit(0.2, "cm"), #Legend key height
        legend.key.width = unit(0.2,"cm"), #Legend key width
        legend.text=element_text(size=4), #Legend text size
        legend.background = element_rect(fill=NA), #Legend background
        legend.title=element_text(size=5), #Legend title size
        axis.text = element_blank(), #X- and Y-axis text
        axis.ticks = element_blank()) +
  # Add the scale bar
  annotation_scale(
    location = "br", # 'bl' means bottom-left
    width_hint = 0.1 # Controls the relative length of the scale bar
  )+
  # Add the north arrow
  annotation_north_arrow(
    location = "tr", # 'tr' means top-right
    which_north = "true", # Can be "true" or "magnetic" depending on the orientation
    height = unit(0.8, "cm"), # Adjusts the arrow height, e.g., 1 cm
    width = unit(0.4, "cm")   # Adjusts the arrow width, e.g., 1 cm
  )
 
 
#For public capital
 
us_merge <- us_merge |>  
  mutate(lpcap=log(us_merge$pcap))
 
 
w <- rgeoda::queen_weights(us_merge, order = 1)
lisa <- local_moran(w, us_merge["lpcap"], # Select only one data column
                    permutations = 999,
                    permutation_method = "complete",
                    significance_cutoff = 0.05,
                    cpu_threads = 2,
                    seed = 123456789)
us_merge$cluster = as.factor(lisa$GetClusterIndicators())
levels(us_merge$cluster) = lisa$GetLabels()
 
us_merge$cluster = factor(us_merge$cluster,
                          levels = c("High-High", 
                                     "High-Low",
                                     "Low-High",
                                     "Low-Low",
                                     "Not significant")) # Convert to factor
 
 
map3_pcap <- ggplot() + geom_sf(data=us_merge, aes(fill = cluster), color="gray95") +
  scale_fill_manual(values = c("red", "pink", "lightblue", "darkblue", "grey95"), drop = FALSE) + 
  labs(title="", fill = "LISA") +
  guides(fill=guide_legend(title.position = "top")) +
  theme(panel.background = element_rect(fill = "white"), #Plot background
        legend.position = c(0.22, 0.2), #Legend position within the plot
        legend.key.size = unit(0.2, "cm"), #Legend key height
        legend.key.width = unit(0.2,"cm"), #Legend key width
        legend.text=element_text(size=4), #Legend text size
        legend.background = element_rect(fill=NA), #Legend background
        legend.title=element_text(size=5), #Legend title size
        axis.text = element_blank(), #X- and Y-axis text
        axis.ticks = element_blank()) +
  # Add the scale bar
  annotation_scale(
    location = "br", # 'bl' means bottom-left
    width_hint = 0.1 # Controls the relative length of the scale bar
  )+
  # Add the north arrow
  annotation_north_arrow(
    location = "tr", # 'tr' means top-right
    which_north = "true", # Can be "true" or "magnetic" depending on the orientation
    height = unit(0.8, "cm"), # Adjusts the arrow height, e.g., 1 cm
    width = unit(0.4, "cm")   # Adjusts the arrow width, e.g., 1 cm
  )
 
 
map3 <- ggarrange(map3_gsp, map3_pcap,
                  ncol = 2, nrow = 1)
map3 <- map3 + 
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # White panel background
    plot.background = element_rect(fill = "white", color = NA)    # White plot background
  )
print(map3)
 
#ggview(map3, units = "in", width = 14, height = 3, dpi = 300, bg="white")


## 3. Estimation

#2.1.-----> POOLED OLS MODEL
pool <- lm(log_gsp ~ log_pcap    + log_pc  + log_emp + unemp, 
           data=Producp)
summ_pool<-summary(pool)

#2.2.-----> RANDOM EFFECTS MODEL
re   <- plm(log_gsp ~ log_pcap    + log_pc  + log_emp + unemp, 
                     data = Producp, model = "random", effect = "individual")
summ_re<-summary(re)

#2.3.-----> FIXED EFFECTS MODEL
fe   <- plm(log_gsp ~ log_pcap    + log_pc  + log_emp + unemp, 
                     data = Producp, model = "within", effect = "individual")
summ_fe<-summary(fe)

#2.4. ----> HAUSMAN TEST
hausman_test <-phtest(fe, re)
hausman_test #Null hypothesis: the random effects estimator is consistent

#2.5.-----> INSTRUMENTAL VARIABLES MODEL
iv_fe <- plm(log_gsp ~ log_pcap    + log_pc  + log_emp + unemp |
                       L2_log_pcap + log_pc  + log_emp + unemp, 
             data = Producp, model = "within", effect = "individual")
summary(iv_fe)

iv_fed <- feols(log_gsp ~ log_pc  + log_emp + unemp | state | log_pcap ~ L2_log_pcap, 
                     Producp, panel.id = ~ state + year)
summary(iv_fed, diagnostic=T)


#3 ######### SPATIAL PANEL MODELS #########
usaww_listw <- mat2listw(usaww, style = "W")

#3.1.-----> SAR MODEL (SPATIAL DEPENDENCE IN THE DEPENDENT VARIABLE)
sarfe <- spml(log_gsp ~ log_pcap    + log_pc  + log_emp + unemp,
              data = Producp, listw = usaww_listw, spatial.error="none", lag=TRUE, 
              model = "within", effect = "twoway", method = "eigen")
summary(sarfe)
print(effects(sarfe))

#3.2.-----> SEM MODEL
semfe <- spml(log_gsp ~ log_pcap    + log_pc  + log_emp + unemp,
              data = Producp, listw = usaww_listw, lag = FALSE, 
              spatial.error = "b", model = "within",
              effect = "twoway", method = "eigen")
summary(semfe)
print(effects(semfe))

#3.3.-----> SAC / SARAR / SARMA MODEL
sararfe <- spml(log_gsp ~ log_pcap    + log_pc  + log_emp + unemp,
                data = Producp, listw = usaww_listw, lag = TRUE, 
                spatial.error = "b", model = "within",
                effect = "twoway", method = "eigen")
summary(sararfe)
print(effects(sararfe))


#3.4.-----> SPATIAL DURBIN MODEL (SPATIALLY LAGGED EXPLANATORY VARIABLES)
Producp$wlog_pcap <-slag(Producp$log_pcap, usaww_listw, 1)
Producp$wlog_pc   <-slag(Producp$log_pc,   usaww_listw, 1)
Producp$wlog_emp  <-slag(Producp$log_emp,  usaww_listw, 1)
Producp$wunemp    <-slag(Producp$unemp,    usaww_listw, 1)

sdmfe <- spml(log_gsp ~ log_pcap    + log_pc  + log_emp + unemp + 
                       wlog_pcap   + wlog_pc + wlog_emp + wunemp,
              data = Producp, listw = usaww_listw, lag = TRUE, 
              spatial.error = "none", model = "within",
              effect = "twoway", method = "eigen")
summary(sdmfe)
print(effects(sdmfe))

#3.5.-----> SPATIAL DURBIN ERROR MODEL
sdemfe <- spml(log_gsp ~ log_pcap    + log_pc  + log_emp + unemp + 
                        wlog_pcap   + wlog_pc + wlog_emp + wunemp,
               data = Producp, listw = usaww_listw, lag = FALSE, 
               spatial.error = "b", model = "within",
               effect = "twoway", method = "eigen")
summary(sdemfe)
print(effects(sdemfe))

#3.6.-----> SLX MODEL
slxfe <- plm(log_gsp ~ log_pcap    + log_pc  + log_emp + unemp + 
                      wlog_pcap   + wlog_pc + wlog_emp + wunemp,
             data = Producp, model = "within",
             effect = "twoway")
summary(slxfe)
summary(fixef(slxfe))

summary(sarfe) 
summary(semfe) 
summary(sararfe) 
summary(slxfe) 
summary(sdemfe)
summary(sdmfe) 

#5 ######### SPATIAL PANEL MODELS WITH DISAGGREGATED PUBLIC CAPITAL #########

#5.1.-----> SAR MODEL (SPATIAL DEPENDENCE IN THE DEPENDENT VARIABLE)
sarfe_des <- spml(log_gsp ~ log_hwy + log_water + log_util + log_pc  + log_emp + unemp,
              data = Producp, listw = usaww_listw, spatial.error="none", lag=TRUE, 
              model = "within", effect = "twoway", method = "eigen")
summary(sarfe_des)
print(effects(sarfe_des))

#5.2.-----> SEM MODEL
semfe_des <- spml(log_gsp ~ log_hwy + log_water + log_util + log_pc  + log_emp + unemp,
              data = Producp, listw = usaww_listw, lag = FALSE, 
              spatial.error = "b", model = "within",
              effect = "twoway", method = "eigen")
summary(semfe_des)
print(effects(semfe_des))

#5.3.-----> SAC / SARAR / SARMA MODEL
sararfe_des <- spml(log_gsp ~ log_hwy + log_water + log_util + log_pc  + log_emp + unemp,
                data = Producp, listw = usaww_listw, lag = TRUE, 
                spatial.error = "b", model = "within",
                effect = "twoway", method = "eigen")
summary(sararfe_des)
print(effects(sararfe_des))


#5.4.-----> SPATIAL DURBIN MODEL (SPATIALLY LAGGED EXPLANATORY VARIABLES)
Producp$wlog_hwy   <-slag(Producp$log_hwy, usaww_listw, 1)
Producp$wlog_water <-slag(Producp$log_water, usaww_listw, 1)
Producp$wlog_util  <-slag(Producp$log_util, usaww_listw, 1)


sdmfe_des <- spml(log_gsp ~ log_hwy  + log_water  + log_util   + log_pc  + log_emp  + unemp + 
                           wlog_hwy + wlog_water + wlog_util  + wlog_pc + wlog_emp + wunemp,
              data = Producp, listw = usaww_listw, lag = TRUE, 
              spatial.error = "none", model = "within",
              effect = "twoway", method = "eigen")
summary(sdmfe_des)
print(effects(sdmfe_des))

#5.5.-----> SPATIAL DURBIN ERROR MODEL
sdemfe_des <- spml(log_gsp ~ log_hwy  + log_water  + log_util   + log_pc  + log_emp  + unemp + 
                            wlog_hwy + wlog_water + wlog_util  + wlog_pc + wlog_emp + wunemp,
               data = Producp, listw = usaww_listw, lag = FALSE, 
               spatial.error = "b", model = "within",
               effect = "twoway", method = "eigen")
summary(sdemfe_des)
print(effects(sdemfe_des))

#5.6.-----> SLX MODEL
slxfe_des <- plm(log_gsp ~ log_hwy  + log_water  + log_util   + log_pc  + log_emp  + unemp + 
                          wlog_hwy + wlog_water + wlog_util  + wlog_pc + wlog_emp + wunemp,
             data = Producp, model = "within",
             effect = "twoway")
summary(slxfe_des)
summary(fixef(slxfe_des))

summary(sarfe_des) 
summary(semfe_des) 
summary(sararfe_des) 
summary(slxfe_des) 
summary(sdemfe_des)
summary(sdmfe_des)

#7 ######### SPATIAL PANEL MODELS WITH INSTRUMENTAL VARIABLES #########
Producp_b <- pdata.frame(Produc, c("state","year"))

Producp_b$log_pcap      <- log(Producp_b$pcap)
Producp_b$L2_log_pcap   <- plm::lag(Producp_b$log_pcap, 2)
Producp_b$log_hwy       <- log(Producp_b$hwy)
Producp_b$L2_log_hwy    <- plm::lag(Producp_b$log_hwy, 2)
Producp_b$log_water     <- log(Producp_b$water)
Producp_b$L2_log_water  <- plm::lag(Producp_b$log_water, 2)
Producp_b$log_util      <- log(Producp_b$util)
Producp_b$L2_log_util   <- plm::lag(Producp_b$log_util, 2)

Producp_b$log_gsp       <- log(Producp_b$gsp)
Producp_b$log_pc        <- log(Producp_b$pc)
Producp_b$log_emp       <- log(Producp_b$emp)

Producp_b$wlog_pcap  <-slag(Producp_b$log_pcap,  usaww_listw, 1)
Producp_b$wlog_hwy   <-slag(Producp_b$log_hwy,   usaww_listw, 1)
Producp_b$wlog_water <-slag(Producp_b$log_water, usaww_listw, 1)
Producp_b$wlog_util  <-slag(Producp_b$log_util,  usaww_listw, 1)
Producp_b$wlog_pc    <-slag(Producp_b$log_pc,    usaww_listw, 1)
Producp_b$wlog_emp   <-slag(Producp_b$log_emp,   usaww_listw, 1)
Producp_b$wunemp     <-slag(Producp_b$unemp,     usaww_listw, 1)

Producp_bna <- na.omit(Producp_b)
Producp_bna$wL2_log_pcap  <-slag(Producp_bna$L2_log_pcap,  usaww_listw, 1)
Producp_bna$wL2_log_hwy   <-slag(Producp_bna$L2_log_hwy,   usaww_listw, 1)
Producp_bna$wL2_log_water <-slag(Producp_bna$L2_log_water, usaww_listw, 1)
Producp_bna$wL2_log_util  <-slag(Producp_bna$L2_log_util,  usaww_listw, 1)

#7.1. DURBIN-IV MODEL WITH AGGREGATE PUBLIC CAPITAL

sdmfe_iv <- spml(log_gsp ~ log_pcap     + log_pc  + log_emp + unemp + 
                          wlog_pcap    + wlog_pc + wlog_emp + wunemp | 
                        L2_log_pcap     + log_pc  + log_emp + unemp +
                      wL2_log_pcap + wlog_pc + wlog_emp + wunemp,
              data = Producp_bna, listw = usaww_listw, lag = TRUE, 
              spatial.error = "none", model = "within",
              effect = "twoway", method = "eigen")
summary(sdmfe_iv, dianostic=T)

#7.2. DURBIN-IV MODEL WITH DISAGGREGATED PUBLIC CAPITAL
sdmfe_iv_des <- spml(log_gsp ~ log_hwy +      log_water +      log_util +  log_pc +  log_emp +  unemp + 
                            + wlog_hwy +     wlog_water +     wlog_util + wlog_pc + wlog_emp + wunemp | 
                            L2_log_hwy +   L2_log_water +   L2_log_util +  log_pc +  log_emp +  unemp +
                           wL2_log_hwy +  wL2_log_water +  wL2_log_util + wlog_pc + wlog_emp + wunemp,
                 data = Producp_bna, listw = usaww_listw, lag = TRUE, spatial.error = "none", model = "within", effect = "twoway", method = "eigen")
summary(sdmfe_iv_des, dianostic=T)



set.seed(123456789)
impacts_sdmfe_iv <- impacts(sdmfe_iv, listw = usaww_listw, R=100000, time=length(unique(Producp$year)))
summary(impacts_sdmfe_iv, zstats=TRUE, short=TRUE)

set.seed(123456789)
impacts_sdmfe_iv_des <- impacts(sdmfe_iv_des, listw = usaww_listw,  R=100000, time=length(unique(Producp$year)))
summary(impacts_sdmfe_iv_des, zstats=TRUE, short=TRUE)

## References

-   Baltagi, B. H. (1995). Econometric Analysis of Panel Data. Wiley.

-   Baltagi, B. H., & Pinnoi, N. (1995). Public capital stock and state productivity growth: Further evidence from an error components model. Empirical Economics, 20(2), 351-359.


## Appendix.

# Tests LM
slmtest(log_gsp ~ log_pcap    + log_pc  + log_emp + unemp, data=Producp, listw = usaww, test="lml",
        model="within", effect='twoway')

slmtest(log_gsp ~ log_pcap    + log_pc  + log_emp + unemp, data=Producp, listw = usaww, test="lme",
        model="within", effect='twoway')

slmtest(log_gsp ~ log_pcap    + log_pc  + log_emp + unemp, data=Producp, listw = usaww, test="rlml",
        model="within", effect='twoway')

slmtest(log_gsp ~ log_pcap    + log_pc  + log_emp + unemp, data=Producp, listw = usaww, test="rlme",
        model="within", effect='twoway')
#When both tests reject the null, the model with the larger standardized test statistic is preferred; here, the robust LM error test (SEM)
#Under Elhorst's (2010) criterion, the Spatial Durbin model could also be selected.
