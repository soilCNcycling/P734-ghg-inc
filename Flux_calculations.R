#' ---
#' title: "Laser GHG flux calculations"
#' subtitle: "P734 Incubation system"
#' author: "Taleta Bailey"
#' date: '`r format(Sys.Date(), "%d/%m/%Y")`'
#' output: 
#' html_document:
#'    toc: true
#' ---


# Set up -------------------------------
# Load packages
packages <- c('tidyverse', 'broom', 'lubridate', "hms")
lapply(packages, library, character.only = T)

# set default ggplot theme
old.gg <- theme_set(theme_bw() + theme(panel.grid = element_blank()))


# # Install packages (if not already)
# lapply(packages, install.packages)



# Load & tidy data ----------------------------

folder <- "./Aeris_data/Marks_inc/"

# Load and row bind multiple GHG data files
datfiles <- list.files(
  path = folder,
  pattern = "data_cores", 
  full.names = T)

datfiles

dat <- lapply(datfiles, \(x) read_csv(x)[-c(1:7), ]) %>% 
  set_names(basename(datfiles)) %>%
  bind_rows(.id = "file") %>% 
  mutate(output_time = gsub(file, pattern = '_data_cores.csv', replacement = ""))

str(dat)

# Change name of some cols
dat <- dat %>% 
  rename(core = `core #`, 
         n2o_ppm = `n2o[ppm]`,
         co2_ppm = `co2[ppm]`,
         h2o_ppm = `h2o[ppm]`)

# Change format of some cols
dat2 <- dat %>% 
  mutate(core = factor(core, levels = unique(str_sort(core, numeric = T)))) %>% 
  mutate(date2 = as.Date(date, format = "%d/%m/%Y")) %>% 
  mutate(datetime = dmy_hms(paste(date, time))) %>% 
  group_by(core, output_time) %>% 
  mutate(seconds = row_number())


# Cores used for compressed air flush and pressure relief. Still recorded data, can be filtered out. 
flush_ch <- c('C24', 'C60', 'C96', 'C1', 'C37', 'C73')



# Flux calcs --------------------------------------------------------------
# Set r^2 cut off for calculating flux from raw ppm data
r2cutoff <- 0.8

# Filter for only treatment cores, add seconds to each chamber, nest data for each chamber
dat_nest <- dat2 %>% 
  select(-c('h2o_ppm')) %>% 
  filter(!(core %in% flush_ch)) %>% 
  pivot_longer(contains('ppm'), values_to = 'ppm', names_to = c('gas', NA), names_sep = '_') %>% 
  group_by(core, output_time, gas) %>% 
  nest()


# Filter each data set for only 90-460 seconds, fit lm to filtered CO2 and N2O data 
dat_nest <- dat_nest %>% 
  mutate(data_flux = map(data, ~filter(.x, seconds >= 90 & seconds <= 460))) %>% 
  mutate(ppm_lm = map(data_flux, ~lm(ppm ~ `seconds`, data = .x)))


# Extract slope and r^2 for each lm
dat_nest <- dat_nest %>% 
  mutate(ppm_slope = map_dbl(ppm_lm, ~coef(.x)[["seconds"]])* 60) %>% # ppm/s * 60 s/m
  mutate(ppm_rsq = map_dbl(ppm_lm, ~glance(.x)$r.squared))


# Check for any with negative slope or r^2 < 0.8
dat_nest %>% 
  filter(ppm_slope < 0 | ppm_rsq < 0.8) %>% 
  # filter(core %in% c(sapply(c(12, 15, 45, 5, 51, 54, 6,91), \(x) paste0('C', x)))) %>% 
  arrange(output_time, gas) %>% 
  print(n = Inf)


# Function for calculating umol g soil^-1 h^-1
# chamber volume in L
# Soil_g = oven dry soil mass
# temp can be replaced with actual temp.. 
# dppm in ppm/min

soilmass <- 100 # g oven dry soil per core

fn_umol_g_h <- function(dppm, soil_g, chamber_cm3 = 1220, temp = 25){
  # Calculate molar volume of ideal gas. Option to input temp, but defaults to 25*C
  R = 8.314463 # Ideal gas constant - m^3.Pa mol.K^-1
  T_K = ifelse(is.na(temp), 25+273.15, temp + 273.15) # Convert *C to *K. Also replace NA with 25*C.
  P_Pa = 101.325*1000 # Atmospheric kPa to Pa
  molv = R * T_K/ P_Pa # units are m^3/mol
  
  # Calculate flux in umol m^-2 h^-1
  # dppm units ppm/min
  chamber_m3 = chamber_cm3 * 10^-6
  flux_umol_g_h = dppm * chamber_m3 * (1/molv) * 60 * (1/soil_g)
  
  return(flux_umol_g_h)
}

# # test run
# fn_umol_g_h(dppm = dat_nest$ppm_slope[1], soil_g = soilmass)


# Calculate gas flux - convert umol to ug CO2-C or N2O-N
dat_flux <- dat_nest %>% 
  mutate(flux_umol_g_h = case_when(ppm_rsq > r2cutoff & ppm_slope > 0 ~ fn_umol_g_h(dppm = ppm_slope, soil_g = soilmass))) %>% 
  mutate(flux_ug_g_h = case_when(gas == 'co2' ~ flux_umol_g_h * 12, 
                                 gas == 'n2o' ~ flux_umol_g_h * 28))



# # MDL from N2 flush data ??? ----------------------------
# # Data recorded for chambers C24, C60 and C96 which were used for N2 flush
# MDL_daily <- dat2[-c(1:15), ] %>% 
#   filter(core %in% c('C24', 'C60', 'C96')) %>% 
#   group_by(date2) %>% 
#   summarise(across(c('n2o_ppm', 'co2_ppm'), 
#                    list(sd = sd, 
#                         count = length))) %>% 
#   mutate(n2o_DL = n2o_ppm_sd * qt(0.05, df = n2o_ppm_count, lower.tail = F)) %>% 
#   mutate(co2_DL = co2_ppm_sd * qt(0.05, df = co2_ppm_count, lower.tail = F)) %>% 
#   mutate(across(contains('DL'), ~.x/(460-90)*60, .names = "{.col}_slope")) %>%   # convert DL to ppm/min slope
#   mutate(n2o_flux_DL_ug_g_h = fn_umol_g_h(dppm = n2o_DL_slope, soil_g = soilmass)*28) %>% 
#   mutate(co2_flux_DL_ug_g_h = fn_umol_g_h(dppm = co2_DL_slope, soil_g = soilmass)*12)
#   
# MDL_daily %>% 
#   ggplot(aes(x = date2, y = n2o_flux_DL_ug_g_h))+
#   geom_point()
# 
# 
# # %>% 
# #   ggplot(aes(x = date2, y = n2o_ppm)) + 
# #   geom_point()
# 
# 
# # qt(p=0.05, df = 20, lower.tail = F)
# 


# # Save .csv output ------------------------------------------------

# Select necessary cols and reshape so CO2 and N2O flux in separate columns
flux_output <- dat_flux %>% 
  select(!c(where(is.list), contains('umol'))) %>% 
  pivot_wider(names_from = gas, values_from = contains(c('ppm', 'flux')), names_glue = "{gas}_{.value}", names_vary = 'slowest') %>% 
  rename(chamber = core, 
         n2o_flux_ug_n_g_soil_h = n2o_flux_ug_g_h, 
         co2_flux_ug_c_g_soil_h = co2_flux_ug_g_h) %>% 
  mutate(output_datetime = as_datetime(output_time, format = "%d%m%y %H%M")) %>%
  mutate(date = date(output_datetime)) %>% 
  arrange(output_datetime, chamber)

# # Write .csv -------------------
# ## Write flux output to .csv file in correct folder
# flux_output %>%
#   write_csv(paste0("Fluxes/Flux_data_output", format(Sys.Date(), "%y%m%d"), ".csv"), na = "")



# Graph to check flux output ---------------------------------------
## N2O
flux_output %>% 
  ggplot(aes(x = output_datetime, y = n2o_flux_ug_n_g_soil_h, col = chamber)) + 
  geom_point(size = 4)+ 
  geom_path() + 
  labs(title = "N2O flux") + 
  facet_wrap(~chamber, nrow = 3) + 
  theme(legend.position = "none")

## CO2
flux_output %>% 
  ggplot(aes(x = output_datetime, y = co2_flux_ug_c_g_soil_h, col = chamber)) + 
  geom_point(size = 4)+ 
  geom_path() + 
  labs(title = "CO2 flux") + 
  facet_wrap(~chamber, nrow = 3) + 
  theme(legend.position = "none")

