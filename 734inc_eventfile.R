# Event file - generate command order for P734 N2O laser incubation system
# Final output is .csv file with actuator and time cols

# 30/04/2024
# Taleta Bailey

# set up -------------------------------------------------------------
# Load tidyverse package
library(tidyverse) 

# If script fails here, run this line:
# install.packages('tidyverse')

# This works if chambers are numbered sequentially. Actual chamber IDs skip ~24 chambers 

# # Function for set chamber numbers. Checks to make sure is multiple of 3 (number of racks), and returns a warning if not valid. 
# setnchamb <- \(minchamb, maxchamb, N2chambs = c(24, 60, 96)){
#   nseq <- minchamb:maxchamb
#   nseq <- nseq[!(nseq %in% N2chambs)]
#   ifelse(length(nseq) %% 3 ==0, 
#          return(nseq), 
#          warning("Invalid number of chambers, please make total nchamb multiple of 3"))
#   
# }
# 
# # # Test function works
# setnchamb(1, 71)


# Incubation system parameters -------------------------------------------

# Number vessels in system:

# *Not used - manually define chamberseq numbers*
# setnchamb function takes min chamber number and max chamber number as inputs, then makes sequence between. 
# chambseq <- setnchamb(minchamb = 1, 
#                     maxchamb = 71)


chambseq <- c(1:23,37:59, 73:95) # update - manual input chamber numbers that will be sampled. 
  # ChambersIDs 24, 60, 96 used for compressed air flush. ChamberIDs 25:36, 61:72 not in use. 

# Chambers to skip:
chambfilt <- c('81') # Change 'none' for chamber numbers e.g. c('38', '26'). Place 'none' if no chambers to be removed. 'none' used as filler, otherwise filtering fails..

# remove chamber IDs for compressed air flush 
# chambseq <- chambseq[!(chambseq %in% c(24, 60, 96))]

nchamb <- length(chambseq)

# Timing of events (seconds): 
tflush <- 5
tflushoff <- 5
tN2 <- 1
tchamber <- 15

# ID which chamber is used for N2 flush
N2_R1 <- "C24"
N2_R2 <- "C60"
N2_R3 <- "C96"


# Create variables for actuator commands --------------------------------------


#This could be done in the data.frame() below, but we're here now..
# Make F1:nchamb
Fnum <- map_chr(chambseq, ~paste0("F", .x))

# Make C1:nchamb
Cnum <- map_chr(chambseq, ~paste0("C", .x))


# Combine F1:nchamb with Rack ID, and add N2 flush col.
vesselID <- data.frame(counter = rep(1:c(nchamb/3), 3), 
  rack = as.factor(c(rep("R1",c(nchamb/3)), rep("R2", c(nchamb/3)), rep("R3", c(nchamb/3)))), 
  flush = Fnum,
  chamber = Cnum
) %>% 
  mutate(N2 = case_match(rack, "R1" ~ N2_R1,
                                 "R2" ~ N2_R2,
                                 "R3" ~ N2_R3), 
         flushoff = map_chr(chambseq, ~paste0("OUTW", .x, ":0")))

# Check looks ok
str(vesselID)

# Flush sequence ---------------------------------------------------

# Make sequence of vessel flushing - alternating between racks that are not being sampled. 
# pivot wider so F for respective racks in separate columns
flushID <- vesselID %>% 
  # filter(!(chamber %in% c(N2_R1, N2_R2, N2_R3))) %>% 
  select(-c(chamber, N2, flushoff)) %>% 
  group_by(rack) %>% 
  pivot_wider(values_from = flush, names_from = rack) %>% 
  select(-c(counter))

# rbind to alternately combine F from racks that are NOT being sampled
flushseq <- data.frame(
'R1sample' = c(rbind(flushID$R2, flushID$R3)),
'R2sample' = c(rbind(flushID$R1, flushID$R3)),
'R3sample' = c(rbind(flushID$R1, flushID$R2))
)

# Sample sequence -------------------------------------------------

# Make df for sampling sequence
  # Needs to follow order of Flush, N2 flush, Chamber, and then R1, R2, R3, R1, etc
sampleseq <- vesselID %>% 
  mutate(sampleorder = rep(1:(length(counter)/3), 3)) %>% # Place in order of R1, R2, R3
  arrange(sampleorder) %>% 
  mutate(sample = map_chr(1:nchamb, ~paste0("sample", .x))) # Make sample number IDs


# Pivot wider so all actuator commands in one col, in correct order
sampleseq_l <- sampleseq %>% 
  pivot_longer(cols = c(flush, flushoff, N2, chamber), # To change order of actuator commands, change order of cols here.
               names_to = 'action', values_to = 'actuator') 



# Event file sequence ----------------------------------------------

# Make sequence with sample, flush sequence
# How to join together flushseq and sampleseq_l.....

# Transponse flushseq so have one row for each rack
flushseq_w <- cbind(rack = c("R1", "R2", "R3"), as.data.frame(t(flushseq)))

# Join transposed flushseq with sampleseq
eventseq <- sampleseq %>% 
  left_join(flushseq_w) 

str(eventseq) # see what it looks like

# Pivot longer so column each for action and actuator 
eventseq_l <- eventseq %>%
  filter(!(grepl(chambfilt, chamber))) %>% # EDIT: added filter here, otherwise samples from flush while chamber is skipped. However, this also removed flushing sequence when chamber should have been sampled.. 
  pivot_longer(cols = c(flush, flushoff, N2, chamber, V1:rev(names(eventseq))[1]), names_to = 'action', values_to = 'actuator') %>% 
  mutate(action = case_when(grepl("V", action) ~ "flush", .default = action)) # replace col names from flushseq_w



# Timings ---------------------------------------------------------------

times <- data.frame('action' = c('flush', 'flushoff', 'N2', 'chamber'),
                    'seconds' = c(tflush, tflushoff, tN2, tchamber)) # these were set at the start of script - at Inc. sys param



# merge with event sequence
eventseq_times <- eventseq_l %>% 
  filter(!(grepl(chambfilt, actuator))) %>% # This line removes chambers specified in set up. Comment out if not needed.
  # filter(rack == 'R3') %>%   # can filter so only particular rack is output in event file. 
  left_join(times) %>% 
  mutate(seconds = if_else(row_number() == 1, 1, seconds)) %>% 
  mutate(time = cumsum(seconds)) # cumulative time in seconds

# eventseq_times %>% 
#   filter(grepl('81', actuator))


# Output to .csv --------------------------------------------------------

eventseq_times %>%
  select(time, actuator) %>%
  rename(Time = time, Actuator = actuator) %>%
  write_csv(paste0('Event files/P734N2Oinc_',
                  paste0(length(sampleseq$sample), 'chambers_'),
                  "eventfile_",
                  format(Sys.Date(), "%y%m%d"),
                  ".csv")
            )
