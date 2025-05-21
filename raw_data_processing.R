## DATA PROCESSING ##

# Load packages
library(dplyr)
library(sf)
library(units)
library(rnaturalearth)

## LOAD DATA ##

# ND-Gain: https://gain.nd.edu/our-work/country-index/download-data/
# GHG Emissions: https://www.climatewatchdata.org/ghg-emissions
  # Data Source: Climate Watch
  # Location: World
  # Sectors/Subsectors: Total including LUCF
  # Gases: All GHG
  # Calculations: Total (MtCO2e) AND Per Capita (tCO2e per capita)
  # Show data by: Countries
# GeoBoundaries: https://www.geoboundaries.org/globalDownloads.html

# Set working directory
setwd("S:/web/geo454projects/Group4/group4-r/")

# Import nd_gain data
nd_gain <- read.csv("raw_data/nd_gain_countryindex_2024/resources/gain/gain.csv")
nd_gain_delta <- read.csv("raw_data/nd_gain_countryindex_2024/resources/gain/gain_delta.csv")

# Import GHG emission data
ghg_total <- read.csv("raw_data/ghg-emissions_total.csv")
ghg_percap <- read.csv("raw_data/ghg-emissions_per-capita.csv")

# Import country boundaries
countries <- read_sf("raw_data/geoBoundariesCGAZ_ADM0.shp")

## SIMPLIFY COUNTRY BOUNDARIES ##

# Drop unneeded columns 
countries$shapeType <- NULL
countries$shapeName <- NULL

# Repair geometries
sf_use_s2(FALSE) # Apparently spherical geometry (s2) sometimes needed to be switched off
countries_rep <- st_make_valid(countries)

# Remove vertices
# dTolerance is in decimal degrees (0.01 = ~1km)
# You can ignore the warning message
countries_simp <- st_simplify(countries, preserveTopology = TRUE, dTolerance = 0.06)

# Remove small islands 
# First explode multi-part polygons (you can ignore warning message)
countries_single <- st_cast(countries_simp, "MULTIPOLYGON") %>% st_cast("POLYGON")

# Calculate area
countries_single$area <- st_area(countries_single) %>% set_units(km^2) %>% drop_units()

# Extract polygons greater than 600km2
countries_single_simp <- countries_single[countries_single$area >= 600, ]

# Merge multi-parts back together
countries_multi_simp <- countries_single_simp %>%
  group_by(shapeGroup) %>% 
  summarize(geometry = st_union(geometry))

# Make geometries valid again
countries_simp_final <- st_make_valid(countries_multi_simp)

# Check outputs
#st_write(countries_simp_final, "countries_simp_final.shp")

## SIMPLIFY/CLEAN-UP ND-GAIN ##

# Round all numeric values to 2 decimal places
nd_gain <- nd_gain %>% mutate_if(is.numeric, round, digits=2)
nd_gain_delta <- nd_gain_delta %>% mutate_if(is.numeric, round, digits=2)


# Rename columns
colnames(nd_gain) <- c("ISO3", "Name", "nd1995", "nd1996", "nd1997",
                       "nd1998", "nd1999", "nd2000", "nd2001", "nd2002", 
                       "nd2003", "nd2004", "nd2005", "nd2006", "nd2007", 
                       "nd2008", "nd2009", "nd2010", "nd2011", "nd2012", 
                       "nd2013", "nd2014", "nd2015", "nd2016", "nd2017",
                       "nd2018", "nd2019", "nd2020", "nd2021", "nd2022"
)

colnames(nd_gain_delta) <- c("ISO3", "Name", "d1995", "d1996", "d1997",
                             "d1998", "d1999", "d2000", "d2001", "d2002", 
                             "d2003", "d2004", "d2005", "d2006", "d2007", 
                             "d2008", "d2009", "d2010", "d2011", "d2012", 
                             "d2013", "d2014", "d2015", "d2016", "d2017",
                             "d2018", "d2019", "d2020", "d2021", "d2022"
)

nd_gain_delta$Name <- NULL

## SIMPLIFY/CLEAN-UP GHG DATA ##

# Drop unneeded columns (to match the ND-GAIN which starts 1995)
ghg_total[ ,c('Country.Region', 'unit', 'X1990', 'X1991', 
              'X1992', 'X1993', 'X1994')] <- list(NULL)
ghg_percap[ ,c('Country.Region', 'unit', 'X1990', 'X1991', 
               'X1992', 'X1993', 'X1994')] <- list(NULL)

# Delete invalid rows from metadata included in csv
ghg_total <- ghg_total[-c(194, 195), ]
ghg_percap <- ghg_percap[-c(194, 195), ]

# Rename columns
colnames(ghg_total) <- c("iso", "gt1995", "gt1996", "gt1997", "gt1998", 
                         "gt1999", "gt2000", "gt2001", "gt2002", "gt2003", 
                         "gt2004", "gt2005", "gt2006", "gt2007", "gt2008", 
                         "gt2009", "gt2010", "gt2011", "gt2012", "gt2013", 
                         "gt2014", "gt2015", "gt2016", "gt2017", "gt2018", 
                         "gt2019", "gt2020", "gt2021", "gt2022")

colnames(ghg_percap) <- c("iso", "gpc1995", "gpc1996", "gpc1997", "gpc1998", 
                          "gpc1999", "gpc2000", "gpc2001", "gpc2002", "gpc2003", 
                          "gpc2004", "gpc2005", "gpc2006", "gpc2007", "gpc2008", 
                          "gpc2009", "gpc2010", "gpc2011", "gpc2012", "gpc2013", 
                          "gpc2014", "gpc2015", "gpc2016", "gpc2017", "gpc2018", 
                          "gpc2019", "gpc2020", "gpc2021", "gpc2022")

## JOIN DATA WITH COUNTRY BOUNDARIES ##

# Join nd_gain with country boundaries (keep everything which has a geometry)
nd_gain_sf <- st_as_sf(right_join(nd_gain, countries_simp_final, 
                                  by= c("ISO3"="shapeGroup")))

# Join nd_gain_sf with emissions data (keep everything which has ND-GAIN data)
nd_gain_ghg_t_sf <- st_as_sf(left_join(nd_gain_sf, ghg_total , 
                                       by= c("ISO3"="iso")))
all_data_sf <- st_as_sf(left_join(nd_gain_ghg_t_sf, ghg_percap , 
                                          by= c("ISO3"="iso")))

# Add the nd_gain delta columns
#all_data_sf <- st_as_sf(left_join(nd_gain_ghg_t_pc_sf, nd_gain_delta , 
#                                  by= c("ISO3"="ISO3")))

## FINAL CLEANING ##

# Tidy up names
all_data_sf[all_data_sf$ISO3 == "COD", "Name"] <- "Democratic Republic of the Congo"
all_data_sf[all_data_sf$ISO3 == "IRN", "Name"] <- "Iran"
all_data_sf[all_data_sf$ISO3 == "BOL", "Name"] <- "Bolivia"
all_data_sf[all_data_sf$ISO3 == "PRK", "Name"] <- "North Korea"
all_data_sf[all_data_sf$ISO3 == "KOR", "Name"] <- "South Korea"
all_data_sf[all_data_sf$ISO3 == "LAO", "Name"] <- "Laos"
all_data_sf[all_data_sf$ISO3 == "LBY", "Name"] <- "Libya"
all_data_sf[all_data_sf$ISO3 == "FSM", "Name"] <- "Micronesia"
all_data_sf[all_data_sf$ISO3 == "MDA", "Name"] <- "Moldova"
all_data_sf[all_data_sf$ISO3 == "VEN", "Name"] <- "Venezuela"
all_data_sf[all_data_sf$ISO3 == "VNM", "Name"] <- "Vietnam"
all_data_sf[all_data_sf$ISO3 == "SYR", "Name"] <- "Syria"
all_data_sf[all_data_sf$ISO3 == "RUS", "Name"] <- "Russia"
all_data_sf[all_data_sf$ISO3 == "TZA", "Name"] <- "Tanzania"

# Simplify / fill in missing info
all_data_sf[all_data_sf$ISO3 == "111", "Name"] <- "Abyei Area"
all_data_sf[all_data_sf$ISO3 == "112", "Name"] <- "Aksai Chin"
all_data_sf[all_data_sf$ISO3 == "113", "Name"] <- "Sang/Jadhang"
all_data_sf[all_data_sf$ISO3 == "114", "Name"] <- "Kaurik/Shipki La"
all_data_sf[all_data_sf$ISO3 == "117", "Name"] <- "Falkland Islands/Islas Malvinas"
all_data_sf[all_data_sf$ISO3 == "121", "Name"] <- "Siachen Glacier"
all_data_sf[all_data_sf$ISO3 == "129", "Name"] <- "West Bank"
all_data_sf[all_data_sf$ISO3 == "ESH", "Name"] <- "Western Sahara"
all_data_sf[all_data_sf$ISO3 == "GRL", "Name"] <- "Greenland"
all_data_sf[all_data_sf$ISO3 == "SSD", "Name"] <- "South Sudan"
all_data_sf[all_data_sf$ISO3 == "TWN", "Name"] <- "Taiwan"
all_data_sf[all_data_sf$ISO3 == "XKX", "Name"] <- "Kosovo"
all_data_sf[all_data_sf$ISO3 == "ATA", "Name"] <- "Antarctica"

# Make sure all numeric columns are stored as numeric
# First extract the column names which should be numeric
numeric_cols <- st_drop_geometry(all_data_sf)
numeric_cols <- colnames(numeric_cols[3:86]) #change to 3:114 if using delta

# Use lapply with as.numeric function to convert each column subset
# You can ignore warnings (these are for NA values which will be removed anyways)
all_data_sf[numeric_cols] <- lapply(all_data_sf[numeric_cols], 
                                    function(x) as.numeric(as.character(x)))

# Remove the Solomon Islands (their GHG emissions data is really erratic/unreliable)
all_data_sf_clean <- all_data_sf[all_data_sf$ISO3 != "SLB", ]

## EXTRA: ADD CONTINENTS ##
#all_data_sf_clean <- read_sf("ndgain_ghg_4326.shp")

# Get countries data with continent info
countries_ne <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(name_long, iso_a3, continent)


all_data_sf_joined <- all_data_sf_clean %>%
  left_join(countries_ne %>% st_drop_geometry(), 
            by = c("ISO3" = "iso_a3"))


all_data_sf_joined <- all_data_sf_joined %>%
  mutate(continent = case_when(
    ISO3 == "FRA" ~ "Europe",
    ISO3 == "NOR" ~ "Europe",
    ISO3 == "MUS" ~ "Africa",
    TRUE ~ continent  # keep existing values
  ))

all_data_sf_joined %>%
  filter(is.na(continent))  # should return 0 rows

# Drop "name_long" column to save space
all_data_sf_joined$name_long <- NULL


## SAVE OUTPUTS ##

# Write final version
st_write(all_data_sf_joined, "all_data_sf_joined.gpkg", append = F)



