library(tidyverse)
library(geojsonsf)
library(sf)
library(tigris)
library(lubridate)
library(hrbrthemes)
library(geofacet)

# Processing fucntion
solar_permits <- function(df){
  df %>% 
    filter(PERMIT_SUBTYPE_NAME == "SOLAR SYSTEM" & !grepl("REVISION",FEE_TYPE) & FEES_PAID > 200) %>%
    mutate(residential_pv = grepl("RESPV",FEE_TYPE),
           commercial_pv = grepl("SOLCOMPV",FEE_TYPE))
}

# Load and process data
permits_2021 <- geojson_sf("https://opendata.arcgis.com/datasets/fea4cae3ae1f41beb5531ecf2ef5efc3_3.geojson") %>% solar_permits()
permits_2020 <- geojson_sf("https://opendata.arcgis.com/datasets/066cda75c1754a088e821baa1cf8ac18_2.geojson") %>% solar_permits()
permits_2019 <- geojson_sf("https://opendata.arcgis.com/datasets/52e671890cb445eba9023313b1a85804_8.geojson") %>% solar_permits()
permits_2018 <- geojson_sf("https://opendata.arcgis.com/datasets/42cbd10c2d6848858374facb06135970_9.geojson") %>% solar_permits()
permits_2017 <- geojson_sf("https://opendata.arcgis.com/datasets/81a359c031464c53af6230338dbc848e_37.geojson") %>% solar_permits()
permits_2016 <- geojson_sf("https://opendata.arcgis.com/datasets/5d14ae7dcd1544878c54e61edda489c3_24.geojson") %>% solar_permits()


# Combine datasets - filter to construction
sol_permits <- bind_rows(
  permits_2021,
  permits_2020,
  permits_2019,
  permits_2018,
  permits_2017,
  permits_2016
) %>%
  filter(PERMIT_TYPE_NAME == "CONSTRUCTION")

# Save data
write_rds(sol_permits, "solar_permits.rds")
sol_permits <- read_rds("solar_permits.rds")

# Summary EDA
sol_permits %>%
  group_by(APPLICATION_STATUS_NAME) %>%
  count()

# Summarize time series - quarterly
quarterly_permits <- sol_permits %>%
  st_drop_geometry() %>%
  mutate(ISSUE_DATE = as.Date(ISSUE_DATE),
         quarter = floor_date(ISSUE_DATE, "quarter"),
         WARD = paste0("Ward ", WARD)) %>% 
  group_by(quarter, WARD, APPLICATION_STATUS_NAME) %>%
  summarize(count = n())

# Load geographic boundary data
dc_lines <- state_legislative_districts(state = "DC", house = "upper", year = 2020)
anc_lines <- geojson_sf("https://opendata.arcgis.com/datasets/fcfbf29074e549d8aff9b9c708179291_1.geojson")
dc_facet_grid <- grid_auto(dc_lines %>% mutate(WARD = NAMELSAD), names = "WARD", seed = 13)
hpn <- geojson_sf("https://opendata.arcgis.com/datasets/de63a68eb7674548ae0ac01867123f7e_13.geojson") %>% 
  mutate(lon = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
         lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]]))

hpn_top <- hpn %>% filter(CODE %in% c("N8","N38","N28","N32","N14","N5", "N30"))

# Map quarterly time series
quarterly_permits %>%
  ggplot(aes(quarter,count)) +
  geom_bar(stat = "identity",fill = "forest green") +
  facet_geo(~WARD, grid = dc_facet_grid) +
  labs(title = "Solar Permits Issued by Ward",
       subtitle = "Permits Issued by DCRA, 2016-2021",
       caption = "Source: DC Open Data, August, 2021", x= NULL, y = NULL)  +
  theme_ipsum_pub()+
  theme(aspect.ratio=1)

ggsave("ward_issued.png", device = "png", width = 16, height = 9, units = "in", dpi = 320)

# Completed solar projects
sol_completed <- sol_permits %>%
  filter(APPLICATION_STATUS_NAME == "COMPLETED"|APPLICATION_STATUS_NAME == "COMPLETE")

# Load and save DC lot data
# dc_lots <- geojson_sf("Common_Ownership_Lots.geojson")
# write_rds(dc_lots, "dc_lots.rds")
dc_lots <- read_rds("dc_lots.rds")
valid <- dc_lots$geometry %>% st_is_valid()

# Filter lots to solar permitted ones only
# Create indicator for completion
dc_lots_solar <- dc_lots %>%
  filter(SSL %in% sol_permits$SSL) %>%
  mutate(completed = SSL %in% sol_completed$SSL)

# Filter to ALL valid lots and create indicators for solar permits
dc_lots_valid <- dc_lots %>% 
  mutate(valid = valid) %>% 
  filter(valid == T) %>% 
  mutate(solar_permit_issued = SSL %in% sol_permits$SSL,
         sol_completed = SSL %in% sol_completed$SSL)



# Visualize ALL lots
dc_lots_valid %>%
  ggplot() +
  geom_sf(aes(fill = solar_permit_issued), lwd = 0,color = NA)  +
  coord_sf() +
  scale_fill_manual(values = c("grey", "forest green"),
                    name = NULL) +
  labs(title = "Over 8,700 Solar Permits Issued in DC",
       subtitle = "Permits Issued by DCRA, 2016-2021",
       caption = "Source: DC Open Data, August, 2021") +
  hrbrthemes::theme_ipsum_pub(grid = F, axis = F, ticks = F) +
  theme(legend.position = "none", axis.text = element_blank(), 
        axis.text.x = element_blank(), axis.text.y = element_blank()) 

ggsave("total_issued.png", device = "png", width = 16, height = 9, units = "in", dpi = 400)

# Visualize solar lots
dc_lots_solar %>%
  ggplot() +
  #geom_sf(data = dc_lines, fill = NA, lwd = .1, alpha = .1) +
  geom_sf(aes(fill = completed),lwd = 0,color = NA) +
  coord_sf() +
  geom_density2d(data = sol_permits, aes(x = LONGITUDE, y = LATITUDE), size = 0.3, 
                 color = "forest green", alpha = .2) +
  geom_sf(data = dc_lines, fill = NA, lwd = .1, alpha = .1) + 
  geom_text(data = hpn_top, aes(label = DC_HPN_NAME, x = lon, y = lat), size = 2, alpha = .5, fontface = "bold") +
  scale_fill_manual(values = c("light green", "forest green"), 
                    labels = c("Permit Issued", "Construction Completed"), 
                    name = NULL) + 
  labs(title = "Neighborhoods with the Most Solar Permits Issued in DC",
       subtitle = "Permits Issued by DCRA, 2016-2021",
       caption = "Source: DC Open Data, August, 2021", x = NULL, y = NULL) +
  hrbrthemes::theme_ipsum_pub(base_size = 16, grid = F, axis = F, ticks = F) +
  theme(legend.position = "top", axis.text = element_blank(), 
        axis.text.x = element_blank(), axis.text.y = element_blank())

ggsave("solar_permits_issued.png", device = "png", width = 16, height = 9, units = "in", dpi = 400)

