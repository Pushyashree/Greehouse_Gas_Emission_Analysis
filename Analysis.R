install.packages(c("ggplot2", "dplyr", "tidyr", "scales", "ggpubr", "ggcorrplot"))
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(ggpubr)
library(ggcorrplot)

ghg_data <- read.csv("greenhouse_gas_inventory_data_data.csv")
head(ghg_data)
str(ghg_data)

#Rename large categories of emissions in the 'category' column
ghg_data <- ghg_data %>%
  mutate(category = recode(category,
                                    "carbon_dioxide_co2_emissions_without_land_use_land_use_change_and_forestry_lulucf_in_kilotonne_co2_equivalent" = "CO2 (No LULUCF)",
                                    "greenhouse_gas_ghgs_emissions_including_indirect_co2_without_lulucf_in_kilotonne_co2_equivalent" = "GHG (Total, No LULUCF)",
                                    "greenhouse_gas_ghgs_emissions_without_land_use_land_use_change_and_forestry_lulucf_in_kilotonne_co2_equivalent" = "GHG (No LULUCF)",
                                    "hydrofluorocarbons_hfcs_emissions_in_kilotonne_co2_equivalent" = "HFCs",
                                    "methane_ch4_emissions_without_land_use_land_use_change_and_forestry_lulucf_in_kilotonne_co2_equivalent" = "CH4 (No LULUCF)",
                                    "nitrogen_trifluoride_nf3_emissions_in_kilotonne_co2_equivalent" = "NF3",
                                    "nitrous_oxide_n2o_emissions_without_land_use_land_use_change_and_forestry_lulucf_in_kilotonne_co2_equivalent" = "N2O (No LULUCF)",
                                    "perfluorocarbons_pfcs_emissions_in_kilotonne_co2_equivalent" = "PFCs",
                                    "sulphur_hexafluoride_sf6_emissions_in_kilotonne_co2_equivalent" = "SF6",
                                    "unspecified_mix_of_hydrofluorocarbons_hfcs_and_perfluorocarbons_pfcs_emissions_in_kilotonne_co2_equivalent" = "HFCs + PFCs (Mixed)"
  ))

# Define European countries based on your list
european_countries <- c("Austria", "Belarus", "Belgium", "Croatia", "Czech Republic", "Denmark",
                        "Estonia", "European Union", "Finland", "France", "Germany", "Greece",
                        "Hungary", "Iceland", "Ireland", "Italy", "Latvia", "Liechtenstein",
                        "Lithuania", "Luxembourg", "Malta", "Monaco", "Netherlands", "Norway",
                        "Poland", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland")

# Assign region
ghg_data <- ghg_data %>%
  mutate(region = ifelse(country_or_area %in% european_countries, "Europe", "Non-Europe"))


filtered_data <- ghg_data %>% filter(year %in% c(1990, 2014))

ggplot(filtered_data, aes(x = as.factor(year), y = value, fill = category)) +
  geom_boxplot(outlier.shape = NA) + 
  scale_y_log10() +  
  facet_wrap(~ category, scales = "free_y") +
  labs(title = "Emission Distribution in 1990 vs 2014 (All Country)",
       x = "Year", y = "Emission Value (log scale)") +
  theme_minimal()

ggplot(ghg_data, aes(x = region, y = value, fill = category)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_log10() +  
  labs(title = "Comparison of Emissions: Europe vs Non-Europe",
       x = "Region", y = "Emission Value (log scale)") +
  theme_minimal()

ggplot(ghg_data, aes(x = year, y = value, color = category)) +
  geom_line(stat="summary", fun="mean", linewidth=1.2) + 
  labs(title = "Yearly Trends of Different Emission Categories",
       x = "Year", y = "Mean Emission Value") +
  theme_minimal()


filtered_data <- ghg_data %>% filter(year %in% c(1990, 2014))

ggplot(filtered_data, aes(x = value, fill = as.factor(year))) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ category, scales = "free") +
  scale_x_log10() +
  labs(title = "Emission Distribution Comparison (1990 vs 2014)",
       x = "Emission Value (log scale)", y = "Density") +
  theme_minimal()

#Additional Visuals

# Reshape data to wide format for correlation
wide_data <- ghg_data %>%
  select(country_or_area, year, category, value) %>%
  pivot_wider(names_from = category, values_from = value)

# Compute correlation matrix
emission_corr <- cor(wide_data[, -c(1,2)], use = "complete.obs")

ggcorrplot(emission_corr, method = "square", type = "lower", lab = TRUE) +
  ggtitle("Correlation Between Emission Categories")


#Percentage Change between the year 1990 to 2014

# Aggregate emissions for each country and category
agg_ghg_data <- ghg_data %>%
  group_by(country_or_area, category, year) %>%
  summarize(total_value = sum(value, na.rm = TRUE), .groups = 'drop') %>%
  filter(year %in% c(1990, 2014))  # Keep only 1990 and 2014

# Reshape data to wide format for 1990 vs 2014
change_data <- agg_ghg_data %>%
  pivot_wider(names_from = year, values_from = total_value, names_prefix = "year_") %>%
  mutate(Percentage_Change = ((year_2014 - year_1990) / year_1990) * 100) %>%
  filter(!is.na(Percentage_Change))  # Remove missing values

# Ensure Percentage_Change is within 0-100% range
change_data <- change_data %>%
  mutate(Percentage_Change = pmin(pmax(Percentage_Change, 0), 100))
library(RColorBrewer)

# Create a custom green-to-red color gradient
color_palette <- colorRampPalette(c("green", "yellow", "orange", "red"))(100)

# Assign region classification
change_data <- change_data %>%
  mutate(region = ifelse(country_or_area %in% european_countries, "Europe", "Non-Europe"))

# Function to plot heatmap
plot_heatmap <- function(region_filter) {
  ggplot(change_data %>% filter(region == region_filter),
         aes(x = category, y = reorder(country_or_area, Percentage_Change), fill = Percentage_Change)) +
    geom_tile() +
    scale_fill_gradientn(colors = color_palette, limits = c(0, 100)) +  # Continuous green-red scale
    labs(title = paste(region_filter, "GHG Emission % Change (1990-2014)"),
         x = "Emission Category", y = "Country", fill = "% Change") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Display heatmaps for Europe & Non-Europe
plot_heatmap("Europe")
plot_heatmap("Non-Europe")

