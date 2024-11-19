# 11/14/2024 - Dylan Dias 

# Data Visualization Project 

# Source: https://www.fao.org/faostat/en/#data/QCL 

# Compare and Contrast Year 2000 - 2010 Total Livestock and Crop Production 
# Finding the Highest Livestock/Crop Grown in both countries 
# Finding the lowest Livestock/Crop Grown in both countries 


#Libraries Used 
library(readxl) 
library(dplyr)
library(tidyverse)
library(gt)
library(gtExtras) 
library(RColorBrewer)
library(ggplot2)
library(plotly)
library(scales)
library(viridis)
library(tidyr)
library(ggthemes)
library(sf)




# Command to load Excel File 
df <- read_excel("C:/Users/dylan/OneDrive/Desktop/projectDV.xls", sheet = 1)

#-------------------------------------------------------------------------------#

# Heatmap for Portugal # 

names(df)
# Convert year from column names if applicable (e.g., "Y2000", "Y2001" etc. as columns)
df <- df %>%
  pivot_longer(cols = starts_with("Y"), names_to = "Year", values_to = "Value", names_repair = "unique") %>%
  mutate(Year = as.numeric(sub("^Y", "", Year))) 

# Filter for Portugal and Spain data (2000 to 2010) 
filtered_spain_portugal_2000_2010 <- df %>%
  filter(Area %in% c("Portugal", "Spain") & Year >= 2000 & Year <= 2010)

# Summarize the data to get total yields per year for Portugal
portugal_data_new <- filtered_spain_portugal_2000_2010 %>%
  filter(Area == "Portugal") %>%
  group_by(Year) %>%
  summarise(Yield = sum(Value, na.rm = TRUE)) %>%
  arrange(desc(Yield)) 

# Find the year with the maximum yield
highlight_year_portugal_new <- portugal_data_new %>%
  filter(Yield == max(Yield, na.rm = TRUE)) %>%
  pull(Year)

# Ensure the year with the max yield is numeric
highlight_year_portugal_new <- as.numeric(highlight_year_portugal_new)

portugal_data_new %>%
  gt() %>%
  tab_header(
    title = md("**Total Crop and Livestock Yield Data in Portugal (2000 - 2010)**"),
    subtitle = md("Comparing Yearly Crop and Livestock Yields Across a Decade")
  ) %>%
  opt_align_table_header(align = "left") %>%
  
  # Apply color to the highest yield row for better contrast
  tab_style(
    style = list(
      cell_fill(color = "lightblue"),  
      cell_text(weight = "bold", color = "black")  
    ),
    locations = cells_body(
      rows = portugal_data_new$Year == highlight_year_portugal_new
    )
  ) %>%
  
  # Source Note
  tab_source_note(
    source_note = md("**Source: _FAO. (2023). Food and Agriculture Organization of the United Nations._**")
  ) %>%
  
  # Align Year column to the left and Yield column to the right
  cols_align(
    align = "left",
    columns = Year
  ) %>%
  cols_align(
    align = "right",
    columns = Yield
  ) %>%
  
  # Table styling options
  tab_options(
    heading.title.font.size = 18,
    heading.subtitle.font.size = 16,
    table.width = pct(80), 
    column_labels.font.size = 14,
    column_labels.font.weight = "bold",
    source_notes.font.size = 10,
    table.border.top.color = "black",  
    table.border.bottom.color = "black",  
    table.border.top.width = px(2),  
    table.border.bottom.width = px(2)  
  ) %>%
  
  # Format Yield numbers with commas for better readability
  fmt_number(
    columns = Yield,
    decimals = 0, 
    use_seps = TRUE
  )
#-------------------------------------------------------------------------------#

# Heatmap for Spain # 

df <- read_excel("C:/Users/dylan/OneDrive/Desktop/projectDV.xls", sheet = 1)

names(df)
# Convert year from column names if applicable (e.g., "Y2000", "Y2001" etc. as columns)
df <- df %>%
  pivot_longer(cols = starts_with("Y"), names_to = "Year", values_to = "Value", names_repair = "unique") %>%
  mutate(Year = as.numeric(sub("^Y", "", Year)))  

# Filter for Portugal and Spain data (2000 to 2010) 
filtered_spain_portugal_2000_2010 <- df %>%
  filter(Area %in% c("Portugal", "Spain") & Year >= 2000 & Year <= 2010)

spain_data %>%
  gt() %>%
  tab_header(
    title = md("**Total Crop and Livestock Yield Data in Spain (2000 - 2010)**"),
    subtitle = md("Comparing Yearly Crop and Livestock Yields Across a Decade")
  ) %>%
  opt_align_table_header(align = "left") %>%
  
  # Apply color to the row with the highest yield for better contrast
  tab_style(
    style = list(
      cell_fill(color = "lightblue"),  
      cell_text(weight = "bold", color = "black")  
    ),
    locations = cells_body(
      rows = spain_data$Year == highlight_year_spain_new
    )
  ) %>%
  
  # Add source note at the bottom
  tab_source_note(
    source_note = md("**Source: FAO. (2023). Food and Agriculture Organization of the United Nations.**")
  ) %>%
  
  # Align Year column to the left and Yield column to the right for better alignment
  cols_align(
    align = "left",
    columns = Year
  ) %>%
  cols_align(
    align = "right",
    columns = Yield
  ) %>%
  
  # Table options for overall styling
  tab_options(
    heading.title.font.size = 18,
    heading.subtitle.font.size = 16,
    table.width = pct(80),  
    column_labels.font.size = 14,
    column_labels.font.weight = "bold",
    source_notes.font.size = 10,
    table.border.top.color = "black",  
    table.border.bottom.color = "black",  
    table.border.top.width = px(2),  
    table.border.bottom.width = px(2)  
  ) %>%
  
  # Format Yield numbers with commas
  fmt_number(
    columns = Yield,
    decimals = 0, 
    use_seps = TRUE
  )
#-------------------------------------------------------------------------------#

# Total Pork between both  countries for 10 years 

pork_data_aggregated <- df %>%
  filter(Area %in% c("Portugal", "Spain")) %>%
  filter(Item %in% c("Edible offal of pigs, fresh, chilled or frozen",
                     "Fat of pigs",
                     "Meat of pig with the bone, fresh or chilled",
                     "Pig fat, rendered",
                     "Swine / pigs")) %>%
  select(Area, Item, Y2000, Y2001, Y2002, Y2003, Y2004, Y2005, Y2006, Y2007, Y2008, Y2009, Y2010) %>%
  # Reshape from wide to long format
  pivot_longer(cols = starts_with("Y"), 
               names_to = "Year", 
               values_to = "Yield") %>%
  # Convert 'Year' to a numeric variable for easier manipulation
  mutate(Year = as.numeric(sub("Y", "", Year))) %>%
  # Aggregate by Year and Area (sum the yields for each item per year)
  group_by(Year, Area) %>%
  summarise(Total_Yield = sum(Yield, na.rm = TRUE), .groups = "drop")

pork_data_aggregated <- pork_data_aggregated %>%
  filter(!is.na(Total_Yield))  # Remove rows with NA values

ggplot(pork_data_aggregated, aes(x = factor(Year), y = Total_Yield, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge") +  # Bar plot with dodge position
  labs(title = md("Yearly Pork Yield Comparison: Portugal vs. Spain (2000-2010)"),
       subtitle = md(" Analyzing the total yield of pork products by country. 
This chart provides a yearly breakdown of pork production 
between Portugal and Spain, two Iberian countries, over the decade."),
       x = "Year",
       y = "Total Yield of Pork (1000KG)",
       caption = md("Source: FAO.(2023).Food and Agriculture Organization of the United Nations."),
       fill = "") +
  scale_fill_manual(values = c("Portugal" = "lightblue", "Spain" = "steelblue")) +
  scale_y_continuous(
    labels = scales::comma  # Add commas to the numbers
  ) +
  
  # Custom color scale for areas
  theme_minimal(base_size = 15) +  # Set base font size
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0, color = "black"),  # Title styling
    plot.subtitle = element_text(size = 12, face = "bold.italic", hjust = 0, color = "black"),  # Title styling
    axis.title = element_text(size = 14, face = "bold", color = "black"),  # Axis title styling
    axis.text = element_text(size = 12, color = "black"),  # Axis text styling
    panel.grid = element_blank(),  # Remove panel grid lines for a cleaner look
    legend.position = "top",
    axis.line = element_line(color = "black", size = 0.5),
    plot.caption = element_text(size = 9, face = "bold.italic", hjust = 0, color = "black")
  )



#-------------------------------------------------------------------------------#


pork_data_aggregated_2 <- df %>%
  filter(Area %in% c("Portugal", "Spain")) %>%
  filter(Item %in% c("Edible offal of pigs, fresh, chilled or frozen",
                     "Fat of pigs",
                     "Meat of pig with the bone, fresh or chilled",
                     "Pig fat, rendered",
                     "Swine / pigs")) %>%
  select(Area, Item, Y2000, Y2001, Y2002, Y2003, Y2004, Y2005, Y2006, Y2007, Y2008, Y2009, Y2010)

# Reshape the data to long format
pork_data_long <- pork_data_aggregated_2 %>%
  pivot_longer(cols = starts_with("Y"), names_to = "Year", values_to = "Yield") %>%
  mutate(Year = as.numeric(sub("^Y", "", Year))) %>%
  group_by(Year, Area) %>%
  summarise(Total_Yield = sum(Yield, na.rm = TRUE), .groups = "drop")

# Create a line plot with separate lines for Portugal and Spain
ggplot(pork_data_long, aes(x = Year, y = Total_Yield, color = Area, group = Area)) +
  geom_line(size = 1) +  # Line plot
   geom_point(size = 3, shape = 16) +
  labs(title = md("Pork Production in Focus: Portugal vs. Spain (2000-2010)"),
       subtitle = md("Identifying the fluctations of Pork Products over a decade"),
       caption = md("Source: FAO.(2023). Food and Agriculture Organization of the United Nations."),
       x = md("Year"),
       y = md("Total Yield of Pork (1000KG)"),
       color = "") +
  scale_y_continuous(
    labels = comma  # Add commas to the numbers
  ) +
  scale_x_continuous(
    breaks = seq(2000, 2010, by = 1), 
    labels = scales::label_number(accuracy = 1, big.mark="")  # Remove decimals from x-axis
  ) +
  scale_color_manual(values = c("Portugal" = "lightblue", "Spain" = "steelblue")) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5, color = "black"),  # Title styling
    plot.subtitle = element_text(size = 12, face = "bold.italic", hjust = 0.5, color = "black"),
    legend.position = "top",
   
    axis.line = element_line(color = "black", size = 0.5),
    plot.caption = element_text(size = 9, face = "bold.italic", hjust = 0),
    )
  
  













