# Load packages -----------------------------------------------------------
library(tidyverse)
library(xlsx)
library(rethinking)

# Write function to read in raw data --------------------------------------
read_plate <- function(file) {
  # read data
  raw <- read.xlsx(
    file,
    sheetIndex = 1,
    rowIndex = c(5:12, 15:22, 25:32),
    colIndex = c(2:13),
    header = FALSE,
    as.data.frame = TRUE
  )
  
  # flatten data
  flat <- unlist(raw)
  
  # create dataframe of row and column IDs
  identifiers <- expand.grid(LETTERS[1:8], 1:12)
  
  # create tibble
  tibble(
    Absorbance = flat,
    Well = rep(paste0(identifiers[, 1], identifiers[, 2]), 3),
    Replicate = rep(1:3, each = 96)
    ) %>%
    mutate(
      Plate = str_remove(file, "_[^_]+$") %>% 
        str_remove("data/"),
      Reader = str_remove(file, ".*_") %>% 
        str_remove("data/") %>%
        str_remove(".xlsx")
      )
}

# Read and concatenate all files ------------------------------------------
file_list <- list.files("data", full.names = TRUE)
all_data <- map_df(file_list, read_plate)
glimpse(all_data)

# Remove erroneous wells --------------------------------------------------
clean_data <- 
  all_data %>%
  filter( !(Plate == "standards" & as.integer(str_sub(Well, 2, -1)) > 6) )

# Plot data by well -------------------------------------------------------
theme_elements <- list(theme_bw(),
                       theme(panel.grid = element_blank(), 
                             axis.text.x = element_blank(),
                             axis.ticks.x = element_blank(),
                             legend.position = c(0.85, 0.18))
                       )

ggplot(clean_data, aes(Well, Absorbance, 
                     Absorbance, 
                     col = Reader, 
                     shape = as.factor(Replicate))) +
  facet_wrap( ~ Plate, scales = "free_x") +
  geom_point() +
  scale_color_brewer(type = "qual", palette = "Set1") +
  theme_elements

ggplot(clean_data, aes(Well, Absorbance, 
                     Absorbance, 
                     col = Reader 
                     )) +
  facet_wrap( ~ Plate, scales = "free_x") +
  geom_point(stat = "summary", fun = "mean") +
  scale_color_brewer(type = "qual", palette = "Set1") +
  theme_elements

# Plot data by reader -----------------------------------------------------
wide <- pivot_wider(clean_data, names_from = Reader, values_from = Absorbance)

ggplot(wide, aes(single, hotel, col = Plate)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

ggplot(filter(all_data, Plate == "standards"), aes(Reader, 
                     Absorbance, 
                     group = interaction(Plate, Well, Replicate))
       ) +
  facet_wrap( ~ Plate) +
  geom_point(size = 0.2) +
  geom_line(size = 0.2, alpha = 0.5) +
  theme_bw()



# Format data for modeling ------------------------------------------------

