# Load packages -----------------------------------------------------------
library(tidyverse)
library(xlsx)
library(ggtext)
library(rethinking)
library(tidybayes)
library(tidybayes.rethinking)

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
  identifiers <- expand.grid(LETTERS[1:8], rep(1:12, each = 3))
  
  # create tibble
  tibble(
    Absorbance = flat,
    Well = paste0(identifiers[, 1], identifiers[, 2]),
    Replicate = rep( rep(1:3, each = 8), 12)
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
                             legend.position = "none",
                             strip.background = element_rect(fill = NA, colour = NA),
                             strip.text = element_text(size = 14, face = "bold"))
                       )

ggplot(clean_data, aes(Well, Absorbance, 
                     Absorbance, 
                     col = Reader 
                     )) +
  facet_wrap( ~ Plate, scales = "free_x") +
  geom_point(stat = "summary", fun = "mean") +
  scale_color_brewer(type = "qual", palette = "Set1") +
  labs(title = "**Comparing per-well absorbance values between plate readers**",
       subtitle =
  "<span style='font-size:11pt'>Each point is the mean of three repeat readings per plate, read from the
    <span style='color:#377EB8;'>**original**</span> and
    <span style='color:#E41A1C;'>**new**</span> plate readers.
    </span>") +
  theme_elements +
  theme(
    plot.subtitle = element_markdown(lineheight = 1.1),
    plot.title = element_markdown(lineheight = 1.1),
  )

ggsave("plots/Per-well differences.png", width = 10, height = 6)

# Plot data by reader -----------------------------------------------------
wide <- pivot_wider(clean_data, names_from = Reader, values_from = Absorbance)

ggplot(wide, aes(single, hotel, col = Plate, group = Replicate)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  labs(x = "Original reader", y = "New reader",
       title = "**Plotting the proportionality of absorbances across readers**",
       subtitle =
         "<span style='font-size:11pt'>Each point corresponds to a single read's
         absorbance values on the original <br> and new plate readers. Solid diagonal
         line indicates direct proportionality.
    </span>") +
  coord_equal() +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        plot.subtitle = element_markdown(lineheight = 1.1),
        plot.title = element_markdown(lineheight = 1.1)
  )

ggsave("plots/Proportionality.png", width = 6, height = 6)

ggplot(clean_data, aes(
  fct_recode(Reader, "New" = "hotel", "Original" = "single"),
  Absorbance,
  group = interaction(Plate, Well, Replicate))
       ) +
  facet_wrap( ~ Plate) +
  geom_point(size = 0.2) +
  geom_line(size = 0.2, alpha = 0.2) +
  labs(title = "**Plotting the per-read differences between plate readers**",
       subtitle =
         "<span style='font-size:11pt'>Each point corresponds to a single read's
         absorbance values on the original and new plate readers. <br> Lines 
         connect the same read on the new and original plate readers.
    </span>") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, colour = NA),
        strip.text = element_text(size = 14, face = "bold"),
        axis.title.x = element_blank(),
        plot.subtitle = element_markdown(lineheight = 1.1),
        plot.title = element_markdown(lineheight = 1.1),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())

ggsave("plots/Differences_between_readers.png", width = 10, height = 6)

# Plot standards ----------------------------------------------------------
clean_data %>%
  filter(Plate == "standards") %>%
  mutate(Concentration = str_sub(Well, 1, 1) %>% as.factor() %>% as.integer) %>%
  ggplot(aes(Concentration, Absorbance)) +
  facet_wrap(~ fct_recode(Reader, "New" = "hotel", "Original" = "single")) +
  geom_line(stat = "summary", fun = "median") +
  labs(title = "**Standard curves for each plate reader**",
       subtitle =
         "<span style='font-size:11pt'>Each point corresponds to a single read. 
          Line connects median read at each concentration. <br> Note the outlying
         well due to human error. Actual concentrations not shown.
    </span>") +
  scale_x_reverse() +
  geom_point() +
  scale_color_brewer(type = "qual", palette = "Set1") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, colour = NA),
        strip.text = element_text(size = 14, face = "bold"),
        plot.subtitle = element_markdown(lineheight = 1.1),
        plot.title = element_markdown(lineheight = 1.1),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())

ggsave("plots/Standards_by_readers.png", width = 10, height = 6)

# Format data for modeling ------------------------------------------------
model_data <- clean_data %>%
  mutate(
    Absorbance = Absorbance,
    Well = as.factor(Well),
    Plate = as.factor(Plate),
    Reader = as.factor(Reader)
    )

glimpse(model_data)

# Train model -------------------------------------------------------------
mod <- ulam(
  alist(
    # likelihood function
    absorbance ~ normal(mu, sigma),
    
    # linear model
    mu <- a_well[Well] + b_well[Well]*Reader,
    
    # priors
    c(a_well, b_well)[well] ~ multi_normal(c(a, b), rho, sigma_well),
    c(a, b) ~ normal(0, 0.5),
    sigma_well ~ exponential(5),
    sigma ~ exponential(2),
    rho ~ lkj_corr(2)
  ), data = data_list, chains = 4, cores = 4
)

mod2 <- ulam(
  alist(
    # likelihood function
    Absorbance ~ normal(mu, sigma),
    
    # linear model
    mu <- a_well[Well] + a_plate[Plate] + b_reader[Reader],
    
    # generated quantities
    gq> reader_diff <- b_reader[1] - b_reader[2],
    gq> hotel <- b_reader[1],
    gq> single <- b_reader[2],
    
    # priors
    a_well[Well] ~ normal(0, tau_well),
    a_plate[Plate] ~ normal(0, tau_plate),
    b_reader[Reader] ~ normal(1.5, 0.2),
    sigma ~ exponential(1),
    
    # hyperpriors
    tau_well ~ exponential(3),
    tau_plate ~ exponential(1)
  ), 
  data = model_data, 
  chains = 4, cores = 4, iter = 4000, warmup = 2000, 
  control = list(adapt_delta = 0.99)
)

plot(mod2)

traceplot(mod2)

# Visualizing parameters ---------------------------------------------------
mod2 %>%
  recover_types(model_data) %>%
  gather_draws(tau_well, tau_plate, sigma, hotel, single, reader_diff, 
               n = 4000) %>%
  ggplot(aes(.value, .variable)) +
  stat_pointinterval(point_interval = "median_qi", 
                     .width = c(0.79, 0.89, 0.99)) +
  theme_bw()

precis(mod2, prob = 0.95)

# Simulating new data from the model --------------------------------------
post_pred <- add_fitted_draws(model_data, mod2)

MAP <- post_pred %>%
  group_by(.row, Well, Replicate, Plate, Reader) %>%
  summarise(MAP_abs = mean(Absorbance))

ggplot(post_pred, aes(
  fct_recode(Reader, "New" = "hotel", "Original" = "single"),
  Absorbance,
  group = interaction(Plate, Well, Replicate))
) +
  facet_wrap( ~ Plate) +
  geom_point(size = 0.2) +
  geom_line(size = 0.2, alpha = 0.5) +
  labs(title = "**Plotting the per-read differences between plate readers**",
       subtitle =
         "<span style='font-size:11pt'>Each point corresponds to a single read's
         absorbance values on the original and new plate readers. <br> Lines 
         connect the same read on the new and original plate readers.
    </span>") +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA, colour = NA),
        strip.text = element_text(size = 14, face = "bold"),
        axis.title.x = element_blank(),
        plot.subtitle = element_markdown(lineheight = 1.1),
        plot.title = element_markdown(lineheight = 1.1),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())
