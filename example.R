##### create mock data

set.seed(123) # for reproducibility

# Generate random employee IDs
employee_id <- 1:1000

# Generate random hire dates
hire_date <- sample(seq(as.Date("2018/01/01"), as.Date("2025/01/01"), by = "day"), 1000, replace = TRUE)

# Generate random termination dates
termination_date <- sample(seq(as.Date("2018/01/01"), as.Date("2025/01/01"), by = "day"), 1000, replace = TRUE)
termination_date[which(termination_date <= hire_date)] <- NA # set termination date to NA if it is before hire date

# Randomly set 20% of termination dates to NA
termination_date[sample(length(termination_date), length(termination_date)*0.2)] <- NA

# Combine into a data frame
mock_data <- data.frame(employee_id, hire_date, termination_date)

##### running the data

# load packages
library(tidyverse)
library(hrbrthemes)

# load data
employee_data <- mock_data

# format data as required
df <- employee_data %>%
  mutate(hire_date = as.Date(hire_date, format = "%m/%d/%Y"),
         termination_date = as.Date(termination_date, format = "%m/%d/%Y")) %>%
  mutate(termination_date = if_else(is.na(termination_date), as.Date("2100-12-31"), termination_date))

# calculate monthly headcount
month_seq <- seq(from = min(df$hire_date),
                 to = max(df$hire_date),
                 by = "1 month")

print(month_seq)

# use sequence to start data frame
headcount_data <- data.frame(Date = month_seq)

# calculate monthly headcount
headcount_data <- headcount_data %>%
  mutate(Active.Employees = sapply(Date, function(x) {
           sum(x >= df$hire_date & (is.na(df$termination_date) | x < df$termination_date))
         }))

# add year
headcount_data <- headcount_data %>%
  mutate(year = as.integer(year(Date)))

# add context for each year
headcount_data <- headcount_data %>%
  mutate(context = case_when(
    year == 2018 ~ "Context for 2018",
    year == 2019 ~ "Context for 2019",
    year == 2020 ~ "COVID-19",
    year == 2024 ~ "Where we are today",
    TRUE ~ "No additional context"
  ))

# create basic line graph of all data
headcount_data %>%
  ggplot(aes(x = Date, y = Active.Employees)) +
  geom_area()

# basic zhushing
# annotations
annotation_ending_year <- max(headcount_data$year)
annotation_ending_headcount <- max(headcount_data$Active.Employees)

# titles
labels_title <- "Our Headcount Story"
labels_subtitle <- last(headcount_data$context)

# adding basic zhushing to basic plot
headcount_data %>%
  ggplot(aes(x = Date, y = Active.Employees)) +
  geom_area() +
  labs(title = labels_title,
       subtitle = labels_subtitle) +
  annotate("text",
           x = max(headcount_data$Date),
           y = max(headcount_data$Active.Employees),
           label = annotation_ending_headcount,
           hjust = -.25)

# create a basic plot for each year

# create a vector for unique years
years <- unique(headcount_data$year)

# empty list for plots to go to
plots <- list()

# loop over the each year in years and create plots
for (i in 2:length(years)) {
  # create subset adding one year at a time
  subset_df <- headcount_data %>%
    filter(year <= years[i])

  # calculations for annotation
  ending_year <- max(subset_df$Date)

  annotation_ending_active <- subset_df %>%
    filter(Date == ending_year) %>%
    select(Active.Employees) %>%
    as.numeric()

  # create a plot (p) using the subset
  p <- subset_df %>%
    ggplot(aes(x = Date, y = Active.Employees)) +
    geom_area() +
    labs(title = "Our Headcount Story",
         subtitle = paste(years[i],":", last(subset_df$context))) +
    annotate("text",
             x = max(subset_df$Date),
             y = max(subset_df$Active.Employees),
             label = annotation_ending_active,
             hjust = -.25)

  # save each plot
  ggsave(p,
         file = paste("example_plot_", years[i], ".png"),
         height = 6, width = 8, units = "in")

}

# create final plots

# loop over the each year in years and create plots
for (i in 2:length(years)) {
  # create subset adding one year at a time
  subset_df <- headcount_data %>%
    filter(year <= years[i])

  # calculations for annotation
  ending_year <- max(subset_df$Date)

  ending_active <- subset_df %>%
    filter(Date == ending_year) %>%
    select(Active.Employees) %>%
    as.numeric()

  # create a plot (p) using the subset
  p <- subset_df %>%
    ggplot(aes(x = Date, y = Active.Employees)) +
    geom_area(fill = "#457b9d") +
    labs(title = "Our Headcount Story",
         subtitle = paste(years[i],":", last(subset_df$context)),
         x = "", y = "") +
    scale_x_date(breaks = "1 year", date_labels = "%Y",
                 expand = c(.1,.1),
                 limits = c(min(headcount_data$Date), max(headcount_data$Date))) +
    theme_classic(base_family = "Arial") +
    theme(plot.title = element_text(size = 24, face = "bold", color = "#457b9d"),
          plot.subtitle = element_text(size = 18),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.line.y = element_blank()) +
    annotate("text", x = ending_year,
             y = ending_active, label = ending_active,
             vjust = -1.25, hjust = -.25, color = "#457b9d") +
    annotate("rect",
             xmin = floor_date(max(subset_df$Date), "year"),
             xmax = ceiling_date(max(subset_df$Date), "year"),
             ymin = -Inf, ymax = ending_active + 300,
             alpha = .1, color = "gray", fill = "gray")

  # save each plot
  ggsave(p,
         file = paste("example_plot_final", years[i], ".png"),
         height = 6, width = 8, units = "in")

}
