library(tidyverse)
knitr::opts_chunk$set(warnings = FALSE, message = FALSE)


global_inflation = read_csv("https://github.com/zaynez4/stat436/raw/refs/heads/main/project/milestone2/Global%20Dataset%20of%20Inflation.csv")

tidy_global_inflation = 
  as.data.frame(global_inflation %>% 
                  pivot_longer('1970':'2022', names_to = "year", values_to = "rate",
                               values_drop_na=T)) %>% 
  rename(CountryCode = 'Country Code',
         IMF = 'IMF Country Code',
         type = 'Series Name') %>% 
  select(CountryCode, IMF, Country, type, year, rate) %>% 
  arrange(Country)


selected_countries <- c("India", "China", "United States", "Indonesia", "Pakistan", "Nigeria", 
                        "Brazil", "Bangladesh", "Russian Federation", "Ethiopia", "Mexico", 
                        "Japan", "Egypt, Arab Rep.", "Philippines", "Congo, Dem. Rep.", "Vietnam", 
                        "Iran, Islamic Rep.", "Turkey", "Germany", "Thailand")

filtered_data <- tidy_global_inflation %>%
  filter(Country %in% selected_countries & as.numeric(year) >= 2005 & as.numeric(year) <= 2022)

average_inflation <- filtered_data %>%
  group_by(Country, year) %>%
  summarise(avg_rate = mean(rate, na.rm = TRUE)) %>%
  ungroup()

p<- ggplot(average_inflation, aes(x = as.numeric(year), y = avg_rate)) +
  geom_line() +
  labs(title = "Average Inflation Rates by Country (2005-2022)",
       x = "Year", 
       y = "Average Inflation Rate (%)",
       caption = "The shaded red area highlights the COVID-19 pandemic years (2020-2021)") +
  facet_wrap(~Country) +
  theme_minimal() 

p + annotate("rect", xmin = 2020, xmax = 2021, ymin = -Inf, ymax = Inf, 
             alpha = 0.2, fill = "red") 
