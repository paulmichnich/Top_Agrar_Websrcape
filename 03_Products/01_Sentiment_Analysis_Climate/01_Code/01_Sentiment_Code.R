## Load the packages
library("syuzhet")
library("dplyr")
library("tidyr")
library("ggplot2")

## Replace with your path
save.path <- ""

article_data <- read.csv(paste0(save.path, "/01_Data_Code/02_Data/heftausgabe_article_titles_2000_2025.csv"))

## Create a vector of relevant German keywords related to climate change and weather
keywords <- c("klimawandel", "klimaänderung", "erderwärmung", "klimaschutz", "klimakatastrophe", 
              "klimakrise", "co2", "treibhausgas", "kohlenstoffdioxid", "klimapolitik",
              "wetter", "unwetter", "hitzewelle", "sturm", "überschwemmung", "hochwasser", "dürre", 
              "regen", "schnee", "hagel", "wettervorhersage", "wetterextrem", 
              "kältewelle", "starkregen", "wetterbedingungen")

## Convert all article titles to lowercase
article_data$Article_Title <- tolower(article_data$Article_Title)

## Initialize a matrix to store counts for each keyword
keyword_counts <- matrix(0, nrow = nrow(article_data), ncol = length(keywords))
colnames(keyword_counts) <- keywords

## Count keyword occurrences in each article title
for (i in 1:length(keywords)) {
  keyword_counts[, i] <- grepl(keywords[i], article_data$Article_Title, ignore.case = TRUE)
}

## Convert the matrix to a data frame for easier manipulation
keyword_counts_df <- as.data.frame(keyword_counts)

## Add the year and article title to the keyword counts dataframe
article_keyword_data <- cbind(article_data[, c("Year", "Article_Title")], keyword_counts_df)

yearly_keyword_summary <- article_keyword_data %>%
  group_by(Year) %>%
  summarise(across(all_of(keywords), sum))

## Convert Year to numeric if it's not already
yearly_keyword_summary$Year <- as.numeric(yearly_keyword_summary$Year)

## Reshape the data into long format for plotting
yearly_keyword_long <- yearly_keyword_summary %>%
  pivot_longer(cols = -Year, names_to = "Keyword", values_to = "Count")

## First plot: All keywords on one plot
plot1 <- ggplot(yearly_keyword_long, aes(x = Year, y = Count, color = Keyword)) +
  geom_line() +
  labs(title = "Häufigkeit von Schlüsselwörtern im Laufe der Zeit", 
       x = "Jahr", 
       y = "Anzahl der Schlüsselwörter in einem 'Top Agrar' Titel",
       color = "Schlüsselwörter",
       # caption = "Diese Grafik zeigt die Anzahl der Vorkommen verschiedener wetter- und klimabezogener Schlüsselwörter (z.B. 'klimawandel', 'co2', 'regen') in Artikelüberschriften über die Jahre hinweg.\nJede Linie stellt ein Schlüsselwort dar, wobei die x-Achse das Jahr und die y-Achse die Anzahl der Vorkommen zeigt.\nDie Farben der Linien stehen für die jeweiligen Schlüsselwörter.") 
       ) + theme_minimal()

# Save the first plot to a file
ggsave(paste0(save.path, "/03_Products/01_Sentiment_Analysis_Climate/02_Figures/keyword_occurrences_over_time.png"),
       plot = plot1, width = 10, height = 6, dpi = 500)

## Second plot: Faceted plot with one plot per keyword
plot2 <- ggplot(yearly_keyword_long, aes(x = Year, y = Count)) +
  geom_line() +
  labs(title = NULL,  # No title as each facet has its own keyword
       x = "Jahr", 
       y = "Anzahl der Schlüsselwörterin einem 'Top Agrar' Titel",
       # caption = "Dieses facettierte Diagramm zeigt das Vorkommen jedes wetter- und klimabezogenen Schlüsselworts im Laufe der Zeit.\nJede Teilgrafik repräsentiert ein einzelnes Schlüsselwort mit der Jahreszahl auf der x-Achse und der Häufigkeit der Vorkommen auf der y-Achse.\nDie y-Achsen-Skala ist für jedes Schlüsselwort unabhängig, um die Unterschiede in der Häufigkeit deutlicher zu visualisieren.") +
  ) + theme_minimal() +
  facet_wrap(~ Keyword, scales = "free_y") +  # Create one plot per keyword with independent y-scales
  theme(strip.text = element_text(size = 10, face = "bold"))

# Save the second plot to a file
ggsave(paste0(save.path, "/03_Products/01_Sentiment_Analysis_Climate/02_Figures/keyword_occurrences_facet_plot.png"),
       plot = plot2, width = 12, height = 8, dpi = 500)
