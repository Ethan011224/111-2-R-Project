#rjava and remotes for installing "tabulizer"
install.packages("remotes")
install.packages("ggplot2")
install.packages("dplyr")
library(remotes)

#installing Tubalizer
remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch")

#Library
library(tabulizer)
library(dplyr)
library(ggplot2)

#Vector of PDF file names
pdf_files <- c("2020_Annual_IMB_Piracy_Report.pdf", "2021_Annual_IMB_Piracy_Report.pdf", "2022_Annual_IMB_Piracy_Report.pdf")

#Empty list to store dataframe
df_list <- list()

#For loop to import tables from each PDF file
for(i in 1:length(pdf_files)){
  tables <- extract_tables(pdf_files[i], pages = 6)
  df <- as.data.frame(tables[[1]])
  
  #cleaning the dataframe
  row_index <- nrow(df)
  
  for(j in 1:row_index){
    if(is.na(df[j, 2]) || df[j, 2] == ""){
    df[j, 2] <- df[j, 1]
    }
  }
  colnames(df) <- df[1, ]
  df <- df[-1, ]
  df <- df[ ,-1]
  
  df[, -1] <- apply(df[, -1], 2, function(x) gsub("\\s+", "", x, perl = TRUE))
  df_list[[i]] <- df
  View(df_list[[i]])
}

#merge all data frame into one for easy operation
merged_df <- merge(merge(df_list[[1]], df_list[[2]], all = TRUE), df_list[[3]], all = TRUE)

merged_df[is.na(merged_df) | merged_df == ""] <- 0

header_order <- c("Location", sort(names(merged_df)[-

which(names(merged_df) == "Location")]))
merged_df <- merged_df[, header_order]

merged_df <- merged_df %>%  group_by(Location) %>%
  summarise_all(max)

merged_df <- merged_df[!(merged_df$Location == "Total at year end"), ]
merged_df[, !(names(merged_df) %in% "Location")] <- lapply(merged_df[, !(names(merged_df) %in% "Location")], as.numeric)

View(merged_df)

#calculation and graph
merged_df$Total <- rowSums(merged_df[, -1])
final_df <- merged_df
sum_row <- colSums(final_df[, -1])
total_row <- c("Total of year", sum_row)
final_df <- rbind(final_df, total_row)
View(final_df)

ggplot(data = merged_df) +
  geom_point(aes(x = Total, y = Location)) +
  labs(x = "Reported Incidents", y = "Country", title = "Piracy Activity By Country 2016-2022") +
  scale_x_continuous(breaks = seq(min(0), max(200), by = 10))
