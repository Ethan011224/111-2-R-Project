#rjava and remotes for installing "tabulizer"
install.packages("rJava", "Remotes")
library(rJava)
library(remotes)

#installing Tubalizer
remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch")

#Library
library(tabulizer)
library(dplyr)

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

#merge all dataframe into one for easy operation
merged_df <- merge(merge(df_list[[1]], df_list[[2]], all = TRUE), df_list[[3]], all = TRUE)

merged_df[is.na(merged_df) | merged_df == ""] <- 0

header_order <- c("Location", sort(names(merged_df)[-which(names(merged_df) == "Location")]))
merged_df <- merged_df[, header_order]

combined_row <- apply(merged_df[c(9, 10), ], 2, max)
merged_df <- merged_df[-c(9, 10), ]
merged_df <- rbind(combined_row, merged_df)

merged_df <- merged_df[!(merged_df$Location == "Total at year end"), ]

View(merged_df)

#calculation

