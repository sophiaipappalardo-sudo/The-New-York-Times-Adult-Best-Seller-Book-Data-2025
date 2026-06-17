# Final Project R Code by Sophia Pappalardo



#Part 1: PDF to Dataframe  ---------------------------------------------------
# Creating a function to convert a pdf to a dataframe in R.

# Load Libraries
library(pdftools)
library(stringr)
library(dplyr)
library(purrr)


# Start of Function pdf_to_df
pdf_to_df <- function(url) {
  
  tmp <- tempfile(fileext = ".pdf")
  download.file(url, tmp, mode = "wb")
  
  # Extract text from pdf pages
  pages <- pdf_text(tmp)
  
  # Extracting the Date from The Page and Assigning it to all Books
  all_text <- paste(pages, collapse = " ")
  report_date <- str_extract(all_text, 
                             "[A-Za-z]+\\s+\\d{1,2},\\s+\\d{4}") |>
    as.Date(format = "%B %d, %Y")
  
  # Get the Genre (Fiction or Nonfiction) from which page the book is 
  # located
  page_lines <- lapply(pages, function(p) {
    x <- str_split(p, "\n")[[1]]
    x <- str_trim(x)
    x[x != ""]
  })
  
  get_category <- function(lines) {
    header <- paste(lines[1:5], collapse = " ")
    
    if (str_detect(header, regex("non.?fiction", ignore_case = TRUE))) {
      "Nonfiction"
    } else if (str_detect(header, regex("fiction", ignore_case = TRUE))) {
      "Fiction"
    } else {
      NA_character_
    }
  }
  
  all_blocks <- list()
  
  for (i in seq_along(page_lines)) {
    lines <- page_lines[[i]]
    category <- get_category(lines)
    
    start_idx <- which(str_detect(lines, "^\\d+\\b"))
    
    if (length(start_idx) == 0) next
    
    blocks <- map(seq_along(start_idx), function(j) {
      start <- start_idx[j]
      end <- if (j < length(start_idx)) start_idx[j+1] - 1 else length(lines)
      
      list(
        text = lines[start:end],
        category = category
      )
    })
    
    all_blocks <- append(all_blocks, blocks)
  }
  
  # Create the function parse_block to partition all_blocks into what 
  # I want.
  parse_block <- function(block_obj) {
    block <- block_obj$text
    category <- block_obj$category
    
    first_line <- block[1]
    text_block <- paste(block, collapse = " ")
    
    tokens <- str_extract_all(first_line, "\\d+|--")[[1]]
    
    # This week value is always the FIRST number
    this_week <- as.integer(tokens[1])
    
    # Last two tokens correspond to Last Week Position and Total Weeks
    if (length(tokens) >= 3) {
      last_token <- tokens[length(tokens) - 1]
      weeks_token <- tokens[length(tokens)]
    } else if (length(tokens) == 2) {
      last_token <- NA
      weeks_token <- tokens[2]
    } else {
      last_token <- NA
      weeks_token <- NA
    }
    
    last_week <- if (!is.na(last_token) && last_token != "--") {
      as.integer(last_token)
    } else {
      NA_integer_
    }
    
    total_weeks <- if (!is.na(weeks_token) && weeks_token != "--") {
      as.integer(weeks_token)
    } else {
      NA_integer_
    }
    
    # Combining Created Vectors into a single data frame and changing names
    # to be better understood
    tibble(
      this_week_position = this_week,
      last_week_position = last_week,
      total_weeks = total_weeks,
      category = category,
      full_text = text_block,
      date = report_date
    )
  }
  # End of Function parse_block
  
  # Finally, apply function parse_block to the data in all_blocks to get the
  # desired data frame
  df <- map_dfr(all_blocks, parse_block)
  
  #Now want to split up the full_text into the three columns that I want:
  # Title, Author, and Publisher
  df <- df |>
    mutate(
      # TITLE = before ", by"
      book_title = str_extract(full_text, "^.*?(?=, by)") |>
        str_replace("^\\d+\\s+", "") |>   # removes leading position number
        str_squish(),
      
      # AUTHOR = after "by" and before the period
      author = str_extract(full_text, "(?<=by\\s).+?(?=\\()"),
      
      # PUBLISHER = located inside parentheses
      publisher = str_extract(full_text, "(?<=\\().+?(?=\\))")
    )
  
  # Now I will reorder the Columns in a way that makes logical sense to me
  df <- df |>
    select(
      date,
      book_title,
      author,
      publisher,
      category,
      this_week_position,
      last_week_position,
      total_weeks,
      full_text
    )
  
  return(df)
}
# End of function pdf_to_df


# Part 2: Creating the Dataframe ---------------------------------------------
# Using the fucntion in part 1 to converte the pdf's into multiple
# dataframes, combining them, and cleaning them up into one. 

# I will create a data frame for each week in the Year 2025
df1 <- pdf_to_df("https://www.hawes.com/2025/2025-01-05.pdf")

df2 <- pdf_to_df("https://www.hawes.com/2025/2025-01-12.pdf")

df3 <- pdf_to_df("https://www.hawes.com/2025/2025-01-19.pdf")

df4 <- pdf_to_df("https://www.hawes.com/2025/2025-01-26.pdf")

df5 <- pdf_to_df("https://www.hawes.com/2025/2025-02-02.pdf")

df6 <- pdf_to_df("https://www.hawes.com/2025/2025-02-09.pdf")

df7 <- pdf_to_df("https://www.hawes.com/2025/2025-02-16.pdf")

df8 <- pdf_to_df("https://www.hawes.com/2025/2025-02-23.pdf")

df9 <- pdf_to_df("https://www.hawes.com/2025/2025-03-02.pdf")

df10 <- pdf_to_df("https://www.hawes.com/2025/2025-03-09.pdf")

df11 <- pdf_to_df("https://www.hawes.com/2025/2025-03-16.pdf")

df12 <- pdf_to_df("https://www.hawes.com/2025/2025-03-23.pdf")

df13 <- pdf_to_df("https://www.hawes.com/2025/2025-03-30.pdf")

df14 <- pdf_to_df("https://www.hawes.com/2025/2025-04-06.pdf")

df15 <- pdf_to_df("https://www.hawes.com/2025/2025-04-13.pdf")

df16 <- pdf_to_df("https://www.hawes.com/2025/2025-04-20.pdf")

df17 <- pdf_to_df("https://www.hawes.com/2025/2025-04-27.pdf")

df18 <- pdf_to_df("https://www.hawes.com/2025/2025-05-04.pdf")

df19 <- pdf_to_df("https://www.hawes.com/2025/2025-05-11.pdf")

df20 <- pdf_to_df("https://www.hawes.com/2025/2025-05-18.pdf")

df21 <- pdf_to_df("https://www.hawes.com/2025/2025-05-25.pdf")

df22 <- pdf_to_df("https://www.hawes.com/2025/2025-06-01.pdf")

df23 <- pdf_to_df("https://www.hawes.com/2025/2025-06-08.pdf")

df24 <- pdf_to_df("https://www.hawes.com/2025/2025-06-15.pdf")

df25 <- pdf_to_df("https://www.hawes.com/2025/2025-06-22.pdf")

df26 <- pdf_to_df("https://www.hawes.com/2025/2025-06-29.pdf")

df27 <- pdf_to_df("https://www.hawes.com/2025/2025-07-06.pdf")

df28 <- pdf_to_df("https://www.hawes.com/2025/2025-07-13.pdf")

df29 <- pdf_to_df("https://www.hawes.com/2025/2025-07-20.pdf")

df30 <- pdf_to_df("https://www.hawes.com/2025/2025-07-27.pdf")

df31 <- pdf_to_df("https://www.hawes.com/2025/2025-08-03.pdf")

df32 <- pdf_to_df("https://www.hawes.com/2025/2025-08-10.pdf")

df33 <- pdf_to_df("https://www.hawes.com/2025/2025-08-17.pdf")

df34 <- pdf_to_df("https://www.hawes.com/2025/2025-08-24.pdf")

df35 <- pdf_to_df("https://www.hawes.com/2025/2025-08-31.pdf")

df36 <- pdf_to_df("https://www.hawes.com/2025/2025-09-07.pdf")

df37 <- pdf_to_df("https://www.hawes.com/2025/2025-09-14.pdf")

df38 <- pdf_to_df("https://www.hawes.com/2025/2025-09-21.pdf")

df39 <- pdf_to_df("https://www.hawes.com/2025/2025-09-28.pdf")

df40 <- pdf_to_df("https://www.hawes.com/2025/2025-10-05.pdf")

df41 <- pdf_to_df("https://www.hawes.com/2025/2025-10-12.pdf")

df42 <- pdf_to_df("https://www.hawes.com/2025/2025-10-19.pdf")

df43 <- pdf_to_df("https://www.hawes.com/2025/2025-10-26.pdf")

df44 <- pdf_to_df("https://www.hawes.com/2025/2025-11-02.pdf")

df45 <- pdf_to_df("https://www.hawes.com/2025/2025-11-09.pdf")

df46 <- pdf_to_df("https://www.hawes.com/2025/2025-11-16.pdf")

df47 <- pdf_to_df("https://www.hawes.com/2025/2025-11-23.pdf")

df48 <- pdf_to_df("https://www.hawes.com/2025/2025-11-30.pdf")

df49 <- pdf_to_df("https://www.hawes.com/2025/2025-12-07.pdf")

df50 <- pdf_to_df("https://www.hawes.com/2025/2025-12-14.pdf")

df51 <- pdf_to_df("https://www.hawes.com/2025/2025-12-21.pdf")

df52 <- pdf_to_df("https://www.hawes.com/2025/2025-12-28.pdf")


# Changing the values of specific cells
# df["rowName", "columnName"] <- New_value

df1[24, "publisher"] = "St. Martin's." # to fix from "St. 7  15 Martin's." 

df1[26, "author"] = "Lisa Marie Presley and Riley Keough." 

df2[25, "publisher"] = "St. Martin's."

df2[24, "author"] = "Lisa Marie Presley and Riley Keough."

df3[22, "publisher"] = "St. Martin's."

df3[27, "author"] = "Lisa Marie Presley and Riley Keough."

df4[26, "publisher"] = "St. Martin's."

df5[19, "author"] = "Brooke Shields with Rachel Bertsche."

df5[29, "publisher"] = "St. Martin's."

df6[28, "publisher"] = "St. Martin's."

df7[26, "author"] = "Peter Beinart."

df7[27, "author"] = "Brooke Shields with Rachel Bertsche."

df7[28, "publisher"] = "St. Martin's."

df8[11, "publisher"] = "Random House Worlds."

df8[25, "author"] = "Peter Beinart."

df8[29, "publisher"] = "St. Martin's."

df9[3, "publisher"] = "Del Rey."

df9[29, "publisher"] = "St. Martin's."

df10[17, "author"] = "Alexander C. Karp and Nicholas W. Zamiska."

df10[30, "publisher"] = "St. Martin's."

df11[18, "author"] = "Alexander C. Karp and Nicholas W. Zamiska."

df11[22, "author"] = "Omar El Akkad."

df12[20, "author"] = "Mary Ellen Matthews with Alison Castle and Emily Oberman."

df12[21, "author"] = "Alexander C. Karp and Nicholas W. Zamiska."

df12[26, "author"] = "Chip Wilson."

df13[27, "author"] = "Alexander C. Karp and Nicholas W. Zamiska."

df14[30, "author"] = "Alexander C. Karp and Nicholas W. Zamiska."

df14[21, "book_title"] = "WHO IS GOVERNMENT?"

df15[20, "publisher"] = "Penguin Press."

df15[23, "book_title"] = "WHO IS GOVERNMENT?"

df16[22, "publisher"] = "Park Row."

df16[25, "book_title"] = "WHO IS GOVERNMENT?"

df17[20, "book_title"] = "WHO IS GOVERNMENT?"

df17[27, "publisher"] = "Park Row."

df18[25, "book_title"] = "WHO IS GOVERNMENT?"

df18[30, "author"] = "Omar El Akkad."

df19[22, "book_title"] = "WHO IS GOVERNMENT?"

df21 <- df21[-23, ]

df24[28, "publisher"] = "Grand Central."

df25[2, "publisher"] = "Little, Brown and Knopf."

df25[17, "author"] = "Elias Weiss Friedman with Ben Greenman."

df26[4, "publisher"] = "Little, Brown and Knopf."

df26[29, "book_title"] = "WHO IS GOVERNMENT?"

df27[5, "publisher"] = "Little, Brown and Knopf."

df28[6, "publisher"] = "Little, Brown and Knopf."

df28[28, "author"] = "Elias Weiss Friedman with Ben Greenman."

df29[11, "publisher"] = "Little, Brown and Knopf."

df30[2, "author"] = "Brigitte Knightley."

df31 <- df31[-17, ]

df33[28, "author"] = "Omar El Akkad."

df33[23, "publisher"] = "Skyhorse/Children’s Health Defense."

df34 <- df34[-22, ]

df35[18, "author"] = "Alyson Stoner."

df35 <- df35[-22, ]

df36[3, "author"] = "Christopher Golden and Brian Keene."

df36 <- df36[-19, ]

df37 <- df37[-19, ]

df37[30, "author"] = "Omar El Akkad."

df38[6, "publisher"] = "Little, Brown."

df38[24, "publisher"] = "St. Martin’s Essentials."

df39[11, "publisher"] = "Little, Brown."

df39[22, "author"] = "Rob Reiner, Christopher Guest, Michael McKean and Harry Shearer with David Kamp."

df40[23, "author"] = "Eliezer Yudkowsky and Nate Soares."

df41[20, "author"] = "Priscilla Beaulieu Presley with Mary Jane Ross."

df41[27, "author"] = "Steven Pinker."

df42[29, "publisher"] = "St. Martin’s Essentials."

df42[30, "author"] = "Priscilla Beaulieu Presley with Mary Jane Ross."

df44[2, "publisher"] = "Grand Central."

df44[15, "author"] = "Matthew Stover."

df45[7, "publisher"] = "Grand Central."

df45[17, "author"] = "Bret Baier with Catherine Whitney."

df46[6, "publisher"] = "Grand Central."

df46[19, "author"] = "Elyse Myers."

df46[21, "author"] = "Bret Baier with Catherine Whitney."

df47[4, "publisher"] = "Grand Central."

df47[14, "book_title"] = "QUEEN ESTHER"

df47[25, "author"] = "Bret Baier with Catherine Whitney."

df47[30, "author"] = "Elyse Myers."

df47[27, "book_title"] = "WE DID OK, KID"

df48[10, "publisher"] = "Grand Central."

df48[23, "author"] = "Bret Baier with Catherine Whitney."

df49[15, "publisher"] = "Grand Central."

df49[20, "publisher"] = "Simon & Schuster."

df49[25, "publisher"] = "Grand Central."

df49[29, "author"] = "Bret Baier with Catherine Whitney."

df50[5, "publisher"] = "Grand Central."

df50[23, "publisher"] = "Simon & Schuster."

df50[28, "author"] = "Bret Baier with Catherine Whitney."

df50[30, "publisher"] = "Grand Central."

df51[11, "publisher"] = "Grand Central."

df51[26, "publisher"] = "Simon & Schuster."

df51[27, "author"] = "Bret Baier with Catherine Whitney."

df52[14, "publisher"] = "Grand Central."

df52[23, "author"] = "Bret Baier with Catherine Whitney."

df52[24, "publisher"] = "Simon & Schuster."


# Now combine into one data frame, clean some more, and save to computer
df_2025 <- bind_rows(mget(paste0("df", 1:52)))
df_2025$author <- str_remove_all(df_2025$author, "\\.")

# I will now create a file path unique to my computer
file_path <- "C:/Users/Sophi/OneDrive/Documents/School Assigments/Data Visualization/Final Project/df_2025.csv"
write.csv(df_2025, file_path, row.names = FALSE)


# Since I have saved the csv file in the same folder as this .qmd file,
# I can load it in with the following code. This prevents me from having to run
# All the code from above again when I want to use it.
df_2025 <- read_csv("df_2025.csv")


#Part 3: Createing Data Visualizations ---------------------------------------

library(tidyverse)
library(dplyr)
library(stringr)

# Make sure that df_2025 is loaded into R (see above code)

# Figure 1: Histogram of All Books

  # First create the subset of df_2025 that I will use
  df_book_counts <- df_2025 |>
    count(book_title, author, publisher, category, name = "count")


  # The Plot
  ggplot(df_book_counts, aes(x = count, fill = category)) +
    geom_histogram(binwidth = 1) +
    scale_fill_manual(values = c(
      "Nonfiction" = "steelblue3",
      "Fiction" = "indianred1"
    )) +
    labs(x = "Total Weeks on List",
         y = "Number of books",
         fill = "",
         title = "Most Books Spent Less than 5 weeks on The New York Times Best Sellers List in 2025") +
    theme(plot.title = element_text(size = 20),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15))+
    scale_x_continuous(breaks = seq(0, 50, by = 5)) +
    annotate(
      "curve",
      x = 48, y = 25,
      xend = 48, yend = 1, 
      curvature = 0.2,
      arrow = arrow(length = unit(0.2, "cm")),
      color = "steelblue3"
    ) +
    
    # All annotate functions are used to create the labled arrows on plot
    annotate(
      "text",
      x = 44, y = 31,
      label = "Top Nonfiction Book",
      hjust = 0,
      color = "steelblue3",
      fontface = "bold"
    ) +
    annotate(
      "curve",
      x = 39, y = 25,      
      xend = 39, yend = 1,
      curvature = 0.2,
      arrow = arrow(length = unit(0.2, "cm")),
      color = "indianred1"
    ) +
    annotate(
      "text",
      x = 36, y = 31,
      label = "Top Fiction Book",
      hjust = 0,
      color = "indianred2",
      fontface = "bold"
    )

# Figure 2: Bar Graphs Comparison Of Nonfiction and Fiction
  
  # creation of subset of data for figure 2a Nonfiction
  top10_NF_books <- df_book_counts |>
    filter(category == "Nonfiction") |>
    slice_max(order_by = count, n = 10)
  
  top10_NF_books$book_title <- str_to_title(top10_NF_books$book_title)
  
  top10_NF_books[6, "book_title"] = "Black AF History"
  top10_NF_books[9, "book_title"] = "The House of My Mother"
  
  # The Plot: 2a Nonfiction
  ggplot(top10_NF_books) + 
    geom_col(aes(x = count, 
                 y = reorder(author, count)),
             fill = "steelblue3",
             position = position_dodge(width = 0.8),
             width = 0.7) +
    geom_text(aes(x = 0, 
                  y = author, 
                  label = book_title,
                  group = book_title),
              hjust = 0,
              position = position_dodge(width = 0.8),
              size = 3.5) +
    geom_text(aes(x = count,
                  y = author,
                  label = count,
                  group = book_title),
              position = position_dodge(width = 0.8),
              hjust = -0.2) +
    labs(x = "",
         y = "",
         title = "Nonfiction")+
    theme(plot.title = element_text(size = 20),
          axis.text.y = element_text(face = "bold"))

  # creation of subset of data for figure 2b Fiction
  top10_F_books <- df_book_counts |>
    filter(category == "Fiction") |>
    slice_max(order_by = count, n = 10)
  
  top10_F_books$book_title <- str_to_title(top10_F_books$book_title)
  
  top10_F_books[9, "book_title"] = "The God of the Woods"
  
  # The Plot: 2b Fiction
  ggplot(top10_F_books) + 
    geom_col(aes(x = count, 
                 y = reorder(author, count),
                 group = book_title), 
             fill = "indianred1",
             position = position_dodge(width = 0.8),
             width = 0.7) +
    geom_text(aes(x = 0, 
                  y = author, 
                  label = book_title,
                  group = book_title),
              hjust = 0,
              position = position_dodge(width = 0.8),
              size = 3.5) +
    geom_text(aes(x = count,
                  y = author,
                  label = count,
                  group = book_title),
              position = position_dodge(width = 0.8),
              hjust = -0.2) +
    labs(y = "",
         x = "",
         title = "Fiction")+
    theme(plot.title = element_text(size = 20),
          axis.text.y = element_text(face = "bold"))

  
# Figure 3: Line graph for Position Over Time for Top Books  
  
  # Create the subset
  position_2 <- df_2025 |>
    filter(book_title == "ONYX STORM" | book_title == "THE ANXIOUS GENERATION")
  
  # Create new data for Nonfiction book to be able to graph weeks not 
  # on list as 16 in this_week_position
  date1 <- c("2025-11-09", "2025-11-16", "2025-11-23", "2025-11-30")
  book_title1 <- c("THE ANXIOUS GENERATION", "THE ANXIOUS GENERATION", "THE ANXIOUS GENERATION", "THE ANXIOUS GENERATION")
  author1 <- c("Jonathan Haidt", "Jonathan Haidt", "Jonathan Haidt", "Jonathan Haidt")
  publisher1 <- c("Penguin Press.", "Penguin Press.", "Penguin Press.", "Penguin Press.")
  category1 <- c("Nonfiction", "Nonfiction", "Nonfiction", "Nonfiction")
  this_week_position1 <- c(16, 16, 16, 16)
  last_week_position1 <- c("NA", "NA", "NA", "NA")
  total_weeks1 <- c("NA", "NA", "NA", "NA")
  full_text1 <- c("NA", "NA", "NA", "NA")
  
  NA_NF <- data.frame(
    date = date1,
    book_title = book_title1,
    author = author1,
    publisher = publisher1,
    category = category1,
    this_week_position = this_week_position1,
    last_week_position = last_week_position1,
    total_weeks = total_weeks1,
    full_text = full_text1)
  
  
  # Create new data for fiction book to be able to graph weeks not on 
  # list as 16 in this_week_position
  date2 <- c("2025-10-12", "2025-11-02", "2025-11-16", "2025-11-23", "2025-11-30", "2025-12-07", "2025-12-21","2025-12-28")
  book_title2 <- c("ONYX STORM", "ONYX STORM", "ONYX STORM", "ONYX STORM", "ONYX STORM", "ONYX STORM", "ONYX STORM", "ONYX STORM")
  author2 <- c("Rebecca Yarros", "Rebecca Yarros", "Rebecca Yarros", "Rebecca Yarros", "Rebecca Yarros", "Rebecca Yarros", "Rebecca Yarros", "Rebecca Yarros")
  publisher2 <- c("Red Tower.", "Red Tower.", "Red Tower.", "Red Tower.", "Red Tower.", "Red Tower.", "Red Tower.", "Red Tower.")
  category2 <- c("Fiction", "Fiction", "Fiction", "Fiction", "Fiction", "Fiction", "Fiction", "Fiction")
  this_week_position2 <- c(16, 16, 16, 16, 16, 16, 16, 16)
  last_week_position2 <- c("NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA")
  total_weeks2 <- c("NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA")
  full_text2 <- c("NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA")
  
  NA_F <- data.frame(
    date = date2,
    book_title = book_title2,
    author = author2,
    publisher = publisher2,
    category = category2,
    this_week_position = this_week_position2,
    last_week_position = last_week_position2,
    total_weeks = total_weeks2,
    full_text = full_text2)
  
  # Combine them into position_2 and clean data for use
  position_3 <- rbind(position_2, NA_NF, NA_F)
  
  position_3$date <- as.Date(position_3$date)
  
  position_3$book_title <- str_to_title(position_3$book_title)
  
  # The Plot
  ggplot(data = position_3, 
         aes(x = date, y = this_week_position, color = book_title)) +
    geom_line(linewidth = 1)+
    scale_y_reverse(breaks = seq(1, 16, by = 1)) +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b") +
    labs(title = "The Top Books in Fiction and Nonfiction Started at Higher Positions on the List Early in the Year",
         x = "",
         y = "Position",
         color = "",
         subtitle = "Positions range from 1-15 for each category") +
    theme(plot.title = element_text(size = 20),
          plot.subtitle = element_text(size=15),
          legend.text = element_text(size = 15),
          axis.title.y = element_text(size = 15))+
    scale_color_discrete(
      labels = c("Onyx Storm by Rebecca Yarros", 
                 "The Anxious Generation by Jonathan Haidt")
    ) +
    scale_color_manual(values = c(
      "The Anxious Generation" = "steelblue3",
      "Onyx Storm" = "indianred1"
    ))
