library(tidyverse)
library(readxl)
library(here)
library(skimr)
library(janitor)

# 2. Read the data into R and set the path to the data file

raw_candy_2015 <- read_excel(
  here("raw_data/boing-boing-candy-2015.xlsx")
)

raw_candy_2016 <- read_excel(
  here("raw_data/boing-boing-candy-2016.xlsx")
)

raw_candy_2017 <- read_excel(
  here("raw_data/boing-boing-candy-2017.xlsx")
)

# 2015 DATASET - Initial sort and tidy

candy_clean_2015 <- clean_names(raw_candy_2015) %>% 
  mutate(Year = as.numeric(str_sub(timestamp, start = 1, end = 4)),
         .after = timestamp)%>% 
  rename("Age" = "how_old_are_you", 
         "Trick_or_Treat" = "are_you_going_actually_going_trick_or_treating_yourself",) %>% 
  pivot_longer(cols = c("butterfinger":"york_peppermint_patties"),
               names_to = "Candy_Type",
               values_to = "Rating") %>% 
  add_column(Country = NA,
             State_or_Province = NA,
             Gender = NA) %>% 
  select("Year", "Age", "Trick_or_Treat", "Candy_Type", "Rating", "Country", "State_or_Province", 
         "Gender"); 

# 2016 DATASET - Initial sort and tidy

candy_clean_2016 <- clean_names(raw_candy_2016) %>% 
  mutate(Year = as.numeric(str_sub(timestamp, start = 1, end = 4)),
         .after = timestamp) %>% 
  rename("Age" = "how_old_are_you",
         "Trick_or_Treat" = "are_you_going_actually_going_trick_or_treating_yourself",
         "Gender" = "your_gender",
         "Country" = "which_country_do_you_live_in",
         "State_or_Province" = "which_state_province_county_do_you_live_in") %>% 
 pivot_longer(cols = c("x100_grand_bar":"york_peppermint_patties"),
               names_to = "Candy_Type",
               values_to = "Rating") %>%
  select("Year", "Age", "Trick_or_Treat", "Candy_Type", "Rating", "Country", "State_or_Province",
         "Gender");

# 2017 DATASET - Initial sort and tidy

candy_clean_2017 <- clean_names(raw_candy_2017) %>% 
  rename("Age" = "q3_age", 
         "Gender" = "q2_gender", 
         "Trick_or_Treat" = "q1_going_out", 
         "Country" = "q4_country", 
         "State_or_Province" = "q5_state_province_county_etc") %>%
  pivot_longer(cols = ("q6_100_grand_bar":"q6_york_peppermint_patties"),
               names_to = "Candy_Type",
               values_to = "Rating") %>% 
  add_column(Year = "2017") %>%
  select("Year", "Age", "Trick_or_Treat", "Candy_Type", "Rating", "Country", "State_or_Province",
         "Gender");

# Use 'Bind' to combine the three datasets

merged_candy_data_171819 <-rbind(candy_clean_2015, candy_clean_2016, candy_clean_2017) 

# Tidy the merged dataset

clean_candy_column <- "^q6_"

tidied_candy_data_171819 <- merged_candy_data_171819 %>% 
     mutate(Country = 
            case_when(Country %in% c("'Merica",
                                      "america",
                                      "Ahem....Amerca",
                                      "Alaska",
                                      "America",
                                      "California",
                                      "Eua",
                                      "I Pretend To Be From Canada, But I Am Really From The United States.",
                                      "I pretend to be from Canada, but I am really from the United States.",
                                      "Merica",
                                      "Murrika",
                                      "murrika",
                                      "N. America",
                                      "Narnia",
                                      "New Jersey",
                                      "New York",
                                      "North Carolina",
                                      "Pittsburgh",
                                      "The Best One - Usa",
                                      "The United States Of America",
                                      "united states of america",
                                      "United  States of America",
                                      "The United States",
                                      "UNited States",
                                      "The Yoo Ess Of Aaayyyyyy",
                                      "The Yoo Ess of Aaayyyyyy",
                                      "Trumpistan",
                                      "USA",
                                      "U S A",
                                      "US",
                                      "U S",
                                      "U.s.",
                                      "U.S.",
                                      "us",
                                      "u.s.",
                                      "U.S.A.",
                                      "U.s.a.",
                                      "USa",
                                      "Ud",
                                      "Unhinged States",
                                      "Unied States",
                                      "Unite States",
                                      "United  States Of America",
                                      "United  States of America ",
                                      "United Sates",
                                      "United Staes",
                                      "United State",
                                      "United Statea",
                                      "United Stated",
                                      "united states",
                                      "United states",
                                      "United States Of America",
                                      "United States of America",
                                      "united states of america",
                                      "United Statss",
                                      "United Stetes",
                                      "United Ststes",
                                      "Unites States",
                                      "Units States",
                                      "United States of America ",
                                      "Us Of A",
                                      "u s a",
                                      "Us",
                                      "us",
                                      "US",
                                      "Usa (I Think But It's An Election Year So Who Can Really Tell)",
                                      "USA (I think but it's an election year so who can really tell)",
                                      "Usa Usa Usa Usa",
                                      "USA USA USA USA",
                                      "USA USA USA",
                                      "Usa Usa Usa!!!!",
                                      "Usa Usa Usa",
                                      "USA! USA! USA!",
                                      "Usa! Usa! Usa!", 
                                      "Usa! Usa!", 
                                      "USA! USA!",
                                      "Usa!!!!!!",
                                      "USA!!!!!!",
                                      "Usa!",
                                      "USA!",
                                      "Usa",
                                      "Usa",
                                      "uSA",
                                      "usa",
                                      "Usa? Hard To Tell Anymore..",
                                      "the best one - usa",
                                      "Usaa",
                                      "Usas",
                                      "Usausausa",
                                      "USSA",
                                      "USAA",
                                      "USA USA USA!!!!",
                                      "united States",
                                      "united ststes",
                                      "Ussa",
                                      "united states") ~ "United States",
                       (Country == "Cascadia") & (State_or_Province == "WA") ~ "United States",
                       (Country == "Murica") & (State_or_Province == "Oregon") ~ "United States",
                       (Country == "Murica") & (State_or_Province == "California") ~ "United States",
                       (Country == "Murica") & (State_or_Province == "NJ") ~ "United States",
                       (is.na(Country)) & (State_or_Province == "CA") ~ "United States",
                       (is.na(Country)) & (State_or_Province == "WA") ~ "United States",
                       (is.na(Country)) & (State_or_Province == "PA") ~ "United States",
                       (is.na(Country)) & (State_or_Province == "Massachusetts") ~ "United States",
                       (is.na(Country)) & (State_or_Province == "Arizona") ~ "United States",
                       (is.na(Country)) & (State_or_Province == "Illinois") ~ "United States",
                       (is.na(Country)) & (State_or_Province == "United States") ~ "United States",
                       (is.na(Country)) & (State_or_Province == "USA") ~ "United States",
                       
                       #cleaning data for UK entries                     
                       Country %in% c("United Kindom",
                                      "England",
                                      "england",
                                      "Endland",
                                      "endland",
                                      "Scotland",
                                      "U.k.",
                                      "Uk",
                                      "uk",
                                      "United Kingdom",
                                      "United kingdom") ~ "UK",
                       
                       #cleaning data for Canada entries  
                       Country %in% c("Can",
                                      "canada",
                                      "Canada`",
                                      "Canae",
                                      "CANADA") ~ "Canada",
                       
                       #general cleaning
                       Country == "belgium" ~ "Belgium",
                       Country == "france" ~ "France",
                       Country == "hungary" ~ "Hungary",
                       Country == "Netherlands" ~ "The Netherlands",
                       Country == "Uae" ~ "UAE",
                       Country == "Brasil" ~ "Brazil",
                       Country == "España" ~ "Spain",
                       (Country == "Korea") & (State_or_Province == "Incheon") ~ "South Korea",
                       
                       #Where Country column contains a number but State/ Province indicates the country is the US
                       Country == "32" ~ "US",
                       Country == "45" ~ "US",
                       Country == "46" ~ "US",
                       Country == "35" ~ "US",
                       Country == "30.0" ~ "US",
                       Country == "45.0" ~ "US",
                       Country == "44.0" ~ "US",
                       Country == "54.0" ~ "US",
                       Country == "47.0" ~ "US",
                       Country == "51.0" ~ "US",
                       
                       #cleaning data for Unknown Country entries 
                       Country %in% c("1",
                                      "A",
                                      "A Tropical Island South Of The Equator",
                                      "A tropical island south of the equator",
                                      "Atlantis",
                                      "EUA",
                                      "UD",
                                      "Earth",
                                      "Europe",
                                      "Fear And Loathing",
                                      "Fear and Loathing",
                                      "God's Country",
                                      "god's country",
                                      "Insanity Lately",
                                      "I Don't Know Anymore",
                                      "Neverland",
                                      "Not The Usa Or Canada",
                                      "Not the USA or Canada",
                                      "One Of The Best Ones",
                                      "one of the best ones",
                                      "See Above",
                                      "See above",
                                      "see above",
                                      "Somewhere",
                                      "Soviet Canuckistan",
                                      "soviet canuckistan",
                                      "Subscribe To Dm4uz3 On Youtube",
                                      "subscribe to dm4uz3 on youtube",
                                      "Sub-Canadian North America... 'Merica",
                                      "The Republic Of Cascadia",
                                      "The republic of Cascadia",
                                      "There Isn't One For Old Men",
                                      "there isn't one for old men",
                                      "I don't know anymore",
                                      "This One",
                                      "this one",
                                      "Denial") ~ "Unknown Country",
                       (Country == "Cascadia") & (State_or_Province == "cascadia") ~ "Unknown Country",
                       (is.na(Country)) & (is.na(State_or_Province)) ~ "Unknown Country",
                       (is.na(Country)) & (State_or_Province == "48") ~ "Unknown Country",
                       (Country == "Murica") & (State_or_Province == "Gawja") ~ "Unknown Country",
                       TRUE ~ Country),
          (Candy_Type =
                       # Invalid responses
                       case_when(Candy_Type %in% c(
                         "abstained from m ming",
                         "bonkers the board game",
                         "box o raisins",
                         "boxo raisins",
                         "broken glow stick",
                         "candy that is clearly just the stuff given out for free at restaurants",
                         "cash or other forms of legal tender",
                         "chardonnay",
                         "creepy religious comics chick tracts",
                         "dental paraphenalia",
                         "generic brand acetaminophen",
                         "healthy fruit",
                         "hugs actual physical hugs",
                         "joy joy mit iodine",
                         "kale smoothie",
                         "lapel pins",
                         "minibags of chips",
                         "person of interest season 3 dvd box set not including disc 4 with hilarious outtakes",
                         "peterson brand sidewalk chalk",
                         "real housewives of orange county season 9 blue ray",
                         "spotted dick",
                         "vials of pure high fructose corn syrup for main lining into your vein",
                         "vicodin",
                         "whatchamacallit bars",
                         "white bread",
                         "whole wheat anything") ~ "invalid_response",
                         # Rename candy_type
                         Candy_Type == "anonymous brown globs that come in black and orange wrappers" ~ "Mary Janes",
                         Candy_Type == "anonymous brown globs that come in black and orange wrappers a k a mary janes" ~ "Mary Janes",
                         Candy_Type == "chick o sticks we don t know what that is" ~ "Chick O Stick",
                         Candy_Type == "gummy bears straight up" ~ "Gummy Bears",
                         Candy_Type == "licorice yes black" ~ "Black licorice",
                         Candy_Type == "nown laters" ~ "Now and later",
                         Candy_Type == "sandwich sized bags filled with boo berry crunch" ~ "Boo Berry crunch",
                         Candy_Type == "sourpatch kids i e abominations of nature" ~ "Sourpatch Kids",
                         Candy_Type == "sweetums a friend to diabetes" ~ "Sweetums",
                         Candy_Type == "those odd marshmallow circus peanut things" ~ "Marshmallow Circus Peanuts",
                         Candy_Type == "tolberone something or other" ~ "Toblerone",
                         Candy_Type == "x100 grand bar" ~ "100 Grand Bar",
                         TRUE ~ Candy_Type)
              ),
    Candy_Type = str_replace_all(Candy_Type, clean_candy_column, ""),
    Candy_Type = str_replace_all(Candy_Type, "_", " ")
    );

# review list of distinct candy types in tidied dataset

distinct_candy_types <- tidied_candy_data_171819 %>% 
  distinct(Candy_Type)

# review list of distinct country names in tidied dataset

distinct_country_names <- tidied_candy_data_171819 %>% 
  distinct(Country)

write.csv(tidied_candy_data_171819, here("clean_data/tidied_candy_data_171819.csv"))
