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

  #Country data - changing to title case
  tidied_candy_data_171819 <- merged_candy_data_171819  %>% 
  mutate(Country_clean = str_to_title(Country),
         .after = Country) %>% 
  
  #cleaning data for US entries
  mutate(Country_clean = 
           case_when(Country_clean %in% c("'Merica",
                                        "Ahem....Amerca",
                                        "Alaska",
                                        "America",
                                        "California",
                                        "Eua",
                                        "I Pretend To Be From Canada, But I Am Really From The United States.",
                                        "Merica",
                                        "Murrika",
                                        "N. America",
                                        "Narnia",
                                        "New Jersey",
                                        "New York",
                                        "North Carolina",
                                        "Pittsburgh",
                                        "The Best One - Usa",
                                        "The United States Of America",
                                        "The United States",
                                        "The Yoo Ess Of Aaayyyyyy",
                                        "Trumpistan",
                                        "U S A",
                                        "U S",
                                        "U.s.",
                                        "U.s.a.",
                                        "Ud",
                                        "Unhinged States",
                                        "Unied States",
                                        "Unite States",
                                        "United  States Of America",
                                        "United Sates",
                                        "United Staes",
                                        "United State",
                                        "United Statea",
                                        "United Stated",
                                        "united states",
                                        "United States Of America",
                                        "United States",
                                        "United Statss",
                                        "United Stetes",
                                        "United Ststes",
                                        "Unites States",
                                        "Units States",
                                        "Us Of A",
                                        "Us",
                                        "Usa (I Think But It's An Election Year So Who Can Really Tell)",
                                        "Usa Usa Usa Usa",
                                        "Usa Usa Usa!!!!",
                                        "Usa Usa Usa",
                                        "Usa! Usa! Usa!", 
                                        "Usa! Usa!", 
                                        "Usa!!!!!!",
                                        "Usa!",
                                        "Usa",
                                        "uSA",
                                        "usa",
                                        "Usa? Hard To Tell Anymore..",
                                        "Usaa",
                                        "Usas",
                                        "Usausausa",
                                        "USSA",
                                        "Ussa",
                                        "united states") ~ "US",
                     (Country_clean == "Cascadia") & (State_or_Province == "WA") ~ "US",
                     (Country_clean == "Murica") & (State_or_Province == "Oregon") ~ "US",
                     (Country_clean == "Murica") & (State_or_Province == "California") ~ "US",
                     (Country_clean == "Murica") & (State_or_Province == "NJ") ~ "US",
                     (is.na(Country_clean)) & (State_or_Province == "CA") ~ "US",
                     (is.na(Country_clean)) & (State_or_Province == "WA") ~ "US",
                     (is.na(Country_clean)) & (State_or_Province == "PA") ~ "US",
                     (is.na(Country_clean)) & (State_or_Province == "Massachusetts") ~ "US",
                     (is.na(Country_clean)) & (State_or_Province == "Arizona") ~ "US",
                     (is.na(Country_clean)) & (State_or_Province == "Illinois") ~ "US",
                     (is.na(Country_clean)) & (State_or_Province == "United States") ~ "US",
                     (is.na(Country_clean)) & (State_or_Province == "USA") ~ "US",
                     
                     #cleaning data for UK entries                     
                     Country_clean %in% c("United Kindom",
                                        "England",
                                        "Endland",
                                        "Scotland",
                                        "U.k.",
                                        "Uk",
                                        "United Kingdom") ~ "UK",
                     
                     #cleaning data for Canada entries  
                     Country_clean %in% c("Can",
                                        "Canada`",
                                        "Canae") ~ "Canada",
                     
                     #general cleaning
                     Country_clean == "Netherlands" ~ "The Netherlands",
                     Country_clean == "Uae" ~ "UAE",
                     Country_clean == "Brasil" ~ "Brazil",
                     Country_clean == "España" ~ "Spain",
                     (Country_clean == "Korea") & (State_or_Province == "Incheon") ~ "South Korea",
                     
                     #Where Country column contains a number but State/ Province indicates the country is the US
                     Country_clean == "32" ~ "US",
                     Country_clean == "45" ~ "US",
                     Country_clean == "46" ~ "US",
                     Country_clean == "35" ~ "US",
                     Country_clean == "30.0" ~ "US",
                     Country_clean == "45.0" ~ "US",
                     Country_clean == "44.0" ~ "US",
                     Country_clean == "54.0" ~ "US",
                     Country_clean == "47.0" ~ "US",
                     Country_clean == "51.0" ~ "US",
                     
                     #cleaning data for Unknown Country entries 
                     Country_clean %in% c("1",
                                        "A",
                                        "A Tropical Island South Of The Equator",
                                        "Atlantis",
                                        "Earth",
                                        "Europe",
                                        "Fear And Loathing",
                                        "God's Country",
                                        "Insanity Lately",
                                        "I Don't Know Anymore",
                                        "Neverland",
                                        "Not The Usa Or Canada",
                                        "One Of The Best Ones",
                                        "See Above",
                                        "Somewhere",
                                        "Soviet Canuckistan",
                                        "Subscribe To Dm4uz3 On Youtube",
                                        "Sub-Canadian North America... 'Merica",
                                        "The Republic Of Cascadia",
                                        "There Isn't One For Old Men",
                                        "This One",
                                        "Denial") ~ "Unknown Country",
                     (Country_clean == "Cascadia") & (State_or_Province == "cascadia") ~ "Unknown Country",
                     (is.na(Country_clean)) & (is.na(State_or_Province)) ~ "Unknown Country",
                     (is.na(Country_clean)) & (State_or_Province == "48") ~ "Unknown Country",
                     (Country_clean == "Murica") & (State_or_Province == "Gawja") ~ "Unknown Country",
                     TRUE ~ Country_clean
           )
  )
  
  
  #cleaning data for candy type 
  
  tidied_candy_data_171819 <- tidied_candy_data_171819 %>% 
    mutate(Candy_clean = str_replace_all(Candy_Type,"^q6_","")) %>% 
    mutate(Candy_clean = str_replace_all(Candy_clean, "_", " "))
  
  tidied_candy_data_171819 <- tidied_candy_data_171819  %>% 
  mutate(Candy_clean =
                       # Invalid responses
                       case_when(Candy_clean %in% c(
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
                         
                         # Rename 
                         Candy_clean == "anonymous brown globs that come in black and orange wrappers" ~ "mary janes",
                         Candy_clean == "anonymous brown globs that come in black and orange wrappers a k a mary janes" ~ "mary janes",
                         Candy_clean == "chick o sticks we don t know what that is" ~ "chick o stick",
                         Candy_clean == "gummy bears straight up" ~ "gummy bears",
                         Candy_clean == "licorice yes black" ~ "black licorice",
                         Candy_clean == "nown laters" ~ "now and later",
                         Candy_clean == "sandwich sized bags filled with boo berry crunch" ~ "boo berry crunch",
                         Candy_clean == "sourpatch kids i e abominations of nature" ~ "sourpatch kids",
                         Candy_clean == "sweetums a friend to diabetes" ~ "sweetums",
                         Candy_clean == "those odd marshmallow circus peanut things" ~ "marshmallow circus peanuts",
                         Candy_clean == "tolberone something or other" ~ "toblerone",
                         Candy_clean == "x100 grand bar" ~ "100 grand bar",
                         TRUE ~ Candy_clean)
              )
  
  #cleaning data for age
  tidied_candy_data_171819 <- tidied_candy_data_171819 %>% 
    mutate(Age_clean = str_replace_all(Age, "\\.0$","")) %>% 
    
    #cleaning data for age unknown
    mutate(Age_clean = 
             case_when(Age_clean %in% c("your mom",
                                        "x",
                                        "very",
                                        "Too old to trick or treat without it being creepy",
                                        "too old for this",
                                        "Too old",
                                        "too old",
                                        "too",
                                        "So old",
                                        "See question 2",
                                        "Same as yo mama",
                                        "really old",
                                        "over retirement age",
                                        "Over 50",
                                        "over 40",
                                        "Older than you",
                                        "older than I want to be",
                                        "Older than i act",
                                        "older than dirt",
                                        "Old, very old",
                                        "old enough to party",
                                        "Old enough to not Trick or Treat.",
                                        "Old enough to know better",
                                        "old enough to know better",
                                        "Old enough",
                                        "old enough",
                                        "old but still know joy from despair",
                                        "OLD",
                                        "Old",
                                        "old",
                                        "Not tell",
                                        "Not as old as you...",
                                        "none of your business",
                                        "no",
                                        "Nevermind",
                                        "Never ask a woman that question.",
                                        "MY NAME JEFF",
                                        "middle-aged",
                                        "Many",
                                        "In dog years?",
                                        "I remember the Nixon administration",
                                        "I can remember when Java was a cool new language",
                                        "Hahahahahaha",
                                        "hahahahaha",
                                        "gofuckyourself",
                                        "Enough",
                                        "enough",
                                        "dadt",
                                        "blah",
                                        "As old as my tongue a few years older than my teeth",
                                        "Ancient",
                                        "ancient",
                                        "Adult",
                                        "--",
                                        "45-55",
                                        "50+",
                                        "55+",
                                        "60+",
                                        "65+",
                                        ">39",
                                        "?",
                                        "７１＋",
                                        "0x2A",
                                        "24-50",
                                        "30's",
                                        "30s",
                                        "30+",
                                        "40s",
                                        "50ish",
                                        "40something",
                                        "5 months",
                                        "1.0E18",
                                        "9.0E22",
                                        "0.62",
                                        "a million") ~ "unknown age",
                       (is.na(Age_clean))~ "unknown age",
                       
    #general cleaning
                       Age_clean == "sixty-nine" ~ "69",
                       Age_clean == "Good Lord!  I'm 43!" ~ "43",
                       Age_clean == "Fifty.  Nine.  Ish." ~ "59",
                       Age_clean == "27^." ~ "27",
                       Age_clean == "37 (I'm taking a child)" ~ "37",
                       Age_clean == "40. Deal with it." ~ "40",
                       Age_clean == "42 - I'm taking my kid" ~ "42",
                       Age_clean == "46 Halloweens." ~ "46",
                       Age_clean == "49 11/12ths" ~ "49",
                       Age_clean == "50 (despair)" ~ "50",
                       Age_clean == "50, taking a 13 year old." ~ "50",
                       Age_clean == "70.5" ~ "70",
                       Age_clean == "44.4444" ~ "44",
                       Age_clean == "39.4" ~ "39",
                       Age_clean == "23.2" ~ "23",
                       Age_clean == "18.75" ~ "18",
                       Age_clean == "18.17" ~ "18",
                       Age_clean == "45, but the 8-year-old Huntress and bediapered Unicorn give me political cover and social respectability.  However, I WILL eat more than they do combined." ~ "45",
                       Age_clean == "59 on the day after Halloween" ~ "58",
                       Age_clean == "27^" ~ "27",
                       TRUE ~ Age_clean
             )
    ) %>% 
    
    mutate(Age_clean = str_replace_all(Age_clean, "[:punct:]$","")) %>% 
    mutate(Age_clean = str_replace_all(Age_clean, "^$","")) %>% 
    mutate(Age_clean = str_replace_all(Age_clean, "[s-z]$","")) %>% 
    
    mutate(Age_clean = as.numeric(Age_clean))
  
  
# review list of distinct candy types in tidied dataset
distinct_candy_clean <- tidied_candy_data_171819 %>% 
  distinct(Candy_clean)

# review list of distinct country names in tidied dataset
distinct_country_clean <- tidied_candy_data_171819 %>% 
  distinct(Country_clean)

# review list of distinct ages in tidied dataset
distinct_age_clean <- tidied_candy_data_171819 %>% 
  distinct(Age_clean)

#saving the data into the clean_data folder

write.csv(tidied_candy_data_171819, here("clean_data/tidied_candy_data_171819.csv"))



