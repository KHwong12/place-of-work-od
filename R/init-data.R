# Load and initialise dataset ready to plot

library(tidyverse)

DISTRICT_ORDER <- c("Central and Western", "Wan Chai", "Eastern", "Southern",
                    "Yau Tsim Mong", "Sham Shui Po", "Kowloon City", "Wong Tai Sin", "Kwun Tong",
                    "Kwai Tsing", "Tsuen Wan", "Tuen Mun", "Yuen Long",
                    "North", "Tai Po", "Sha Tin", "Sai Kung", "Islands")

WORKPLACE_ORDER <- c("Central and Western", "Wan Chai", "Eastern", "Southern",
                     "Yau Tsim Mong", "Sham Shui Po", "Kowloon City", "Wong Tai Sin", "Kwun Tong",
                     "Kwai Chung New Town", "Tsuen Wan New Town", "Tsing Yi New Town", "Tuen Mun New Town", "Tin Shui Wai New Town", "Yuen Long New Town",
                     "Fanling/ Sheung Shui New Town", "Tai Po New Town", "Sha Tin New Town", "Ma On Shan New Town", "Tseung Kwan O New Town", "North Lantau New Town",
                     "Other areas in the New Territories", "No fixed places of work in Hong Kong/ Marine/ Work at home/ Places outside Hong Kong")

DISTRICT_ORDER_CHI <- c("中西區", "灣仔", "東區", "南區",
                        "油尖旺", "深水埗", "九龍城", "黃大仙", "觀塘",
                        "葵青", "荃灣", "屯門", "元朗",
                        "北區", "大埔", "沙田", "西貢", "離島")

WORKPLACE_ORDER_CHI <- c("中西區", "灣仔", "東區", "南區",
                         "油尖旺", "深水埗", "九龍城", "黃大仙", "觀塘",
                         "葵涌新市鎮", "荃灣新市鎮", "青衣新市鎮", "屯門新市鎮", "天水圍新市鎮", "元朗新市鎮",
                         "粉嶺／上水新市鎮", "大埔新市鎮", "沙田新市鎮", "馬鞍山新市鎮", "將軍澳新市鎮", "北大嶼山新市鎮",
                         "新界其他地區", "在香港沒有固定工作地點／水上／於家中工作／香港以外地方")

WORKPLACE_ORDER_SIM = c("Central and Western", "Wan Chai", "Eastern", "Southern",
                        "Yau Tsim Mong", "Sham Shui Po", "Kowloon City", "Wong Tai Sin", "Kwun Tong",
                        "Kwai Chung NT", "Tsuen Wan NT", "Tsing Yi NT", "Tuen Mun NT", "Tin Shui Wai NT", "Yuen Long NT",
                        "Fanling/ Sheung Shui NT", "Tai Po NT", "Sha Tin NT", "Ma On Shan NT", "Tseung Kwan O NT", "North Lantau NT",
                        "Others")



dataset <- read_csv("data/data-output/cross_travel_type_rmspecial.csv")

nodes_OD <- read_csv("data/data-output/nodes_OD.csv")
regions_list <- read_csv("data/data-output/regions_list.csv")


origin_df <- regions_list %>% subset(node_ID <= 17)
destination_df <- regions_list %>% subset(node_ID > 17)

# get name list from df
# https://stackoverflow.com/questions/46238448/make-a-named-list-from-two-columns-with-multiple-values-per-name
origin_choice_list <- split(origin_df$node_ID, origin_df$node_name)

# reorder by the node_ID
# https://stackoverflow.com/questions/30651365/sorting-a-key-value-list-in-r-by-value
origin_choice_list <- origin_choice_list[order(unlist(origin_choice_list))]

destination_choice_list <- split(destination_df$node_ID, destination_df$node_name)
destination_choice_list <- destination_choice_list[order(unlist(destination_choice_list))]


origin_choice_list_tc <- split(origin_df$node_ID, origin_df$node_name_tc)
origin_choice_list_tc <- origin_choice_list_tc[order(unlist(origin_choice_list_tc))]
destination_choice_list_tc <- split(destination_df$node_ID, destination_df$node_name_tc)
destination_choice_list_tc <- destination_choice_list_tc[order(unlist(destination_choice_list_tc))]


dataset_simlabel <- dataset %>% 
  mutate(
    place_of_residence = factor(place_of_residence, DISTRICT_ORDER),
    place_of_work = factor(place_of_work, WORKPLACE_ORDER),
    place_of_residence_chi = factor(place_of_residence_chi, DISTRICT_ORDER_CHI),
    place_of_work_chi = factor(place_of_work_chi, WORKPLACE_ORDER_CHI)
  ) %>%
  # simplified label for heatmap
  mutate(
    place_of_work_sim = ifelse(place_of_work == "Other areas in the New Territories", "Others", str_replace(place_of_work, "New Town", "NT"))
  ) %>%
  mutate(
    place_of_work_sim = factor(place_of_work_sim, WORKPLACE_ORDER_SIM)
  ) %>%
  # add colour code for links
  mutate(
    link_colour = case_when(
      residence_area == "HKI" ~ "#E41A1C33",
      residence_area == "KL" ~ "#4DAF4A33",
      residence_area == "NTW" ~ "#FF7F0033",
      residence_area == "NTE" ~ "#984EA333",
      residence_area == "I" ~ "#377EB833"
    ),
    journey_type_colour = case_when(
      journey_type_mostsim == "Same Area" ~ "#4daf4a66",
      journey_type_mostsim == "HKI/KL" ~ "#377eb866",
      journey_type_mostsim == "HKI/NT" ~ "#ff7f0066",
      journey_type_mostsim == "KL/NT" ~ "#984ea366"
    )
  )

# may consider remove unused columns to improve drawing speed
work_OD_plot_reorder_plotly <- dataset_simlabel