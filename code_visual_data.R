
                      # University Performance by Region (Radar Charts)

data_asia <- data.frame(
  row.names = c("University of Gadjah Mada", "University of Sabanci", "University of Khon Kaen", "University of Indonesia", "University of Marmara", 
                "University of Uskudar", "University of Sarakrya", "University of NTU", "University of Arab Open", "University of Brawijaya"),
  `SI` = c(1450,1375,1325,1250,1250,1250,1225,1225,1225,1200),
  `EC` = c(1700,1725,1400,1575,1560,985,1575,1335,1950,1750),
  `WS` = c(1425,1350,1425,1125,1425,750,1425,1800,1350,1350),
  `WR` = c(800,850,900,700,750,500,800,900,750,800)
)


# EUROPE
data_europe <- data.frame(
  row.names = c("University of Oxford", "University of Kazan", "University of Budapest", "University of WarsawTech", "University of Barcelona",
                "University of Warsaw", "University of Bogazici", "University of Bartin", "University of Koc", "University of Udmurt"),
  `SI` = c(1375,1175,1125,1100,1050,1040,995,975,925,885),
  `EC` = c(1825,1775,1000,1650,1900,760,1060,1050,725,1025),
  `WS` = c(1800,1800,1275,1650,1575,1275,675,1200,900,1575),
  `WR` = c(1000,900,410,460,950,450,500,700,120,410)
)

# AMERICA
data_america <- data.frame(
  row.names = c("University of Harvard", "University of Brasilia", "University of Ecuador", "University of Azteca", "University of Charles", 
                "University of Noreste", "University of Colombia", "University of Guanajuato", "University of RioJeneiro", "University of CasaBlanca"),
  `SI` = c(725,1325,1275,1150,1125,1100,1050,1005,1000,975),
  `EC` = c(885,1725,1700,1925,1700,1675,1700,1825,1115,1800),
  `WS` = c(1050,1650,1350,1800,1275,1425,1425,1200,750,1500),
  `WR` = c(500,850,700,900,950,900,950,750,500,900)
)

# AFRICA
data_africa <- data.frame(
  row.names = c("University of Morocco", "University of Alexandria", "University of Addis Ababa", "University of Angola", "University of Al Azhar", "University of Moumouni"),
  `SI` = c(1030, 950, 855, 720, 495, 365),
  `EC` = c(1475, 1375, 810, 420, 960, 460),
  `WS` = c(975, 600, 450, 0, 600, 150),
  `WR` = c(450, 750, 450, 410, 200, 20)
)

# ------------------ PLOT ------------------

# Panel 2x2
par(mfrow=c(2,2), mar=c(1,2,2,1))
make_radar(data_asia, "Asia")
make_radar(data_europe, "Europe")
make_radar(data_america, "America")
make_radar(data_africa, "Africa")


                    #Mapping University Competitive Positions



library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(ggrepel)


data <- data_univ %>%
  mutate(
    Competitive_Edge = (`Score EC` + `Score WS`) / 2,  # Daya saing
    Academic_Strength = (`Score TR` + `Score ED`) / 2   # Kekuatan akademik
  )

data <- data %>%
  mutate(
    Competitive_Edge_Scaled = Competitive_Edge / 100,
    Academic_Strength_Scaled = Academic_Strength / 100
  )

ggplot(data = data, aes(x = Competitive_Edge_Scaled, y = Academic_Strength_Scaled)) +
  # Menggunakan Competitive_Edge_Scaled dan Academic_Strength_Scaled
  geom_point(aes(color = Region), size = 1.5, alpha = 0.7) +
  
  geom_text_repel(aes(label = `University Name`), size = 3) +
  
  labs(title = "Competitive Positioning of Universities",
       x = "Competitive Edge (Employability & Reputation)",
       y = "Academic Strength (Teaching & Education)") +
  
  
  scale_color_discrete(name = NULL) +
  
  scale_x_continuous(breaks = c(5, 7, 10, 12, 15, 18), # Sesuaikan dengan rentang data setelah dibagi 100
                     labels = c("5", "7", "10", "12", "15", "18")) + # Sesuai dengan breaks
  
  
  scale_y_continuous(breaks = c(5, 7, 10, 12, 15), # Sesuaikan dengan rentang data setelah dibagi 100
                     labels = c("5", "7", "10", "12", "15")) +
  
  theme_minimal()


                    # Social Impact vs. Global Ranking (Top 50 Universities)

library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats) # Untuk fct_reorder

long_data_universities <- data_univ %>%
  select(`University Name`, `Score SI`, `Score WR`)

long_data_universities <- long_data_universities %>%
  rowwise() %>%
  mutate(Avg_Overall_Score = mean(c(`Score SI`, `Score WR`), na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(Avg_Overall_Score)) # Tetap urutkan descending di sini agar head() memilih yang teratas

top_50_universities <- long_data_universities %>%
  head(50)

long_data_top_50 <- top_50_universities %>%
  pivot_longer(
    cols = c(`Score SI`, `Score WR`),
    names_to = "Indicator",
    values_to = "Score"
  )

long_data_top_50$Indicator <- factor(long_data_top_50$Indicator,
                                     levels = c("Score SI", "Score WR"),
                                     labels = c("Score SI", "Score WR"))

long_data_top_50$`University Name` <- factor(long_data_top_50$`University Name`,
                                             levels = rev(unique(top_50_universities$`University Name`)))

p_top_50_reversed <- ggplot(long_data_top_50, aes(x = Indicator, y = `University Name`, fill = Score)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Score") +
  geom_text(aes(label = round(Score, 0)), color = "black", size = 0) +
  labs(
    title = "SI and WR Scores for Top 50 Universities (Reverse Order)", 
    x = "Indicator",
    y = "Universities"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )

print(p_top_50_reversed)

