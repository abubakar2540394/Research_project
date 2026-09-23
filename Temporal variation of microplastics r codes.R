### Temporal Variation of microplastic in Antarctic Terebellid worms over a decade
## Abu Bakar Siddik


# Install required packages
require(readxl)
require(dplyr)
require(FSA)
require(ggplot2)

# Descriptive Statistics

# setwd("E:/Abu Project")
df <- read_excel("project_dataset.xlsx") %>%
  mutate(Year = as.character(Year))

descriptive_table <- df %>%
  group_by(Year) %>%
  summarise(
    n = n(),
    
    `Weight (g) with SD` =
      paste0(
        sprintf("%.2f", mean(`Weight (g)`, na.rm = TRUE)),
        " ± ",
        sprintf("%.2f", sd(`Weight (g)`, na.rm = TRUE))
      ),
    
    `Width of 2nd Segment (mm) with SD` =
      paste0(
        sprintf("%.2f", mean(`Width of 2nd Segment (mm)`, na.rm = TRUE)),
        " ± ",
        sprintf("%.2f", sd(`Width of 2nd Segment (mm)`, na.rm = TRUE))
      ),
    
    `Width of 6th Segment (mm) with SD` =
      paste0(
        sprintf("%.2f", mean(`Width of 6 th Segment (mm)`, na.rm = TRUE)),
        " ± ",
        sprintf("%.2f", sd(`Width of 6 th Segment (mm)`, na.rm = TRUE))
      ),
    
    `Width of 10th Segment (mm) with SD` =
      paste0(
        sprintf("%.2f", mean(`Width of 10th segment (mm)`, na.rm = TRUE)),
        " ± ",
        sprintf("%.2f", sd(`Width of 10th segment (mm)`, na.rm = TRUE))
      ),
    
    `Net fibre count with SD` =
      paste0(
        sprintf("%.2f", mean(netfibre, na.rm = TRUE)),
        " ± ",
        sprintf("%.2f", sd(netfibre, na.rm = TRUE))
      ),
    
    .groups = "drop"
  )

#overall stat add in table
overall <- df %>%
  summarise(
    Year = "Overall",
    n = n(),
    
    `Weight (g) with SD` =
      paste0(
        sprintf("%.2f", mean(`Weight (g)`, na.rm = TRUE)),
        " ± ",
        sprintf("%.2f", sd(`Weight (g)`, na.rm = TRUE))
      ),
    
    `Width of 2nd Segment (mm) with SD` =
      paste0(
        sprintf("%.2f", mean(`Width of 2nd Segment (mm)`, na.rm = TRUE)),
        " ± ",
        sprintf("%.2f", sd(`Width of 2nd Segment (mm)`, na.rm = TRUE))
      ),
    
    `Width of 6th Segment (mm) with SD` =
      paste0(
        sprintf("%.2f", mean(`Width of 6 th Segment (mm)`, na.rm = TRUE)),
        " ± ",
        sprintf("%.2f", sd(`Width of 6 th Segment (mm)`, na.rm = TRUE))
      ),
    
    `Width of 10th Segment (mm) with SD` =
      paste0(
        sprintf("%.2f", mean(`Width of 10th segment (mm)`, na.rm = TRUE)),
        " ± ",
        sprintf("%.2f", sd(`Width of 10th segment (mm)`, na.rm = TRUE))
      ),
    
    `Net fibre count with SD` =
      paste0(
        sprintf("%.2f", mean(netfibre, na.rm = TRUE)),
        " ± ",
        sprintf("%.2f", sd(netfibre, na.rm = TRUE))
      )
  )


final_table <- bind_rows(
  descriptive_table,
  overall
)


print(final_table)



write.csv(
  final_table,
  "Yearwise_Descriptive_Statistics.csv",
  row.names = FALSE
)

# Positive control scatter plot
# Read data
df <- read_excel("project_dataset.xlsx")



df_clean <- df %>%
  filter(
    !is.na(`retained plastic (%)`),
    !is.na(addedplastic)
  )

p <- ggplot(
  df_clean,
  aes(
    x = addedplastic,
    y = `retained plastic (%)`
  )
) +
  
  geom_point(
    size = 3,
    shape = 21,
    fill = "white",
    color = "black",
    stroke = 0.8
  ) +
  
  labs(
    x = "Added Plastic Beads (g)",
    y = "Retained Plastic Beads (%)"
  ) +
  scale_x_continuous(
    breaks = c(0.02, 0.1, 0.255),
    labels = c("0.02", "0.1", "0.255")
  ) +
  scale_y_continuous(
    limits = c(65, 100),
    breaks = seq(60, 100, by = 5),
    expand = c(0, 0)
  ) +
  
  theme_classic(base_size = 14) +
  
  theme(
    axis.line = element_line(linewidth = 0.8),
    axis.ticks = element_line(linewidth = 0.7),
    axis.ticks.length = unit(-0.2, "cm"),
    
    axis.text.x = element_text(
      color = "black",
      margin = margin(t = 8)
    ),
    
    axis.text.y = element_text(
      color = "black",
      margin = margin(r = 8)
    ),
    
    axis.title = element_text(
      color = "black"
    )
  )

print(p)


ggsave(
  "Positive_Control_vs_Added_Fibre.eps",
  plot = p,
  device = cairo_ps,
  width = 7,
  height = 5,
  units = "in",
  dpi = 600
)

# PNG
ggsave(
  "Positive_Control_vs_Added_Fibre.png",
  plot = p,
  width = 7,
  height = 5,
  units = "in",
  dpi = 800
)

# Kruskal-Wallis Test
# Response: NetFibre
# Factor: Year


df <- read_excel("project_dataset.xlsx")


df_clean <- df %>%
  filter(
    !is.na(netfibre),
    !is.na(Year)
  ) %>%
  mutate(
    Year = factor(Year)
  )


summary_table <- df_clean %>%
  group_by(Year) %>%
  summarise(
    n = n(),
    Mean = mean(netfibre),
    Median = median(netfibre),
    SD = sd(netfibre),
    Min = min(netfibre),
    Max = max(netfibre),
    .groups = "drop"
  )

print(summary_table)


p <- ggplot(df_clean,
            aes(x = Year,
                y = netfibre,
                fill = Year)) +
  
  geom_boxplot(
    width = 0.6,
    color = "black",
    linewidth = 0.7,
    outlier.shape = NA
  ) +

  
  scale_fill_manual(values = c(
    "#FCC1BD",
    "#8CE0A5",
    "#B7D2FF"
  )) +
  
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  labs(
    x = "Year",
    y = "Net Fibre Count"
  ) +
  
  theme_classic(base_size = 14) +
  
  theme(
    legend.position = "none",
    axis.line = element_line(linewidth = 0.8),
    axis.ticks = element_line(linewidth = 0.6),
    axis.ticks.length = unit(-0.2, "cm"),
    axis.text = element_text(color = "black")
  )

print(p)


ggsave(
  "NetFibre_by_Year.eps",
  plot = p,
  device = cairo_ps,
  width = 7,
  height = 5,
  units = "in",
  dpi = 600
)


kw_result <- kruskal.test(netfibre ~ Year, data = df_clean)

print(kw_result)


dunn_result <- dunnTest(
  netfibre ~ Year,
  data = df_clean,
  method = "holm"
)

print(dunn_result)



capture.output({
  
  cat("=====================================\n")
  cat("Summary Statistics\n")
  cat("=====================================\n\n")
  print(summary_table)
  
  cat("\n=====================================\n")
  cat("Kruskal-Wallis Test\n")
  cat("=====================================\n\n")
  print(kw_result)
  
  cat("\n=====================================\n")
  cat("Dunn's Post Hoc Test (Holm)\n")
  cat("=====================================\n\n")
  print(dunn_result)
  
}, file = "Kruskal_Wallis_NetFibre_Year.txt")

# Spearman correlation 
df <- read_excel("project_dataset.xlsx")



df_clean <- df %>%
  filter(
    !is.na(`Weight (g)`),
    !is.na(netfibre)
  )

spearman_result <- cor.test(
  df_clean$`Weight (g)`,
  df_clean$netfibre,
  method = "spearman",
  exact = FALSE
)

rho <- unname(spearman_result$estimate)
p_value <- spearman_result$p.value

# \u03c1: unicode of rho
annotation_text <- paste0(
  "Spearman's \u03c1 = ", round(rho, 3),
  "\np = ", round(p_value, 4)
)

p <- ggplot(
  df_clean,
  aes(
    x = `Weight (g)`,
    y = netfibre
  )
) +
  
  
  geom_point(
    size = 3,
    shape = 21,
    fill = "white",
    color = "black",
    stroke = 0.8
  ) +
  
  # LOESS trend for non linear trend
  geom_smooth(
    method = "loess",
    se = TRUE,
    linewidth = 1
  ) +
  
  # Spearman stat
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = annotation_text,
    hjust = 1.1,
    vjust = 1.5,
    size = 5
  ) +
  
  labs(
    x = "Weight (g)",
    y = "Net Fibre Count"
  ) +
  
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  theme_classic(base_size = 14) +
  
  theme(
    axis.line = element_line(linewidth = 0.8),
    axis.ticks = element_line(linewidth = 0.7),
    axis.ticks.length = unit(-0.2, "cm"),
    
    axis.text.x = element_text(
      color = "black",
      margin = margin(t = 8)
    ),
    
    axis.text.y = element_text(
      color = "black",
      margin = margin(r = 8)
    ),
    
    axis.title = element_text(
      color = "black"
    )
  )

print(p)


ggsave(
  "Spearman_netfibre_vs_Weight.eps",
  plot = p,
  device = cairo_ps,
  width = 7,
  height = 5,
  units = "in",
  dpi = 600
)



ggsave(
  "Spearman_netfibre_vs_Weight.png",
  plot = p,
  width = 7,
  height = 5,
  units = "in",
  dpi = 800
)

