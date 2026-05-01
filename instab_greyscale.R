############################################################################################
############################################################################################
####Project: (In)Stability of Risk and Time Preference			                          	####			
####Purpose: Descriptives                                                              	####
####By: Sabine																	                                        ####
####Date: May 01, 2026   														                            	    ####          														
############################################################################################
############################################################################################
  
  
#To do: 
#Here I follow the EE publishing guidelines: https://www.e-elgar.com/author-hub/as-you-write-your-book-or-chapter/#accordion-44
  
rm(list = ls())

#install.packages("ggpattern")

#load packages
library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(ggpattern)



setwd("O:/Data-Work/25_Agricultural_Economics-RE/251_Agrarsoziologie/Sabine/(In)Stability_RPTP")

#Load data
instab <- read_xlsx("instabRPTPdataForR.xlsx")

################################################################################
#Consistent theme across all figures for the book chapter
################################################################################
theme_book <- theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

geom_book <- function() {
  geom_col(color = "black", size = 0.2)
}

#Domain
scale_fill_domain <- scale_fill_manual(values = c(
  "Both" = "grey30",
  "Risk" = "grey60",
  "Time" = "grey90"
))

#Stability conclusions
scale_fill_stability <- scale_fill_manual(values = c(
  "Stable" = "grey30",
  "Mixed" = "grey60",
  "Unstable" = "grey90"
))

#Measurement method
scale_fill_measurement <- scale_fill_manual(values = c(
  "Self-Assessment" = "grey90",
  "Experiment" = "grey60",
  "Both" = "grey30"
))

#Study design
scale_fill_design <- scale_fill_manual(values = c(
  "Cross-section" = "grey90",
  "Repeated cross-section" = "grey60",
  "Panel" = "grey30"
))

#Event study
scale_fill_event <- scale_fill_manual(values = c(
  "No" = "grey90",
  "Yes" = "grey30"
))


#Event types
scale_fill_event_type <- scale_fill_manual(
  breaks = c("None",
             "Economic distress",
             "Natural disaster",
             "Climatic event",
             "Social conflict",
             "Life-cycle event",
             "Mixed"),
  values = c(
    "None" = "white",              # very light
    "Economic distress" = "grey10",
    "Natural disaster" = "grey30",
    "Climatic event" = "grey50",
    "Social conflict" = "grey70",
    "Life-cycle event" = "grey90",
    "Mixed" = "grey100" # Later I will add a pattern here to distinguish it from "None" and will add a Note!
      )
)

#A second one that excludes "None"
scale_fill_event_type2 <- scale_fill_manual(
  breaks = c("Economic distress",
             "Natural disaster",
             "Climatic event",
             "Social conflict",
             "Life-cycle event",
             "Mixed"),
  values = c(
    "Economic distress" = "grey10",
    "Natural disaster" = "grey30",
    "Climatic event" = "grey50",
    "Social conflict" = "grey70",
    "Life-cycle event" = "grey90",
    "Mixed" = "grey100" # Later I will add a pattern here to distinguish it from "None" and will add a Note!
  )
)

#Event time horizon
scale_fill_event_time <- scale_fill_manual(
  breaks = c("Long",
             "Medium",
             "Short"),
  values = c("Long" = "grey30",
    "Medium" = "grey60",
    "Short" = "grey90"
    )
)


#Source of instability
scale_fill_source <- scale_fill_manual(values = c(
  "Measurement-related" = "grey70",
  "Context or state dependence" = "grey40",
  "Genuine change" = "grey10",
  "Unclear" = "grey95"
))

#Direction of change for RPs
scale_fill_direction_risk <- scale_fill_manual(values = c(
  "Less risk-averse" = "grey70",
  "Mixed / heterogenous direction" = "grey40",
  "More risk-averse" = "grey10",
  "Not specified" = "grey95"
))

#Direction of change for TPs
#First lump the categories further together:
instab <- instab %>%
  mutate(
    time_direction_clean = case_when(
      time_direction %in% c("More impatient", 
                            "More impatient or more present biased",
                            "More impatient or more present-biased") ~ "More impatient",
      time_direction %in% c("More patient", 
                            "More patient or less present biased",
                            "More patient or less present-biased") ~ "Less impatient",
      TRUE ~ time_direction   # keeps everything else unchanged
    )
  )


scale_fill_direction_time <- scale_fill_manual(values = c(
  "Less impatient" = "grey70",
  "Mixed / heterogenous direction" = "grey40",
  "More impatient" = "grey10",
  "Not specified" = "grey95"
))

################################################################################
#Figure 2 Overview of the literature
################################################################################

#Stacked bar chart of preference domain and study design over the years

#Panel A: preference domain over time
year_domain <- instab %>%
  group_by(publication_year, pref_domain) %>%
  summarise(n = n(), .groups = "drop")

p2A <- ggplot(year_domain, aes(x = publication_year, y = n, fill = pref_domain)) +
  geom_book() +
  labs(
    x = "Publication year",
    y = "Number of studies",
    fill = "Preference domain"
  ) +
  theme_book + 
  scale_fill_domain
p2A

#Panel B: Study design per year
instab$data_structure <- factor(
  instab$data_structure,
  levels = c("Panel", "Repeated cross-section", "Cross-section")  #Make sure Panel is mentioned first
)

year_design <- instab %>%
  group_by(publication_year, data_structure) %>%
  summarise(n = n(), .groups = "drop")

p2B <- ggplot(year_design, aes(x = publication_year, y = n, fill = data_structure)) +
  geom_book() +
  labs(
    x = "Publication year",
    y = "Number of studies",
    fill = "Study design"
  ) +
  theme_book+ 
  scale_fill_design
p2B



#Panel C: Events per year
instab <- instab %>%
  mutate(
    event_type = ifelse(is.na(event_type) | event_study == "No",
                        "None",
                        event_type)
  )

#Show "None" first on the list
instab$event_type <- factor(
  instab$event_type,
  levels = c("None",
             "Economic distress",
             "Natural disaster",
             "Climatic event",
             "Social conflict",
             "Life-cycle event",
             "Mixed")
)


year_event <- instab %>%
  group_by(publication_year, event_type) %>%
  summarise(n = n(), .groups = "drop")

p2C <- ggplot(year_event,
              aes(x = publication_year,
                  y = n,
                  fill = event_type,
                  pattern = event_type)) +
  geom_col_pattern(
    position = "stack",
    color = "black",
    size = 0.2,
    pattern_fill = "black",
    pattern_colour = "black"
  ) +
  labs(
    x = "Publication year",
    y = "Number of studies",
    fill = "Event type"
  ) +
  theme_book +
  scale_fill_event_type +
  scale_pattern_manual(values = c(
    "None" = "none",
    "Economic distress" = "none",
    "Natural disaster" = "none",
    "Climatic event" = "none",
    "Social conflict" = "none",
    "Life-cycle event" = "none",
    "Mixed" = "stripe"   # pattern ONLY here
  )) +
  scale_pattern_density_manual(values = c(
    "Mixed" = 0.4
  )) +
  scale_pattern_spacing_manual(values = c(
    "Mixed" = 0.03
  )) +
  guides(
    fill = guide_legend(override.aes = list(
      pattern = c("none", "none", "none", "none", "none", "none", "stripe")
    )),
    pattern = "none"
  )
p2C

#Combine A and B and C
(p2A / p2B / p2C) +
  plot_layout(guides = "keep")

ggsave("Fig2.png", width = 10, height = 12, dpi = 300)

################################################################################
#Figure 3 Stability outcomes
################################################################################

#Stability conclusions overall, by measurement method and by study design

#Aggregate counts
unique(instab$stability_conclusion) #"Mixed"    "Unstable" "Stable"   "Mixed " 
instab <- instab %>%
  mutate(
    stability_conclusion = str_trim(stability_conclusion),  # remove spaces
    stability_conclusion = str_to_title(stability_conclusion) # standardize case
  )

instab <- instab %>%
  mutate(
    measurement_method = str_trim(measurement_method),  # remove spaces
    measurement_method = str_to_title(measurement_method) # standardize case
  )

instab$stability_conclusion <- factor(
  instab$stability_conclusion,
  levels = c("Stable", "Mixed", "Unstable")  #Make sure Stable is mentioned first
)



#Counts of Stable, Mixed, Unstable
stability_counts <- instab %>%
  group_by(stability_conclusion) %>%
  summarise(n = n(), .groups = "drop")

p3A <- ggplot(stability_counts,
                      aes(x = stability_conclusion, y = n, fill = stability_conclusion)) +
  geom_col() +
  labs(
    x = "",
    y = "Number of studies",
    title = "Stability of risk and time preferences across studies"
  ) +
  theme_book +
  scale_fill_stability
p3A

#Stability by measurement method
stability_measurement <- instab %>%
  group_by(measurement_method, stability_conclusion) %>%
  summarise(n = n(), .groups = "drop")

p3B <- ggplot(stability_measurement,
                        aes(x = stability_conclusion, y = n, fill = measurement_method)) +
  geom_col(position = "fill") +
  labs(
    x = "Stability conclusion",
    y = "Share of studies",
    fill = "Measurement method"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_book +
  scale_fill_measurement
p3B

#Stability by study design
stability_design <- instab %>%
  group_by(data_structure, stability_conclusion) %>%
  summarise(n = n(), .groups = "drop")

p3C <- ggplot(stability_design,
                   aes(x = stability_conclusion, y = n, fill = data_structure)) +
  geom_col(position = "fill") +
  labs(
    x = "Stability conclusion",
    y = "Share of studies",
    fill = "Study design"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_book +
  scale_fill_design
p3C

#Combine p2B and p2C
p3B/p3C
  plot_layout(guides = "collect")
ggsave("Fig3.png", width = 10, height = 8, dpi = 300)

################################################################################
#Figure 4: Role of shocks
################################################################################

#Stability conclusions by event-study and by type of event

#Aggregate counts
event_stability <- instab %>%
  group_by(event_study, stability_conclusion) %>%
  summarise(n = n(), .groups = "drop")

p4A <- ggplot(event_stability,
                  aes(x = stability_conclusion, y = n, fill = event_study)) +
  geom_col(position = "fill") +
  labs(
    x = "Stability conclusion",
    y = "Share of studies",
    fill = "Event study"
    ) +
  scale_y_continuous(labels = scales::percent) +
  theme_book +
  scale_fill_event
p4A

#Stability outcomes by event type
event_only <- instab%>%
  filter(event_study == "Yes")

event_type_stability <- event_only %>%
  group_by(event_type, stability_conclusion) %>%
  summarise(n = n(), .groups = "drop")

p4B <- ggplot(event_type_stability,
                       aes(x =stability_conclusion , y = n, fill = event_type, pattern = event_type)) +
  geom_col_pattern(position = "fill", color = "black", size = 0.2) +
  labs(
    x = "Stability conclusion",
    y = "Share of studies",
    fill = "Event type"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_book +
  scale_fill_event_type2 +
  scale_pattern_manual(values = c(
    "Economic distress" = "none",
    "Natural disaster" = "none",
    "Climatic event" = "none",
    "Social conflict" = "none",
    "Life-cycle event" = "none",
    "Mixed" = "stripe"   # pattern ONLY here
  )) +
  scale_pattern_density_manual(values = c(
    "Mixed" = 0.4
  )) +
  scale_pattern_spacing_manual(values = c(
    "Mixed" = 0.03
  )) +
  guides(
    fill = guide_legend(override.aes = list(
      pattern = c("none", "none", "none", "none", "none", "stripe")
    )),
    pattern = "none"
  )
p4B

#Combine A and B
p4A/p4B
plot_layout(guides = "collect")
ggsave("Fig4.png", width = 10, height = 12, dpi = 300)

#What event types dominate
event_counts <- event_only %>%
  count(event_type)

#Study design × Event study
design_event <- instab %>%
  group_by(data_structure, event_study) %>%
  summarise(n = n(), .groups = "drop")

p4C <- ggplot(design_event,
       aes(x = data_structure, y = n, fill = event_study)) +
  geom_col(position = "fill") +
  labs(
    x = "Study design",
    y = "Share of studies",
    fill = "Event study",
    title = "Event studies by study design"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_book +
  scale_fill_event
p4C
#ggsave("Fig4C.png", width = 10, height = 8, dpi = 300)





################################################################################
#Figure 5, 6 and 7: Sources of instability
################################################################################

#Only studies that are not stable
instability_data <- instab %>%
  filter(stability_conclusion != "Stable")

#Order the sources
instab$instab_source <- factor(
  instab$instab_source,
  levels = c(
    "Genuine change",
    "Context or state dependence",
    "Measurement-related",
    "Unclear"
  )
)

instability_counts <- instability_data %>%
  group_by(instab_source) %>%
  summarise(n = n(), .groups = "drop")

#Stability outcome x source
instability_breakdown <- instability_data %>%
  group_by(instab_source, stability_conclusion) %>%
  summarise(n = n(), .groups = "drop")

p5 <- ggplot(instability_breakdown,
              aes(x = instab_source,
                  y = n,
                  fill = stability_conclusion)) +
  geom_col(position = "fill") +
  labs(
    x = "Source of instability",
    y = "Share of studies",
    fill = "Stability conclusion"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_book +
  scale_fill_stability +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p5
ggsave("Fig5.png", width = 10, height = 8, dpi = 300)


#Figure 6: Source x Measurement method x Study design (analogous to Fig2)

instability_breakdown2 <- instability_data %>%
  group_by(instab_source, measurement_method) %>%
  summarise(n = n(), .groups = "drop")
p6A <- ggplot(instability_breakdown2,
              aes(x = instab_source,
                  y = n,
                  fill = measurement_method)) +
  geom_col(position = "fill") +
  labs(
    x = "Source of instability",
    y = "Share of studies",
    fill = "Measurement method"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_book +
  scale_fill_measurement +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p6A



instability_breakdown3 <- instability_data %>%
  group_by(instab_source, data_structure) %>%
  summarise(n = n(), .groups = "drop")

p6B <- ggplot(instability_breakdown3,
              aes(x = instab_source,
                  y = n,
                  fill = data_structure)) +
  geom_col(position = "fill") +
  labs(
    x = "Source of instability",
    y = "Share of studies",
    fill = "Study design"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_book +
  scale_fill_design +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p6B

#Combine p6A and p6B
p6A/p6B
plot_layout(guides = "collect")
ggsave("Fig6.png", width = 10, height = 8, dpi = 300)


#Figure 7: Source x Event study x Types of event x Event horizon
instability_event <- instability_data %>%
  group_by(event_study, instab_source) %>%
  summarise(n = n(), .groups = "drop")

p7A <- ggplot(instability_event,
                              aes(x = instab_source,
                                  y = n,
                                  fill = event_study)) +
  geom_col(position = "fill") +
  labs(
    x = "Source of instability",
    y = "Share of studies",
    fill = "Event studies"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_book +
  scale_fill_event
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p7A

#Source x Type of event
eventANDinstab_only <- instability_data%>%  #Exclude those with no exogenous events and those with stable conclusions
  filter(event_study == "Yes")

event_type_source <- eventANDinstab_only %>%
  group_by(event_type, instab_source) %>%
  summarise(n = n(), .groups = "drop")

p7B <- ggplot(event_type_source,
              aes(x = instab_source, y = n, fill = event_type, pattern = event_type)) +
  geom_col_pattern(position = "fill", color = "black", size = 0.2) +
  labs(
    x = "Source of instability",
    y = "Share of studies",
    fill = "Event type"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_book +
  scale_fill_event_type2 +
  scale_pattern_manual(values = c(
    "Economic distress" = "none",
    "Natural disaster" = "none",
    "Climatic event" = "none",
    "Social conflict" = "none",
    "Life-cycle event" = "none",
    "Mixed" = "stripe"   # pattern ONLY here
  )) +
  scale_pattern_density_manual(values = c(
    "Mixed" = 0.4
  )) +
  scale_pattern_spacing_manual(values = c(
    "Mixed" = 0.03
  )) +
  guides(
    fill = guide_legend(override.aes = list(
      pattern = c("none", "none", "none", "none", "none", "stripe")
    )),
    pattern = "none"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p7B

# Source x time horizon
event_type_time <- eventANDinstab_only %>%
  group_by(event_horizon, instab_source) %>%
  summarise(n = n(), .groups = "drop")

p7C <- ggplot(event_type_time,
              aes(x = instab_source, y = n, fill = event_horizon)) +
  geom_col(position = "fill") +
  labs(
    x = "Source of instability",
    y = "Share of studies",
    fill = "Event time horizon"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_book +
  scale_fill_event_time +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p7C

#Combine A and B
p7A/p7B/p7C
plot_layout(guides = "collect")
ggsave("Fig7.png", width = 10, height = 12, dpi = 300)


################################################################################
#Figure 8: Sources of instability by direction of change
################################################################################
#Panel A: Risk
#Stability outcome x direction of change

#Only instab data and risk
instab_risk_data <- instability_data %>%
  filter(pref_domain != "Time") %>%
  mutate(
    risk_direction = factor(
      risk_direction,
      levels = c(
        "More risk-averse",
        "Mixed / heterogenous direction",
        "Less risk-averse",
        "Not specified"
      )
    )
  )

#Order the sources
instab_risk_data$instab_source <- factor(
  instab_risk_data$instab_source,
  levels = c(
    "Genuine change",
    "Context or state dependence",
    "Measurement-related",
    "Unclear"
  )
)

instability_risk <- instab_risk_data %>%
  group_by(instab_source, risk_direction) %>%
  summarise(n = n(), .groups = "drop")

p8A <- ggplot(instability_risk,
              aes(x = instab_source,
                  y = n,
                  fill = risk_direction)) +
  geom_col(position = "fill") +
  labs(
    x = "Source of instability",
    y = "Share of studies",
    fill = "Direction of change in RPs"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_book +
  scale_fill_measurement +
  scale_fill_direction_risk
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p8A


#Panel B: Time
instab_time_data <- instability_data %>%
  filter(pref_domain != "Risk") %>%
  mutate(
    time_direction_clean = factor(
      time_direction_clean,
      levels = c(
        "More impatient",
        "Mixed / heterogenous direction",
        "Less impatient",
        "Not specified"
      )
    )
  )

#Still one NA, which exhibits stability in time preferences (Study S=087)
instab_time_data <- instab_time_data %>%
  filter(time_direction_clean != "NA")

#Order the sources
instab_time_data$instab_source <- factor(
  instab_time_data$instab_source,
  levels = c(
    "Genuine change",
    "Context or state dependence",
    "Measurement-related",
    "Unclear"
  )
)

instability_time <- instab_time_data %>%
  group_by(instab_source, time_direction_clean) %>%
  summarise(n = n(), .groups = "drop")

p8B <- ggplot(instability_time,
              aes(x = instab_source,
                  y = n,
                  fill = time_direction_clean)) +
  geom_col(position = "fill") +
  labs(
    x = "Source of instability",
    y = "Share of studies",
    fill = "Direction of change in TPs"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_book +
  scale_fill_direction_time +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p8B

#Combine A and B
p8A/p8B
plot_layout(guides = "collect")
ggsave("Fig8.png", width = 10, height = 8, dpi = 300)
