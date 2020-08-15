#0. LIBRARIES ----
library(tidyverse)  # Data manipulation
library(tidyquant)  # API for financial data
library(anomalize)  # Anomaly detection
library(scales)     # Date axis formatting
library(grid)       # Controls for visual components
library(gridExtra)  # Controls for visual components
library(gtable)     # Controls for visual components



#1. DATA ----
ntt_stock <- tq_get("NTTYY", 
                    get  = "stock.prices", 
                    from = " 2019-01-01",
                    to   = "2020-08-12")

#2. DETECTION ----

# GESD with STL & Twitter
anomaly_data <- ntt_stock %>%
    time_decompose(adjusted, method = "stl") %>%
    anomalize(remainder, method = "gesd") %>%
    select(date, observed,anomaly) %>% 
    mutate(Decomposition = "STL", Anomaly_Method = "GESD") %>% 
    bind_rows(ntt_stock %>%
                  time_decompose(adjusted, method = "twitter") %>%
                  anomalize(remainder, method = "gesd") %>%
                  select(date, observed,anomaly) %>% 
                  mutate(Decomposition = "Twitter", 
                         Anomaly_Method = "GESD")) %>% 
# IQR with STL & Twitter
    bind_rows(ntt_stock %>%
                  time_decompose(adjusted, method = "stl") %>%
                  anomalize(remainder, method = "iqr") %>%
                  select(date, observed,anomaly) %>% 
                  mutate(Decomposition = "STL", Anomaly_Method = "IQR")) %>%
    bind_rows(ntt_stock %>%
                  time_decompose(adjusted, method = "twitter") %>%
                  anomalize(remainder, method = "iqr") %>%
                  select(date, observed,anomaly) %>% 
                  mutate(Decomposition = "Twitter", Anomaly_Method = "IQR")) %>% 
    mutate_at(vars(date), ~ as.POSIXct(.)) 
    
#3. STATIC VALUES ----

#NTT color
ntt_color <- "#6684C3"

#Footnote
footnote= expression(paste(bold("Source: \n"),"Yahoo Finance. Extraction from Jan 1st '19 through Aug 12th '20 with the tidyquant API | ticker: NTTYY.\nwilberth@post.harvard.edu"))

#ggplot theme
my_theme <- theme(text = element_text(size = 18,family = "Georgia"), # general theming
               #axis formatting
               axis.title.y = element_blank(),
               axis.title.x = element_blank(),
               axis.text = element_text(size = 16),
               axis.ticks = element_blank(),
               strip.text.x = element_text(colour = "red",face = "bold"),
               strip.text.y = element_text(colour = ntt_color,face = "bold"),
               strip.background = element_rect(color="white", fill="white"),
               plot.title = element_markdown(),
               #panel formatting
               panel.grid.major.y = element_line(color='gray', size =  .3, linetype = "dotted"),
               panel.grid.minor.y = element_blank(),
               panel.grid.major.x = element_line(color='gray', size =  .3, linetype = "dotted"),
               panel.grid.minor.x = element_blank(),
               panel.background = element_blank(),
               panel.border = element_rect(color="lightgray", fill = NA),
               #legend formatting
               legend.justification = "center",
               legend.key = element_blank(),
               legend.position = "top")



#4. VISUAL ----

visual_anomalies <-  anomaly_data %>% 
    ggplot(aes(date, observed)) +
    geom_line() + 
    facet_grid(Decomposition ~ Anomaly_Method) +
    geom_point(data = anomaly_data %>% filter(anomaly=="Yes"),
               color  = "red",
               shape  = 21,
               size   = 1.5,
               stroke = 1.2,
               fill   ="white") + 
    scale_x_datetime(labels = label_date_short()) +
    scale_y_continuous(limits = c(18,26),
                       breaks = seq(18,26,2),
                       labels = c(18,20,22,24,"$26")) + my_theme +
    labs(y="",x="",
         title ="<b style='color:red'>Anomaly Detection Methods</b> <b style=color:'#6684C3'>Based on Decomposition Approaches</b>",
         subtitle="NTT's adjusted share price.\nAvg. Price: $22.2 in 2019 & $23.6 in 2020 as of Aug 12th.\n")


# Preparing the visual's components
ready_plot <- ggplotGrob(visual_anomalies)
ready_plot <- gtable_add_rows(ready_plot, 
                              heights = unit(1, "cm"))  
g <- arrangeGrob(ready_plot, 
                 bottom = textGrob(footnote, x = 0.05, 
                                   hjust = 0, vjust=-0.7, 
                                   gp = gpar(fontface = "italic", 
                                             fontsize = 12, 
                                             fontfamily="Georgia")))

# Saving the plot
ggsave("visual_anomalies.png", g, dpi=500,width = 13,height = 7)
