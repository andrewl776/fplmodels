
# EDA ---------------------------------------------------------------------
library(corrplot)
library(GGally)

gw_data %>%
  select(where(is.numeric)) %>% 
  # select(-next_gw_points) %>%
  cor() %>% 
  as.data.frame() %>% 
  map(function(vec) sort(vec))

tiff("corrplot.tiff", units="in", width=5, height=5, res=300)
# insert ggplot code
gw_data %>%
  select(where(is.numeric)) %>% 
  # select(-next_gw_points) %>%
  cor() %>%
  corrplot(order = "hclust", tl.cex = 0.4)
dev.off()

# There is a variable (ep_next) which 

