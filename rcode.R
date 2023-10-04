library(tidyverse)
library(cowplot)
library(urbnmapr)


#source of information:
# https://aidsvu.org/data-methods/data-methods-city-level/
# https://map.aidsvu.org/map
# https://data.census.gov/
dir.create('results', showWarnings = FALSE)
edu = read.csv('raw_data/edu_ACSST5Y2021.S1501-Data.csv')
edu = edu[-1,c("GEO_ID", "NAME", "S1501_C02_014E")]
colnames(edu)[3] = 'education'
edu$education = as.numeric(edu$education)

income = read.csv('raw_data/income_ACSST5Y2021.S1903-Data.csv')
income = income[-1,c("GEO_ID", "NAME", "S1903_C03_001E")]
colnames(income)[3] = 'med_income'
income$med_income = as.numeric(income$med_income)

gini = read.csv('raw_data/gini_ACSDT5Y2021.B19083-Data.csv')
gini = gini[-1,c("GEO_ID", "NAME", "B19083_001E")]
colnames(gini)[3] = 'gini'
gini$gini = as.numeric(gini$gini)


poverty = read.csv('raw_data/pov_ACSST5Y2020.S1701-Data.csv')
poverty = poverty[-1,c("GEO_ID", "NAME", "S1701_C03_001E")]
colnames(poverty)[3] = 'poverty'
poverty$poverty = as.numeric(poverty$poverty)

df = edu %>% 
  left_join(gini) %>% 
  left_join(income) %>% 
  left_join(poverty)

for(cl in colnames(df)){
  if(sum(is.na(df[, cl]))){
    mn = median(df[, cl], na.rm = TRUE)
    df[is.na(df[,cl]), cl] = mn
  }
}

hiv_dat = read.csv('raw_data/county.csv', skip = 6)
colnames(hiv_dat)[3] = 'hiv_rate'
hiv_dat$hiv_rate = as.numeric(hiv_dat$hiv_rate)
hiv_dat$hiv_rate[is.na(hiv_dat$hiv_rate)] = -1

for(i in 1: nrow(hiv_dat)){
  if(hiv_dat$hiv_rate[i]<0){
    hiv_dat$hiv_rate[i] = sample(1:13, 1, replace = TRUE)
  }
}

hiv_dat <- hiv_dat %>% 
  group_by(county, state) %>% 
  summarise(hiv_rate = sum(hiv_rate))
county_clean = function(x){
  county_sep = unlist(strsplit(x, ', '))[c(T,F)]
  suffix = c(' County', ' City and Borough',
             ' Borough', ' Census Area', 
             ' Municipality', ' city', ' Parish', ' Municipio')
  indx = which(endsWith(county_sep, suffix = suffix))
  if(length(indx)>0){
    sub(suffix[min(indx)], '', county_sep)  
  }else{
    county_sep
  }
  
}
for(i in 1:nrow(df)){
  df$county[i] = county_clean(df[, 'NAME'][i])
}
df$state = unlist(strsplit(df$NAME, ', '))[c(F,T)]

df = df %>% 
  left_join(hiv_dat)

ind1 = which(startsWith(df$NAME,'District of Columbia'))
ind2 = which(hiv_dat$county=='District of Columbia')
df$hiv_rate[ind1] = hiv_dat$hiv_rate[ind2]


df_clean = df %>% 
  drop_na()

kmdat = scale(df_clean[, c("education", "gini", 
                     'med_income', "poverty")])
cr = cor(kmdat)
wss = c()
for(i in 1:15){
  km = kmeans(kmdat, i, iter.max = 500, nstart = 20)
  wss = c(wss, km$tot.withinss)
}


png(filename = 'results/elbow.png')
plot(1:15, wss, 'b')
dev.off()

set.seed(123)
km = kmeans(kmdat, 5, iter.max = 500, nstart = 20)
df_clean$cluster = km$cluster

k = 5
report = df_clean %>% 
  group_by(cluster) %>% 
  summarise(education = median(education),
            gini = median(gini),
            med_income = median(med_income),
            poverty = median(poverty),
            hiv_rate = median(hiv_rate),
            freq = n()) %>% 
  arrange(hiv_rate) %>% 
  mutate(new_cluster = 1:k)

write.csv(report,
          'results/report.csv')
# Create a mapping from old cluster values to new values
cluster_mapping <- setNames(report$new_cluster, report$cluster)
df_clean$cluster <- cluster_mapping[as.character(df_clean$cluster)]

df_clean$county_long = unlist(strsplit(df_clean$NAME, ', '))[c(T,F)]

write.csv(df_clean,
          'results/df_with_clusters.csv')

#grab county shape files for our map
counties <- get_urbn_map("counties", sf = TRUE)


counties <- counties %>% 
  left_join(df_clean,
            c('state_name'='state',
              'county_name'='county_long'))
# The palette with grey:
cbPalette <- c("#E69F00", "#56B4E9", 
                        "#009E73", "#F0E442", "#0072B2",
                        "#D55E00", "#CC79A7")
whole_map <- ggplot() +
  geom_sf(data = counties,
          mapping = aes(fill = factor(cluster)))+
  theme_classic()+
  scale_fill_manual(name = "Clusters",values = cbPalette[1:5])+
  guides(fill = guide_legend(nrow = 1)) +
  theme(legend.title.align = 0.5,
        legend.position = "bottom",  # Position the legend at the top
        # legend.direction = "horizontal",  # Set the legend direction to horizontal
        # legend.box = "horizontal",
        plot.title = element_text(hjust = 0.5, size = 18),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        #Remove axis labels and ticks
        line = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

fl_map <- ggplot() +
  geom_sf(data = counties %>% 
            filter(state_abbv == "FL"),
          mapping = aes(fill = factor(cluster)))+
  theme_classic()+
  scale_fill_manual(name = "Clusters",values = cbPalette[1:5])+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 18),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        #Remove axis labels and ticks
        line = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

final_map <- ggdraw() +
  draw_plot(whole_map,
            x = -0.05, y = 0, width = 0.8, height = 1)+
  draw_plot(fl_map,
            x = 0.65, y = 0.3, width = .33, height = 0.33)+
  draw_label(
    'Socioeconomic status of counties',
    fontface = 'bold',
    x = 0.35,
    y = 0.95,
    hjust = 0
  )
#save files to folder
ggsave(filename = 'results/final_map.pdf', 
       plot=final_map, width = 7.2, height = 6, dpi = 300)
ggsave(filename = 'results/final_map.png', 
       plot=final_map, width = 7.2, height = 6,dpi = 300)


### ML
library(caret)

### random Forest
set.seed(1234)
train = df_clean[, c("education", "gini", 
                     'med_income', "poverty", "hiv_rate")]
train$cl = as.factor(ifelse(train$hiv_rate > median(train$hiv_rate),
                            'High', 'Low')
                     )
rf_train <- train(x = train[, c("education", "gini", 
                                'med_income', "poverty")],
                  y = train$cl,
                  method = "rf", ntree = 500,
                  tuneGrid = data.frame(mtry = c(4,2)),
                  trControl = trainControl(method = "cv", number = 10))

rf_train$results
# Get the best tune from the random forest
bt = rf_train$bestTune
rf_train$results

  
  
rf_imp <- varImp(rf_train$finalModel) %>% rownames_to_column(var = 'var')
rf_imp$Overall <- rf_imp$Overall*100/sum(rf_imp$Overall)
colnames(rf_imp)[2] <- 'Importance'

plot_imp <- ggplot(rf_imp %>% arrange(Importance), 
                   aes(x = reorder(var, Importance), y = Importance))+
  geom_bar(fill = "#033C5A", stat = 'identity', position=position_dodge())+
  ylab('Relative Importance')+
  ggtitle('Feature importance')+
  coord_flip()+
  theme_classic()+
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(face="bold", size = 14),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 16),
        legend.position = c(0.75, 0.27),
        legend.key.size = unit(.3, 'cm'))

ggsave(filename = 'results/plot_imp_cl.png', 
       plot=plot_imp, width = 7.2, height = 3,dpi = 300)
