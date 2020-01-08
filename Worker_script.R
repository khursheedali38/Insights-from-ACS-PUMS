#load libraries
library(tidyr)
library(tidyverse)
library(ggplot2)
library(dplyr)
load("df_a.RData")
load("df_b.RData")
load("df_c.RData")
load("df_d.RData")


#the State columns in pumsa is char type and double in rest, therefore type casting to adjust that, else error while binding
df_a_mod$ST <- as.numeric(df_a_mod$ST)

#bind the 4 dataframes into 1, don't remove incomplete cases as we can remove when working on subset, if we remove now then there is a possibility that working subset may have none and we loose lot of data
df_abcd <- bind_rows(df_a_mod, df_b_mod, df_c_mod, df_d_mod)

#save the above dataframe
save(df_abcd, file = "pums_abcd.RData")

#remove the 4 dataframes
rm(df_a_mod, df_b_mod, df_c_mod, df_d_mod)

#gathering the data dictionaries for mapping
data_dict <- read_csv("csv_pus/PUMS_Data_Dictionary_2013-2017.csv", col_names = FALSE, skip_empty_rows = TRUE, progress = show_progress())

#taking unique as else key error based on tidy data rule as ST and REGION are repeated, one for Housing and other for person, key error check github issue
data_dict_filtered <- unique(data_dict) %>% 
  na.omit(data_dict) %>%
  select(X2, X5, X7) %>%
  filter(X2 %in% colnames(df_abcd)) 

colnames(data_dict_filtered) <- c("VAR", "CODE", "VAL")

#storing the dictionary for each of our selected columns
for(name in colnames(df_abcd)) {
  dict_temp <- data_dict_filtered %>%
    filter(VAR == name) %>%
    spread(VAR, CODE)
  
  
  assign(paste0("dict_", name), dict_temp)
}


# modifying the dict_ST for better analysis and cleaner output as bigger names mightnbot fit onto display and converting the ST to numeric as it is in numeric in df_Abcd, compartible join operation
dict_ST <- dict_ST %>%
  separate(VAL, into = c("State", "Code"), sep = "/")

dict_ST$ST <- as.numeric(dict_ST$ST)

head(dict_ST)

#demographics of population across different states
#different age groups across states
df_abcd %>%
  select(ST, AGEP) %>%
  group_by(ST, AGEP) %>%
  summarize(
    count = n()
  ) %>%
  left_join(dict_ST, by = "ST") %>%
  select(Code, AGEP, count) %>%
  ggplot(aes(x = Code, y = count, fill = AGEP)) + 
  geom_bar(alpha = 0.5, stat = "identity") + 
  labs(x = "States", fill = "Age") +
  theme(
    axis.text.x = element_text(angle=90, hjust = 1, size = 5),
    legend.text = element_text(size = 5), 
  )



#education pattern among different age groups, quite a 40% are less than 3 years old
df_abcd %>%
  select(AGEP, SCHL) %>%
  left_join(dict_SCHL, by = "SCHL") %>%
  mutate(
    Age = AGEP, 
    Education = VAL
  ) %>%
  group_by(Age, Education) %>%
  summarize(
    count = n() / sum(n())
  ) %>%
  na.omit() %>%
  ggplot(aes(x="", y = count, fill = Education)) + 
  geom_bar(stat="identity", width = 1, alpha = 0.5) + 
  coord_polar("y", start = 0)

#class of workers in different age groups as mumeric
dict_COW$COW <- as.numeric(dict_COW$COW)

#agegroup < 15 aren\'t working
df_abcd %>%
  select(AGEP, COW) %>%
  left_join(dict_COW, by = "COW") %>%
  mutate(
    Age = AGEP, 
    Class = VAL
  )%>%
  count(Age, Class) %>%
  filter(Age > 15) %>%
  ggplot(aes(x = Age, y = n, fill = Class)) +
  geom_bar(stat = "identity") +
  theme(
    axis.text.x = element_text(angle=90, hjust = 1, size = 5),
    legend.text = element_text(size = 5), 
  )



# Educational qualifications, degrees type Master, bachelors etc among individual
dict_FOD1P$FOD1P <- as.numeric(dict_FOD1P$FOD1P)
dict_FOD2P$FOD2P <- as.numeric(dict_FOD2P$FOD2P)

df_abcd %>%
  select(SCHL, FOD1P, FOD2P) %>%
  mutate(
    FOD1P = as.numeric(FOD1P), 
    FOD2P = as.numeric(FOD2P)
  ) %>%
  na.omit() %>%
  left_join(dict_FOD1P, by = "FOD1P") %>%
  left_join(dict_FOD1P, by = c("FOD2P" = "FOD1P")) %>%
  left_join(dict_SCHL, by = "SCHL") %>%
  mutate(
    Domain1 = VAL.x,
    Domain2 = VAL.y,
    Degree = VAL
  ) %>%
  select(Degree, Domain1, Domain2) %>%
  count(Degree) %>%
  mutate(
    percent = n / sum(n) * 100
  ) %>%
  ggplot(aes(x="", y = percent, fill = Degree)) + 
  geom_bar(stat="identity", width = 1, alpha = 0.5) + 
  coord_polar("y", start = 0) + 
  labs(x = element_blank(), y = element_blank())

# Facet wrapping the degree across the domain1 or Field of degree 1 among individuals
df_abcd %>%
  select(SCHL, FOD1P, FOD2P) %>%
  na.omit() %>%
  left_join(dict_FOD1P, by = "FOD1P") %>%
  left_join(dict_FOD1P, by = c("FOD2P" = "FOD1P")) %>%
  left_join(dict_SCHL, by = "SCHL") %>%
  mutate(
    Domain1 = VAL.x,
    Domain2 = VAL.y,
    Degree = VAL
  ) %>%
  select(Degree, Domain1, Domain2) %>%
  filter(str_detect(Domain1, "Computer|Information" )) %>%
  count(Degree, Domain1, Domain2) %>%
  ggplot(aes(x = Degree, y = n, fill = Domain1)) +
  geom_bar(alpha = 0.5, stat = "identity") +
  facet_wrap(vars(Domain1)) +
  theme(
    axis.text.x = element_text(angle=90, hjust = 1, size = 5),
    legend.text = element_text(size = 5)
  )


# state wise cow
#change type to numeric for join
df_abcd %>% 
  select(ST, COW) %>%
  na.omit() %>%
  left_join(dict_ST, by = "ST") %>%
  left_join(dict_COW, by = "COW") %>%
  mutate(
    CW = VAL
  ) %>%
  select(State, CW) %>%
  group_by(State, CW) %>%
  summarise(
    count = n()
  ) 


# modifying the type of time
df_abcd$JWDP <- as.numeric(df_abcd$JWDP)
df_abcd$JWAP <- as.numeric(df_abcd$JWAP)

# performing a linear regression on arrival and departure
df_abcd %>%
  select(JWAP, JWDP) %>%
  na.omit() %>%
  ggplot(aes(x = JWDP, y = JWAP)) +
  geom_point()


relation <- lm(df_abcd$JWDP ~ df_abcd$JWAP)
relation

#summary of relation
options(scipen=999) 
summary(relation)
```