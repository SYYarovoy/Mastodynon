```{r}
getwd()

library(rstatix)
#������ ������
data <- read.csv2("team_5.csv", stringsAsFactors = TRUE, encoding = "UTF-8")
```


```{r}
summary(data)

data_cleaned <- data %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(V0_DEM_GEN = "�������" %>% as.factor()) %>% #������ ���
  mutate(V0_DEM_AGE = V0_DEM_AGE + 20) %>% #������ �������
  mutate(id = .[,1]) %>%
  mutate(V1_NORM_ECG = V1_NORM_ECG %>% as.factor(), V1_NORM_PHYS = V1_NORM_PHYS %>% as.factor(), V1_NORM_VIT = V1_NORM_VIT %>% as.factor(), V2_NORM_ECG = V2_NORM_ECG %>% as.factor(), V2_NORM_PHYS = V2_NORM_PHYS %>% as.factor(), V2_NORM_VIT = V2_NORM_VIT %>% as.factor())


summary(data_cleaned)
view(dfSummary(data_cleaned))
print(dfSummary(data_cleaned, plain.ascii = FALSE), method = 'render')

```
```{r}
wil <- wilcox_test(data_cleaned, V1_CB_HCT ~ V2_CB_HCT, paired = FALSE, detailed = TRUE)
print(wil)

#������ � ������� �����������: 
data2 <- data_cleaned %>%
  filter(`V0_GRP` == "����������")

wil2 <- data2 %>%
  select(X.U.FEFF.ID,V1_CB_HCT, V2_CB_HCT) %>%
  pivot_longer(!`X.U.FEFF.ID`) %>%
  wilcox_test(value ~ name, paired = TRUE, detailed = TRUE) %>%
  print()

?mcnemar.test

#������ � ������� �����������: 
data2 <- data_cleaned %>%
  filter(`V0_GRP` == "����������")

#��� � ������ �����������:
sopr2 <- data2 %>%       
  select(V1_NORM_ECG, V2_NORM_ECG)

sopr_ispravl <- matrix(c(sum(sopr2[,1] == 0 & sopr2[,2] == 0), sum(sopr2[,1] == 0 & sopr2[,2] == 1), sum(sopr2[,1] == 1 & sopr2[,2] == 0), sum(sopr2[,1] == 1 & sopr2[,2] == 1)), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("V1_ECG_NORM=0", "V1_ECG_NORM=1"), c("V2_ECG_NORM=0", "V2_ECG_NORM=1"))) %>% print()

mcn <- sopr_ispravl %>%
  mcnemar.test() %>%
  tidy()

#����������� ������ � ������ �����������:
sopr2 <- data2 %>%       
  select(V1_NORM_PHYS, V2_NORM_PHYS)

sopr_ispravl <- matrix(c(sum(sopr2[,1] == 0 & sopr2[,2] == 0), sum(sopr2[,1] == 0 & sopr2[,2] == 1), sum(sopr2[,1] == 1 & sopr2[,2] == 0), sum(sopr2[,1] == 1 & sopr2[,2] == 1)), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("V1_ECG_PHYS=0", "V1_ECG_PHYS=1"), c("V2_ECG_PHYS=0", "V2_ECG_PHYS=1"))) %>% print()

mcn <- sopr_ispravl %>%
  mcnemar.test() %>%
  tidy()

#��������� ���������� ������ � ������ �����������:
sopr2 <- data2 %>%       
  select(V1_NORM_VIT, V2_NORM_VIT)

sopr_ispravl <- matrix(c(sum(sopr2[,1] == 0 & sopr2[,2] == 0), sum(sopr2[,1] == 0 & sopr2[,2] == 1), sum(sopr2[,1] == 1 & sopr2[,2] == 0), sum(sopr2[,1] == 1 & sopr2[,2] == 1)), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("V1_ECG_VIT=0", "V1_ECG_VIT=1"), c("V2_ECG_VIT=0", "V2_ECG_VIT=1"))) %>% print()

mcn <- sopr_ispravl %>%
  mcnemar.test() %>%
  tidy()



#������ � ������� �������: 
data3 <- data_cleaned %>%
  filter(`V0_GRP` == "�������")

#��� � ������ �������:
sopr3 <- data3 %>%       
  select(V1_NORM_ECG, V2_NORM_ECG)

sopr_ispravl <- matrix(c(sum(sopr3[,1] == 0 & sopr3[,2] == 0), sum(sopr3[,1] == 0 & sopr3[,2] == 1), sum(sopr3[,1] == 1 & sopr3[,2] == 0), sum(sopr3[,1] == 1 & sopr3[,2] == 1)), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("V1_ECG_NORM=0", "V1_ECG_NORM=1"), c("V2_ECG_NORM=0", "V2_ECG_NORM=1"))) %>% print()

mcn <- sopr_ispravl %>%
  mcnemar.test() %>%
  tidy()

#����������� ������ � ������ �������:
sopr3 <- data3 %>%       
  select(V1_NORM_PHYS, V2_NORM_PHYS)

sopr_ispravl <- matrix(c(sum(sopr3[,1] == 0 & sopr3[,2] == 0), sum(sopr3[,1] == 0 & sopr3[,2] == 1), sum(sopr3[,1] == 1 & sopr3[,2] == 0), sum(sopr3[,1] == 1 & sopr3[,2] == 1)), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("V1_ECG_PHYS=0", "V1_ECG_PHYS=1"), c("V2_ECG_PHYS=0", "V2_ECG_PHYS=1"))) %>% print()

mcn <- sopr_ispravl %>%
  mcnemar.test() %>%
  tidy()

#��������� ���������� ������ � ������ �������:
sopr3 <- data3 %>%       
  select(V1_NORM_VIT, V2_NORM_VIT)

sopr_ispravl <- matrix(c(sum(sopr3[,1] == 0 & sopr3[,2] == 0), sum(sopr3[,1] == 0 & sopr3[,2] == 1), sum(sopr3[,1] == 1 & sopr3[,2] == 0), sum(sopr3[,1] == 1 & sopr3[,2] == 1)), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("V1_ECG_VIT=0", "V1_ECG_VIT=1"), c("V2_ECG_VIT=0", "V2_ECG_VIT=1"))) %>% print()

mcn <- sopr_ispravl %>%
  mcnemar.test() %>%
  tidy()
```
������ �� ���, ������������ ������� � ������ ��������-������ ����������� �� � ����� ������� � ������ �����������, ��� � � ������ �������, ������������� ������� �� ����������.

```{r}
data_control <- data_cleaned %>%
  filter(V0_GRP == "����������")

names <- c("TIDES_AMOUNT", "CB_WBC", "CB_RBC", "CB_HGB", "CB_HCT", 
           "CB_PLT", "CB_NEUT.", "CB_LYM.", "CB_BAS.", 
           "NORM_ECG", "NORM_PHYS")
failnames <- c("CB_MON.", "CB_EO.", "NORM_VIT")

# ������� ������ ��������� ��� ���������� �����������
wilcox_results_df <- data.frame()

for (name in names) {
  name_1 <- paste0("V1_", name)
  name_2 <- paste0("V2_", name)
  # �������� ���� � ��������� ���������� � ����������
  result <- data_control %>%
    select(ID, name_1, name_2) %>%
    pivot_longer(!ID) %>%
    wilcox_test(value ~ name, paired = FALSE, detailed = TRUE) 
  
  # ��������� ���������� � ����������
  wilcox_results_df <- bind_rows(wilcox_results_df, result)
}

# ������� ���� ��������� � ������������ ������
print(wilcox_results_df)
```
