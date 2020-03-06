library(tidyverse)

# Wczytanie danych i przypisanie do zmiennej
dane <- read.csv("ncn_grants.csv")

# ----- Wstępna eksploracja danych -----
dim(dane)
head(dane)
str(dane)

# Zmieńmy typ kolumny zawierającej daty
dane[["date"]] <- as.Date(dane[["date"]], format = "%Y-%m-%d")

# Podstawowe podsumowanie danych
summary(dane)

dane <- na.omit(dane)
summary(dane)


# ----- Przetwarzanie danych -----
# Wybieranie kolumn
select(dane, c(type, panel, subpanel))

# Usuwanie kolumn
select(dane, -date)

# Filtrowanie

# Załóżmy, że interesują nas tylko zmienne type, panel, subpanel oraz wiersze odpowiadające panelowi NZ. 
# Następnie zliczmy liczbę takich projektów. 
# sposób 1:
wybrane <- select(dane, c(type, panel, subpanel))
wybrane2 <- filter(wybrane, panel == 'NZ')
nrow(wybrane2)
# sposób 2:
nrow(filter(select(dane, c(type, panel, subpanel)), panel == 'NZ'))

# sposób 3 - najbardziej czytelny:
# przydatny skrót klawiszowy: %>% = ctrl + shift + m
dane %>% 
  select(c(type, panel, subpanel)) %>% 
  filter(panel == 'NZ') %>% 
  nrow()

# Znajdźmy projekty o budżecie przekraczającym 2mln zł realizowany przez więcej niż 15 badaczy
dane %>% 
  filter(budget > 2000000 & coinvestigators > 15)

# Zadanie 
# Znajdź projekty z konkursu SONATA w podpanelu NZ1 lub NZ2, które otrzymały finansowanie
# w kwocie wyższej niż 1mln zł. Ile jest takich projektów?
dane %>% 
  filter(type == 'SONATA' & budget > 1000000 & subpanel %in% c('NZ1', 'NZ8'))

# Podsumowywanie i sortowanie danych
# Zliczmy liczby projektów w poszczególnych panelach
dane %>% 
  group_by(panel) %>% 
  summarise(count = n())

# Policzmy średnie wysokości budżetu grantów MAESTRO w zależności od panelu i czasu trwania projektu
dane %>% 
  filter(type == 'MAESTRO') %>% 
  group_by(panel, duration) %>% 
  summarise(srednia = mean(budget)) %>% 
  arrange(srednia)

# Znajdźmy 5 projektów z panelu HS i typu OPUS, które otrzymały najwyższe finansowanie
dane %>% 
  filter(panel == 'HS' & type == 'OPUS') %>% 
  arrange(desc(budget)) %>% 
  head(5)

# Zadanie 
# Wylicz średnie wysokości finansowania grantów SONATINA w podpanelach ST
dane %>% 
  filter(panel == 'ST') %>% 
  group_by(subpanel) %>% 
  summarise(srednia = mean(budget))

# Zadanie
# Wylicz ile grantów PRELUDIUM o budżecie powyżej 100 tys. zł zostało przyznanych w poszczególnych panelach 
dane %>% 
  filter(type == 'PRELUDIUM' & budget > 100000) %>% 
  group_by(panel) %>% 
  summarise(liczba = n())


# Modyfikacje i dodawanie nowych zmiennych

# Przedstawmy wysokość budżetu w tysiącach złotych
dane %>% 
  mutate(budget = budget/100)

# Wyliczmy minimalny, maksymalny i średni budżet projektów dla każdego typu, 
# biorąc pod uwagę jedynie projekty trwające 36 miesięcy. 
dane %>% 
  filter(duration == 36) %>% 
  group_by(panel) %>% 
  summarise(sredni_budzet = mean(budget), min_budzet = min(budget), max_budzet = max(budget))

# Stwórzmy nową ramkę danych zawierającą projekty PRELUDIUM złożone w latach 2016-18
dane4 <- dane %>% 
  filter(str_detect(date, '2016|2017|2018') & type == 'PRELUDIUM')

# Korzystając z nowej ramki danych, sprawdźmy sumę środków przekazanych na realizację 
# projektów w poszczególnych edycjach. 
dane4 %>% 
  group_by(date) %>% 
  summarise(suma_srodkow = sum(budget))
  
# Zadanie
# Korzystając z dane4 określ liczby wniosków złożonych w poszczególnych edycjach
dane4 %>% 
  group_by(date) %>% 
  summarise(count = n())

# Zadanie 
# Korzystając z dane4 określ sumę środków przeznaczonych na każdy z paneli 
# w poszczególnych edycjach. Na który panel przeznaczone są zazwyczaj największe środki?
dane4 %>% 
  group_by(date, panel) %>% 
  summarise(suma = sum(budget)) %>% 
  arrange(desc(suma))
