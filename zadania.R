# Zadanie 1
# Znajdź projekty z konkursu SONATA w podpanelu NZ1 lub NZ2, które otrzymały finansowanie
# w kwocie wyższej niż 1mln zł. Ile jest takich projektów?
dane %>% 
  filter(type == 'SONATA' & budget > 1000000 & subpanel %in% c('NZ1', 'NZ8'))


# Zadanie 2
# Wylicz średnie wysokości finansowania grantów SONATINA w podpanelach ST
dane %>% 
  filter(panel == 'ST') %>% 
  group_by(subpanel) %>% 
  summarise(srednia = mean(budget))


# Zadanie 3
# Wylicz ile grantów PRELUDIUM o budżecie powyżej 100 tys. zł zostało przyznanych w poszczególnych panelach 
dane %>% 
  filter(type == 'PRELUDIUM' & budget > 100000) %>% 
  group_by(panel) %>% 
  summarise(liczba = n())


# Zadanie 4
# Korzystając z dane4 określ liczby wniosków złożonych w poszczególnych edycjach
dane4 %>% 
  group_by(date) %>% 
  summarise(count = n())


# Zadanie 5
# Korzystając z dane4 określ sumę środków przeznaczonych na każdy z paneli 
# w poszczególnych edycjach. Na który panel przeznaczone są zazwyczaj największe środki?
dane4 %>% 
  group_by(date, panel) %>% 
  summarise(suma = sum(budget)) %>% 
  arrange(desc(suma))
