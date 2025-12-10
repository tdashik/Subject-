# Lab2
daria.toikina@yandex.ru
2025-11-10

# Цель работы

1\. Развить практические навыки использования языка программирования R
для обработки данных 2. Закрепить знания базовых типов данных языка R 3.
Развить практические навыки использования функций обработки данных
пакета dplyr – функции select(), filter(), mutate(), arrange(), group\_
by()

## Установка пакета dplyr

    install.packages("dplyr")
    library(dplyr)

## Выполнение практического задания

### Проанализировать встроенный в пакет dplyr набор данных starwars с помощью языка R и ответить на вопросы:

#### 1. Сколько строк в датафрейме?

    starwars %>% nrow()
    [1] 87

#### 2. Сколько столбцов в датафрейме?

    starwars %>% ncol()
    [1] 14

#### 3. Как просмотреть примерный вид датафрейма?

    starwars %>% glimpse()
    Rows: 87
    Columns: 14
    $ name       <chr> "Luke Skywalker", "C-3PO", "R2-D2", "Darth Vader", "Leia Organa", "Owen Lars",…
    $ height     <int> 172, 167, 96, 202, 150, 178, 165, 97, 183, 182, 188, 180, 228, 180, 173, 175, …
    $ mass       <dbl> 77.0, 75.0, 32.0, 136.0, 49.0, 120.0, 75.0, 32.0, 84.0, 77.0, 84.0, NA, 112.0,…
    $ hair_color <chr> "blond", NA, NA, "none", "brown", "brown, grey", "brown", NA, "black", "auburn…
    $ skin_color <chr> "fair", "gold", "white, blue", "white", "light", "light", "light", "white, red…
    $ eye_color  <chr> "blue", "yellow", "red", "yellow", "brown", "blue", "blue", "red", "brown", "b…
    $ birth_year <dbl> 19.0, 112.0, 33.0, 41.9, 19.0, 52.0, 47.0, NA, 24.0, 57.0, 41.9, 64.0, 200.0, …
    $ sex        <chr> "male", "none", "none", "male", "female", "male", "female", "none", "male", "m…
    $ gender     <chr> "masculine", "masculine", "masculine", "masculine", "feminine", "masculine", "…
    $ homeworld  <chr> "Tatooine", "Tatooine", "Naboo", "Tatooine", "Alderaan", "Tatooine", "Tatooine…
    $ species    <chr> "Human", "Droid", "Droid", "Human", "Human", "Human", "Human", "Droid", "Human…
    $ films      <list> <"A New Hope", "The Empire Strikes Back", "Return of the Jedi", "Revenge of t…
    $ vehicles   <list> <"Snowspeeder", "Imperial Speeder Bike">, <>, <>, <>, "Imperial Speeder Bike"…
    $ starships  <list> <"X-wing", "Imperial shuttle">, <>, <>, "TIE Advanced x1", <>, <>, <>, <>, "X…

#### 4. Сколько уникальных рас персонажей (species) представлено в данных?

    starwars %>% select(species) %>% unique() %>% nrow()
    [1] 38

#### 5. Найти самого высокого персонажа.

starwars %\>% filter(height == max(height, na.rm = TRUE)) %\>%
select(name, height)

    # A tibble: 1 × 2
      name        height
      <chr>        <int>
    1 Yarael Poof    264

#### 6. Найти всех персонажей ниже 170

    starwars %>% filter(height < 170) %>% select(name, height)
    # A tibble: 22 × 2
       name                  height
       <chr>                  <int>
     1 C-3PO                    167
     2 R2-D2                     96
     3 Leia Organa              150
     4 Beru Whitesun Lars       165
     5 R5-D4                     97
     6 Yoda                      66
     7 Mon Mothma               150
     8 Wicket Systri Warrick     88
     9 Nien Nunb                160
    10 Watto                    137
    # ℹ 12 more rows
    # ℹ Use `print(n = ...)` to see more rows

#### 7. Подсчитать ИМТ (индекс массы тела) для всех персонажей. ИМТ подсчитать по формуле 𝐼= 𝑚/ℎ2 , где 𝑚 – масса (weight), а ℎ – рост (height).

    starwars %>% mutate(bmi = mass / (height/100)^2) %>% select(name, mass, height, bmi)
    # A tibble: 87 × 4
       name                mass height   bmi
       <chr>              <dbl>  <int> <dbl>
     1 Luke Skywalker        77    172  26.0
     2 C-3PO                 75    167  26.9
     3 R2-D2                 32     96  34.7
     4 Darth Vader          136    202  33.3
     5 Leia Organa           49    150  21.8
     6 Owen Lars            120    178  37.9
     7 Beru Whitesun Lars    75    165  27.5
     8 R5-D4                 32     97  34.0
     9 Biggs Darklighter     84    183  25.1
    10 Obi-Wan Kenobi        77    182  23.2
    # ℹ 77 more rows
    # ℹ Use `print(n = ...)` to see more rows

#### 8. Найти 10 самых “вытянутых” персонажей. “Вытянутость” оценить по отношению массы (mass) к росту (height) персонажей.

    starwars %>% mutate(stretch_ratio = mass / height) %>% arrange(desc(stretch_ratio)) %>% select(name, mass, height, stretch_ratio) %>% head(10)
    # A tibble: 10 × 4
       name                   mass height stretch_ratio
       <chr>                 <dbl>  <int>         <dbl>
     1 Jabba Desilijic Tiure  1358    175         7.76 
     2 Grievous                159    216         0.736
     3 IG-88                   140    200         0.7  
     4 Owen Lars               120    178         0.674
     5 Darth Vader             136    202         0.673
     6 Jek Tono Porkins        110    180         0.611
     7 Bossk                   113    190         0.595
     8 Tarfful                 136    234         0.581
     9 Dexter Jettster         102    198         0.515
    10 Chewbacca               112    228         0.491

#### 9. Найти средний возраст персонажей каждой расы вселенной Звездных войн.

    starwars %>% group_by(species) %>% filter(!is.na(birth_year)) %>% summarise(mean_age = mean(birth_year, na.rm = TRUE)) %>% arrange(desc(mean_age))
    # A tibble: 15 × 2
       species        mean_age
       <chr>             <dbl>
     1 Yoda's species    896  
     2 Hutt              600  
     3 Wookiee           200  
     4 Cerean             92  
     5 Zabrak             54  
     6 Human              53.7
     7 Droid              53.3
     8 Trandoshan         53  
     9 Gungan             52  
    10 Mirialan           49  
    11 Twi'lek            48  
    12 Rodian             44  
    13 Mon Calamari       41  
    14 Kel Dor            22  
    15 Ewok                8  

#### 10. Найти самый распространенный цвет глаз персонажей вселенной Звездных войн.

    starwars %>% count(eye_color, sort = TRUE) %>% head(1)
    # A tibble: 1 × 2
      eye_color     n
      <chr>     <int>
    1 brown        21

#### 11. Подсчитать среднюю длину имени в каждой расе вселенной Звездных войн.

    starwars %>%  mutate(name_length = nchar(name)) %>% group_by(species) %>% summarise(mean_name_length = mean(name_length, na.rm = TRUE)) %>%  arrange(desc(mean_name_length))
    # A tibble: 38 × 2
       species   mean_name_length
       <chr>                <dbl>
     1 Ewok                  21  
     2 Hutt                  21  
     3 Geonosian             17  
     4 Besalisk              15  
     5 Mirialan              14  
     6 Toong                 14  
     7 Aleena                12  
     8 Cerean                12  
     9 Gungan                11.7
    10 Human                 11.3
    # ℹ 28 more rows
    # ℹ Use `print(n = ...)` to see more rows
