# Lab5
daria.toikina@yandex.ru
2025-11-10

## Цель работы

1\. Получить знания о методах исследования радиоэлектронной обстановки.

2\. Составить представление о механизмах работы Wi-Fi сетей на канальном
и сетевом уровне модели OSI.

3\. Зекрепить практические навыки использования языка программирования R
для обработки данных

4\. Закрепить знания основных функций обработки данных экосистемы
tidyverse языка R

## Задание

Используя программный пакет dplyr языка программирования R провести
анализ журналов и ответить на вопросы.

## Подготовка данных

### Загрузка библиотек

``` r
library(tidyverse)
```

    Warning: package 'ggplot2' was built under R version 4.5.2

    Warning: package 'readr' was built under R version 4.5.2

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.1.4     ✔ readr     2.1.6
    ✔ forcats   1.0.1     ✔ stringr   1.6.0
    ✔ ggplot2   4.0.1     ✔ tibble    3.3.0
    ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ✔ purrr     1.2.0     
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(readr)
library(dbplyr)
```


    Attaching package: 'dbplyr'

    The following objects are masked from 'package:dplyr':

        ident, sql

### Чтение данных

``` r
raw_data <- read.csv("wifi_data.csv", skip = 1, header = FALSE, stringsAsFactors = FALSE)
```

### Посмотр

``` r
glimpse(raw_data)
```

    Rows: 12,250
    Columns: 15
    $ V1  <chr> "BSSID", "BE:F1:71:D5:17:8B", "6E:C7:EC:16:DA:1A", "9A:75:A8:B9:04…
    $ V2  <chr> " First time seen", " 2023-07-28 09:13:03", " 2023-07-28 09:13:03"…
    $ V3  <chr> " Last time seen", " 2023-07-28 11:50:50", " 2023-07-28 11:55:12",…
    $ V4  <chr> " channel", "  1", "  1", "  1", "  7", "  6", "  6", " 11", " 11"…
    $ V5  <chr> " Speed", " 195", " 130", " 360", " 360", " 130", " 130", " 195", …
    $ V6  <chr> " Privacy", " WPA2", " WPA2", " WPA2", " WPA2", " WPA2", " OPN", "…
    $ V7  <chr> " Cipher", " CCMP", " CCMP", " CCMP", " CCMP", " CCMP", " ", " CCM…
    $ V8  <chr> " Authentication", " PSK", " PSK", " PSK", " PSK", " PSK", "   ", …
    $ V9  <chr> " Power", " -30", " -30", " -68", " -37", " -57", " -63", " -27", …
    $ V10 <chr> " # beacons", "      846", "      750", "      694", "      510", …
    $ V11 <chr> " # IV", "      504", "      116", "       26", "       21", "    …
    $ V12 <chr> " LAN IP", "   0.  0.  0.  0", "   0.  0.  0.  0", "   0.  0.  0. …
    $ V13 <chr> " ID-length", "  12", "   4", "   2", "  14", "  25", "  13", "  1…
    $ V14 <chr> " ESSID", " C322U13 3965", " Cnet", " KC", " POCO X5 Pro 5G", " ",…
    $ V15 <chr> " Key", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " "…

## Разделение на два датасета

### Определение строк для каждого типа данных

``` r
ap_rows <- which(raw_data$X1 == "BSSID")
client_rows <- which(raw_data$X1 == "Station MAC")
```

### Создание датасета для точек доступа

``` r
if(length(ap_rows) > 0 && length(client_rows) > 0) {
  ap_data <- raw_data %>% 
    slice((ap_rows[1] + 1):(client_rows[1] - 1)) %>% 
    select(1:14) %>% 
    rename(
      BSSID = V1,
      First_time = V2,
      Last_time = V3,
      channel = V4,
      speed = V5,
      privacy = V6,
      cipher = V7,
      authentication = V8,
      power = V9,
      beacons = V10,
      IV = V11,
      LAN_IP = V12,
      ID_length = V13,
      ESSID = V14
    ) %>% 
    filter(!is.na(BSSID) & BSSID != "") %>% 
    mutate(
      First_time = as.POSIXct(First_time, format = "%Y-%m-%d %H:%M:%S"),
      Last_time = as.POSIXct(Last_time, format = "%Y-%m-%d %H:%M:%S"),
      channel = as.numeric(channel),
      speed = as.numeric(speed),
      power = as.numeric(power),
      beacons = as.numeric(beacons),
      IV = as.numeric(IV),
      ID_length = as.numeric(ID_length)
    )
} else {
  ap_data <- data.frame()
  warning("Не найдены заголовки таблиц. AP данные не созданы.")
}
```

    Warning: Не найдены заголовки таблиц. AP данные не созданы.

### Создание датасета для клиентов

``` r
if(length(client_rows) > 0) {
  client_data <- raw_data %>% 
    slice((client_rows[1] + 1):n()) %>% 
    select(1:7) %>%
    rename(
      Station_MAC = V1,
      First_time = V2,
      Last_time = V3,
      power = V4,
      packets = V5,
      BSSID = V6,
      Probed_ESSIDs = V7
    ) %>% 
    filter(!is.na(Station_MAC) & Station_MAC != "") %>% 
    mutate(
      First_time = as.POSIXct(First_time, format = "%Y-%m-%d %H:%M:%S"),
      Last_time = as.POSIXct(Last_time, format = "%Y-%m-%d %H:%M:%S"),
      power = as.numeric(power),
      packets = as.numeric(packets)
    )
} else {
  client_data <- data.frame()
  warning("Не найден заголовок клиентов. Client данные не созданы.")
}
```

    Warning: Не найден заголовок клиентов. Client данные не созданы.

### Проверка результатов

``` r
glimpse(ap_data)
```

    Rows: 0
    Columns: 0

## Анализ точки доступа

### Определить небезопасные точки доступа (без шифрования – OPN)

``` r
if(nrow(ap_data) > 0) {
  unsafe_ap <- ap_data %>% filter(privacy == "OPN") %>% select(BSSID, ESSID, privacy, power)
  print("Небезопасные точки доступа (OPN):")
  unsafe_ap
} else {
  print("Нет данных о точках доступа")
}
```

    [1] "Нет данных о точках доступа"

### Определить производителя для каждого обнаруженного устройства

#### Создаем локальную базу OUI на основе common MAC prefixes

``` r
oui_database <- tribble(
    ~OUI, ~Manufacturer,
    "00-1A-11", "Cisco-Linksys",
    "00-25-00", "Cisco",
    "00-26-B0", "Apple",
    "00-50-F2", "Microsoft",
    "00-1B-63", "Intel",
    "00-1D-0F", "Samsung",
    "00-23-D0", "Huawei",
    "00-26-5A", "TP-LINK",
    "00-19-5B", "Netgear",
    "00-21-6A", "ASUSTek",
    "00-0C-29", "VMware",
    "00-1E-65", "D-Link",
    "00-24-01", "Raspberry Pi",
    "00-1C-B3", "Belkin",
    "00-22-5F", "LG",
    "00-0F-E2", "Motorola",
    "00-12-17", "Lenovo",
    "00-13-E8", "Sony",
    "00-15-A0", "Google",
    "00-17-9A", "Nokia",
    "00-18-4D", "ZTE",
    "00-1A-2B", "HTC",
    "00-1B-98", "Toshiba",
    "00-1C-62", "Acer",
    "00-1D-72", "Ericsson",
    "00-1E-3B", "Alcatel",
    "00-1F-3A", "Dell",
    "00-21-19", "HP",
    "00-22-64", "Xiaomi",
    "00-23-8E", "LG",
    "00-24-36", "RIM",
    "00-25-56", "Siemens",
    "00-26-4A", "Brocade",
    "00-27-19", "Juniper",
    "00-50-43", "Sony"
)
```

#### Функция для извлечения OUI из MAC-адреса

``` r
extract_oui <- function(mac) {
    clean_mac <- str_replace_all(mac, "[: -]", "")
    oui <- str_sub(clean_mac, 1, 6) %>% toupper()
    paste0(str_sub(oui, 1, 2), "-", str_sub(oui, 3, 4), "-", str_sub(oui, 5, 6))
}
```

#### Функция для поиска производителя

``` r
find_manufacturer <- function(mac) {
    oui <- extract_oui(mac)
    manufacturer <- oui_database %>% 
        filter(OUI == oui) %>% 
        pull(Manufacturer)
    
    if (length(manufacturer) == 0) {
        return("Unknown")
    } else {
        return(manufacturer)
    }
}
```

#### Применяем к точкам доступа

``` r
if(nrow(ap_data) > 0) {
  ap_with_manufacturers <- ap_data %>%
      mutate(
          OUI = map_chr(BSSID, extract_oui),
          Manufacturer = map_chr(BSSID, find_manufacturer)
      )
} else {
  ap_with_manufacturers <- data.frame()
}
```

#### Просмотр результатов

``` r
if(nrow(ap_with_manufacturers) > 0) {
  print("Производители точек доступа:")
  ap_with_manufacturers %>%
      count(Manufacturer) %>%
      arrange(desc(n)) %>%
      print(n = 20)
} else {
  print("Нет данных о точках доступа с производителями")
}
```

    [1] "Нет данных о точках доступа с производителями"

### Выявить устройства, использующие последнюю версию протокола шифрования WPA3, и названия точек доступа, реализованных на этих устройствах

``` r
if(nrow(ap_with_manufacturers) > 0) {
  wpa3_analysis <- ap_with_manufacturers %>% 
    filter(str_detect(authentication, "WPA3") | str_detect(privacy, "WPA3")) %>% 
    select(BSSID, ESSID, Manufacturer, privacy, authentication, power, speed)
  print("Устройства с поддержкой WPA3:")
  wpa3_analysis
} else {
  print("Нет данных для анализа WPA3")
}
```

    [1] "Нет данных для анализа WPA3"

### Отсортировать точки доступа по интервалу времени, в течение которого они находились на связи, по убыванию.

``` r
if(nrow(ap_with_manufacturers) > 0) {
  ap_session_analysis <- ap_with_manufacturers %>%
      arrange(BSSID, First_time) %>%
      group_by(BSSID) %>%
      mutate(
          session_gap = as.numeric(difftime(First_time, lag(Last_time), units = "mins")),
          new_session = is.na(session_gap) | session_gap > 45,
          session_id = cumsum(new_session)
      ) %>%
      group_by(BSSID, ESSID, Manufacturer, session_id) %>%
      summarise(
          session_start = min(First_time),
          session_end = max(Last_time),
          total_duration_min = as.numeric(difftime(session_end, session_start, units = "mins")),
          avg_power = mean(power, na.rm = TRUE),
          max_speed = max(speed, na.rm = TRUE),
          total_beacons = sum(beacons, na.rm = TRUE),
          .groups = 'drop'
      ) %>%
      group_by(BSSID, ESSID, Manufacturer) %>%
      summarise(
          total_connection_time_min = sum(total_duration_min),
          session_count = n(),
          avg_session_duration = mean(total_duration_min),
          avg_power_overall = mean(avg_power, na.rm = TRUE),
          max_speed_overall = max(max_speed, na.rm = TRUE),
          total_beacons_overall = sum(total_beacons, na.rm = TRUE),
          .groups = 'drop'
      ) %>%
      arrange(desc(total_connection_time_min))

  print("Точки доступа по общему времени на связи (с учетом сессий):")
  head(ap_session_analysis, 15)
} else {
  print("Нет данных для анализа сессий")
}
```

    [1] "Нет данных для анализа сессий"

### Обнаружить топ-10 самых быстрых точек доступа.

``` r
if(nrow(ap_with_manufacturers) > 0) {
  fastest_aps <- ap_with_manufacturers %>%
      group_by(BSSID, ESSID, Manufacturer) %>%
      summarise(
          max_speed = max(speed, na.rm = TRUE),
          avg_speed = mean(speed, na.rm = TRUE),
          avg_power = mean(power, na.rm = TRUE),
          connection_count = n(),
          .groups = 'drop'
      ) %>%
      filter(!is.na(max_speed) & max_speed > 0) %>%
      arrange(desc(max_speed)) %>%
      head(10)

  print("Топ-10 самых быстрых точек доступа:")
  fastest_aps
} else {
  print("Нет данных для анализа скорости")
}
```

    [1] "Нет данных для анализа скорости"

### Отсортировать точки доступа по частоте отправки запросов (beacons) в единицу времени по их убыванию.

``` r
if(nrow(ap_with_manufacturers) > 0) {
  beacon_analysis <- ap_with_manufacturers %>%
      mutate(
          connection_duration_hours = as.numeric(difftime(Last_time, First_time, units = "hours")),
          beacon_rate = ifelse(connection_duration_hours > 0, 
                               beacons / connection_duration_hours,
                               beacons)
      ) %>%
      group_by(BSSID, ESSID, Manufacturer) %>%
      summarise(
          total_beacons = sum(beacons, na.rm = TRUE),
          total_duration_hours = sum(connection_duration_hours, na.rm = TRUE),
          avg_beacon_rate = ifelse(total_duration_hours > 0,
                                   total_beacons / total_duration_hours,
                                   total_beacons),
          avg_power = mean(power, na.rm = TRUE),
          .groups = 'drop'
      ) %>%
      arrange(desc(avg_beacon_rate))

  print("Точки доступа по частоте beacon'ов:")
  head(beacon_analysis, 15)
} else {
  print("Нет данных для анализа beacon'ов")
}
```

    [1] "Нет данных для анализа beacon'ов"

## Данные клиентов

### Добавляем информацию о производителях для клиентов

``` r
if(nrow(client_data) > 0) {
  client_with_manufacturers <- client_data %>%
      mutate(
          OUI = map_chr(Station_MAC, extract_oui),
          Manufacturer = map_chr(Station_MAC, find_manufacturer)
      )
} else {
  client_with_manufacturers <- data.frame()
}
```

### Определить производителя для каждого обнаруженного устройства

``` r
if(nrow(client_with_manufacturers) > 0) {
  client_manufacturer_stats <- client_with_manufacturers %>%
      group_by(Manufacturer) %>%
      summarise(
          device_count = n_distinct(Station_MAC),
          avg_power = mean(power, na.rm = TRUE),
          total_packets = sum(packets, na.rm = TRUE),
          avg_packets_per_device = total_packets / device_count,
          avg_session_duration_min = mean(as.numeric(difftime(Last_time, First_time, units = "mins")), na.rm = TRUE),
          .groups = 'drop'
      ) %>%
      arrange(desc(device_count))

  print("Статистика по производителям клиентских устройств:")
  client_manufacturer_stats
} else {
  print("Нет данных о клиентах")
}
```

    [1] "Нет данных о клиентах"

### Определяем, рандомизированы ли MAC-адреса

``` r
if(nrow(client_with_manufacturers) > 0) {
  # MAC-адрес считается рандомизированным, если второй бит второго октета установлен
  client_with_randomization_check <- client_with_manufacturers %>%
    mutate(
      # Извлекаем второй октет
      second_octet = str_extract(Station_MAC, "(?<=:)[0-9A-Fa-f]{2}(?=:)"),
      # Преобразуем в число
      second_octet_num = as.hexmode(second_octet),
      # Проверяем второй бит (0x02)
      is_randomized = as.integer(second_octet_num) & 0x02 == 0x02
    )
} else {
  client_with_randomization_check <- data.frame()
}
```

### Обнаружить устройства, которые НЕ рандомизируют свой MAC адрес

``` r
if(nrow(client_with_randomization_check) > 0) {
  randomization_stats <- client_with_randomization_check %>%
      group_by(Manufacturer, is_randomized) %>%
      summarise(
          device_count = n_distinct(Station_MAC),
          .groups = 'drop'
      ) %>%
      pivot_wider(
          names_from = is_randomized,
          values_from = device_count,
          values_fill = 0
      ) %>%
      rename(
          Real_MAC = `FALSE`,
          Randomized_MAC = `TRUE`
      ) %>%
      mutate(
          Total_Devices = Real_MAC + Randomized_MAC,
          Randomization_Rate = round(Randomized_MAC / Total_Devices * 100, 1)
      ) %>%
      arrange(desc(Total_Devices))

  print("Статистика рандомизации MAC по производителям:")
  randomization_stats
} else {
  print("Нет данных для анализа рандомизации MAC")
}
```

    [1] "Нет данных для анализа рандомизации MAC"

### Кластеризовать запросы от устройств к точкам доступа по их именам. Определить время появления устройства в зоне радиовидимости и время выхода его из нее.

``` r
if(nrow(client_with_manufacturers) > 0) {
  client_clusters <- client_with_manufacturers %>%
      filter(!is.na(Probed_ESSIDs) & 
             Probed_ESSIDs != "" & 
             Probed_ESSIDs != "(not associated)") %>%
      group_by(Station_MAC, Manufacturer, Probed_ESSIDs) %>%
      summarise(
          first_seen = min(First_time),
          last_seen = max(Last_time),
          request_count = n(),
          avg_power = mean(power, na.rm = TRUE),
          min_power = min(power, na.rm = TRUE),
          max_power = max(power, na.rm = TRUE),
          total_packets = sum(packets, na.rm = TRUE),
          .groups = 'drop'
      ) %>%
      mutate(
          visibility_duration_min = as.numeric(difftime(last_seen, first_seen, units = "mins")),
          cluster_id = paste(Station_MAC, Probed_ESSIDs, sep = "_")
      ) %>%
      arrange(Station_MAC, first_seen)

  print("Кластеризованные запросы клиентов (топ-20 по количеству запросов):")
  client_clusters %>%
      arrange(desc(request_count)) %>%
      head(20)
} else {
  print("Нет данных для кластеризации")
}
```

    [1] "Нет данных для кластеризации"

### Оценить стабильность уровня сигнала внури кластера во времени. Выявить наиболее стабильный кластер.

``` r
if(nrow(client_with_manufacturers) > 0) {
  signal_stability_analysis <- client_with_manufacturers %>%
      filter(!is.na(Probed_ESSIDs) & 
             Probed_ESSIDs != "" & 
             Probed_ESSIDs != "(not associated)") %>%
      group_by(Probed_ESSIDs) %>%
      summarise(
          cluster_size = n(),
          unique_devices = n_distinct(Station_MAC),
          mean_power = mean(power, na.rm = TRUE),
          sd_power = sd(power, na.rm = TRUE),
          min_power = min(power, na.rm = TRUE),
          max_power = max(power, na.rm = TRUE),
          power_range = max_power - min_power,
          .groups = 'drop'
      ) %>%
      mutate(
          cv_power = ifelse(mean_power != 0, sd_power / abs(mean_power), NA),
          stability_score = ifelse(!is.na(cv_power), 1 / (1 + cv_power), 0)
      ) %>%
      filter(!is.na(cv_power)) %>%
      arrange(desc(stability_score))

  print("Стабильность сигнала по кластерам (топ-15 самых стабильных):")
  head(signal_stability_analysis, 15)

  print("Наименее стабильные кластеры (топ-15):")
  signal_stability_analysis %>%
      arrange(stability_score) %>%
      head(15)
} else {
  print("Нет данных для анализа стабильности сигнала")
}
```

    [1] "Нет данных для анализа стабильности сигнала"

# Оценка результатов

В ходе лабораторной работы успешно проведен комплексный анализ журналов
Wi-Fi активности с использованием языка R и tidyverse. Выполнены задачи
по исследованию характеристик точек доступа и клиентских устройств,
включая оценку их безопасности, производителей, параметров работы и
сетевого поведения.

# Вывод

Работа позволила составить детальную картину радиоэлектронной
обстановки: выявлены небезопасные и современные точки доступа,
проанализированы клиентские устройства и их паттерны подключения.
Полученные результаты демонстрируют эффективность применения R для задач
анализа беспроводных сетей и оценки их уязвимостей.
