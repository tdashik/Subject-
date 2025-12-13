# Lab4
daria.toikina@yandex.ru
2025-11-17

# Цель работы

1\. Зекрепить практические навыки использования языка программирования R
для обработки данных

2\. Закрепить знания основных функций обработки данных экосистемы
tidyverse языка R

3\. Закрепить навыки исследования метаданных DNS трафика

# Задание

Используя программный пакет dplyr, освоить анализ DNS логов с помощью
языка программирования R.

## Подготовка

### Установка необходимых библеотек

    > library(tidyselect)
    > library(httr)
    > library(jsonlite)
    > library(stringi)

### Загрузка данных

    url <- "https://storage.yandexcloud.net/dataset.ctfsec/dns.zip"
    > download.file(url, "dns.zip")
    trying URL 'https://storage.yandexcloud.net/dataset.ctfsec/dns.zip'
    Content type 'application/zip' length 6407934 bytes (6.1 MB)
    ==================================================
    downloaded 6.1 MB

### Добавьте пропущенные данные о структуре данных (назначении столбцов)

    col_names <- c( "ts", "uid", "id.orig_h", "id.orig_p", "id.resp_h", "id.resp_p", "proto", "trans_id", "query", "unknown1", "qclass", "qclass_name", "qtype", "qtype_name", "rcode", "rcode_name", "AA", "TC", "RD", "RA", "Z", "answers", "TTLs")

### Преобразуйте данные в столбцах в нужный формат

    dns_data <- read_tsv( "dns.log", col_names = col_names, col_types = cols(.default = col_character(), ts = col_double(), id.orig_p = col_integer(), id.resp_p = col_integer(), trans_id = col_integer(), qclass_name = col_integer(), qtype_name = col_integer(), AA = col_logical(), TC = col_logical(), RD = col_logical(), RA = col_integer()), na = c("-", "(empty)", ""), comment = "#", show_col_types = FALSE)

### Просмотрите общую структуру данных с помощью функции glimpse()

    > glimpse(dns_data)
    Rows: 427,935
    Columns: 23
    $ ts          <dttm> 2012-03-16 12:30:05, 2012-03-16 12:30:15, 2012-03-16 12:30:15, 2012-03-16 12:30:16, 20…
    $ uid         <chr> "CWGtK431H9XuaTN4fi", "C36a282Jljz7BsbGH", "C36a282Jljz7BsbGH", "C36a282Jljz7BsbGH", "C…

## Анализ

#### Функция для определения внутренних IP-адресов

    is_internal_ip <- function(ip) { grepl("^10\\.|^192\\.168\\.|^172\\.(1[6-9]|2[0-9]|3[0-1])\\.", ip)}

### Сколько участников информационного обмена в сети Доброй Организации?

    all_ips <- unique(c(dns_data$id.orig_h, dns_data$id.resp_h))
    internal_participants <- all_ips[sapply(all_ips, is_internal_ip)]
     cat(" Участников информационного обмена в сети Доброй Организации:", 
    +     length(internal_participants), "\n")
    Участников информационного обмена в сети Доброй Организации: 1267 

### Какое соотношение участников обмена внутри сети и участников обращений к внешним ресурсам?

    internal_count <- length(internal_participants)
    external_count <- length(all_ips) - internal_count
    cat("Соотношение участников:\n")
    Соотношение участников:
    cat("   Внутренних:", internal_count, "\n")
       Внутренних: 1267 
    cat("   Внешних:", external_count, "\n")
       Внешних: 92 
    cat("   Соотношение внутренних/внешних:", 
    +     round(internal_count/external_count, 2), "\n")
       Соотношение внутренних/внешних: 13.77 

### Найдите топ-10 участников сети, проявляющих наибольшую сетевую активность.

    top_active <- dns_data %>% filter(sapply(id.orig_h, is_internal_ip)) %>% count(id.orig_h, sort = TRUE) %>% head(10)
    cat(" Топ-10 самых активных участников:\n")
    Топ-10 самых активных участников:
    > print(top_active)
    # A tibble: 10 × 2
       id.orig_h           n
       <chr>           <int>
     1 10.10.117.210   75943
     2 192.168.202.93  26522
     3 192.168.202.103 18121
     4 192.168.202.76  16978
     5 192.168.202.97  16176
     6 192.168.202.141 14967
     7 10.10.117.209   14222
     8 192.168.202.110 13372
     9 192.168.203.63  12148
    10 192.168.202.106 10784

### Найдите топ-10 доменов, к которым обращаются пользователи сети и соответственное количество обращений

    top_domains <- dns_data %>% filter(!is.na(query), query != "", !grepl("^\\\\|\\*", query)) %>% count(query, sort = TRUE) %>% head(10)
    cat(" Топ-10 доменов :\n")
    Топ-10 доменов:
    > print(top_domains)
    # A tibble: 10 × 2
       query                               n
       <chr>                           <int>
     1 teredo.ipv6.microsoft.com       39273
     2 tools.google.com                14057
     3 www.apple.com                   13390
     4 time.apple.com                  13109
     5 safebrowsing.clients.google.com 11658
     6 WPAD                             9134
     7 44.206.168.192.in-addr.arpa      7248
     8 HPE8AA67                         6929
     9 ISATAP                           6569
    10 imap.gmail.com                   5543

### Опеределите базовые статистические характеристики (функция summary() ) интервала времени между последовательными обращениями к топ-10 доменам.

    analyze_intervals <- function(domain) {
    +     intervals <- dns_data %>%
    +         filter(query == domain) %>%
    +         arrange(ts) %>%
    +         mutate(diff = as.numeric(difftime(ts, lag(ts), units = "secs"))) %>%
    +         pull(diff) %>%
    +         na.omit()
    +     
    +     if (length(intervals) > 0) {
    +         return(summary(intervals))
    +     } else {
    +         return("Недостаточно данных")
    +     }
    + }
    > 
    > cat("Статистика интервалов для топ-3 доменов:\n")
    Статистика интервалов для топ-3 доменов:
    > for(i in 1:min(3, nrow(top_domains))) {
    +     cat("\nДомен:", top_domains$query[i], "\n")
    +     print(analyze_intervals(top_domains$query[i]))
    + }

    Домен: teredo.ipv6.microsoft.com 
         Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
        0.000     0.000     0.000     2.941     0.510 50387.760 

    Домен: tools.google.com 
         Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
        0.000     0.000     0.000     8.187     1.000 50364.830 

    Домен: www.apple.com 
         Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
        0.000     0.000     1.000     8.607     3.010 50963.630 
    > 

### Часто вредоносное программное обеспечение использует DNS канал в качестве канала управления, периодически отправляя запросы на подконтрольный злоумышленникам DNS сервер. По периодическим запросам на один и тот же домен можно выявить скрытый DNS канал. Есть ли такие IP адреса в исследуемом датасете?

    > suspicious_dns <- dns_data %>%
    +     filter(sapply(id.orig_h, is_internal_ip),
    +            !is.na(query), query != "",
    +            !grepl("^\\\\|\\*", query)) %>%
    +     group_by(id.orig_h, query) %>%
    +     summarise(
    +         request_count = n(),
    +         time_span = as.numeric(difftime(max(ts), min(ts), units = "secs")),
    +         avg_interval = ifelse(request_count > 1, 
    +                               time_span / (request_count - 1), 
    +                               NA),
    +         .groups = 'drop'
    +     ) %>%
    +     filter(request_count >= 5, 
    +            !is.na(avg_interval), 
    +            avg_interval > 0,
    +            avg_interval < 300) %>%  
    +     arrange(avg_interval)
    > 
    > cat("Подозрительные DNS-каналы (периодические запросы):\n")
    Подозрительные DNS-каналы (периодические запросы):
    > if(nrow(suspicious_dns) > 0) {
    +     print(head(suspicious_dns, 10))
    + } else {
    +     cat("Не обнаружено\n")
    + }
    # A tibble: 10 × 5
       id.orig_h       query                           request_count time_span avg_interval
       <chr>           <chr>                                   <int>     <dbl>        <dbl>
     1 192.168.202.103 fxfeeds.mozilla.com                        16   0.01000     0.000667
     2 192.168.202.79  www.iana.org                               16   0.01000     0.000667
     3 192.168.202.94  quickdraw.splunk.com                       16   0.01000     0.000667
     4 192.168.202.108 versioncheck.addons.mozilla.org            12   0.01000     0.000909
     5 192.168.202.138 www.midatlanticccdc.org                    16   0.0200      0.00133 
     6 192.168.203.61  addons.mozilla.org                         16   0.0200      0.00133 
     7 192.168.203.62  addons.mozilla.org                         16   0.0200      0.00133 
     8 192.168.204.70  de3.php.net                                16   0.0200      0.00133 
     9 192.168.202.112 filezilla.sourceforge.net                   8   0.01000     0.00143 
    10 192.168.202.112 php.net                                     8   0.01000     0.00143

### Определите местоположение (страну, город) и организацию-провайдера для топ-10 доменов. Для этого можно использовать сторонние сервисы, например http://ip-api.com (API-ндпоинт –http:/ /ip-api.com/json).

    > library(httr)
    > library(purrr)
    > 
    > get_geo_info <- function(ip) {
    +     tryCatch({
    +         if(grepl(":", ip)) return(NULL)  
    +         
    +         response <- GET(paste0("http://ip-api.com/json/", ip), 
    +                         timeout(5))
    +         if(status_code(response) == 200) {
    +             info <- content(response, "parsed")
    +             return(data.frame(
    +                 ip = ip,
    +                 country = ifelse(!is.null(info$country), info$country, NA),
    +                 city = ifelse(!is.null(info$city), info$city, NA),
    +                 isp = ifelse(!is.null(info$isp), info$isp, NA),
    +                 stringsAsFactors = FALSE
    +             ))
    +         }
    +     }, error = function(e) NULL)
    +     return(NULL)
    + }
    > 
    > external_ips <- dns_data %>%
    +     filter(!sapply(id.resp_h, is_internal_ip),
    +            !grepl(":", id.resp_h),
    +            id.resp_h != "255.255.255.255") %>%
    +     distinct(id.resp_h) %>%
    +     head(5)
    > 
    > cat("Геолокация для 5 внешних IP:\n")
    Геолокация для 5 внешних IP:
    > if(nrow(external_ips) > 0) {
    +     geo_results <- map_df(external_ips$id.resp_h, get_geo_info)
    +     if(nrow(geo_results) > 0) {
    +         print(geo_results)
    +     } else {
    +         cat("Не удалось получить геолокацию\n")
    +     }
    + } else {
    +     cat("Внешние IP не найдены\n")
    + }
                 ip       country     city                       isp
    1 156.154.70.22 United States New York Neustar Security Services
    2    8.26.56.26 United States  Clifton           Flexential Corp
    3 198.153.192.1        Canada  Toronto Neustar Security Services
    4 198.153.194.1 United States New York Neustar Security Services
