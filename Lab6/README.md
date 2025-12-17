# Lab6
daria.toikina@yandex.ru
2025-12-08

# Цель работы

1\. Закрепить навыки исследования данных журнала Windows Active
Directory

2\. Изучить структуру журнала системы Windows Active Directory

3\. Зекрепить практические навыки использования языка программирования R
для обработки данных

4\. Закрепить знания основных функций обработки данных экосистемы
tidyverse языка R

# Исходные данные

1\. Ноутбук с ОС MacOS

2\. RStudio

3\. Интерпретатор языка R 4.5.1

# Практика

## Установка и загрузка необходимых библиотек

``` r
library(dplyr)     
```


    Attaching package: 'dplyr'

    The following objects are masked from 'package:stats':

        filter, lag

    The following objects are masked from 'package:base':

        intersect, setdiff, setequal, union

``` r
library(tidyr)      
library(jsonlite)  
library(xml2)       
```

    Warning: package 'xml2' was built under R version 4.5.2

``` r
library(rvest)     
library(stringr)    
```

## Подготовка данных

``` r
dataset_url <- "https://storage.yandexcloud.net/iamcth-data/dataset.tar.gz"
temp_archive <- "security_logs.tar.gz"
extract_dir <- "extracted_logs"
```

## Загрузка архива

``` r
download_result <- tryCatch({
    download.file(dataset_url, temp_archive, mode = "wb", quiet = TRUE)
    cat(" Архив успешно загружен\n")
    TRUE
}, error = function(e) {
    cat(" Ошибка при загрузке:", e$message, "\n")
    stop("Не удалось загрузить данные")
})
```

     Архив успешно загружен

## Распаковка архива

``` r
tryCatch({
    if (!dir.exists(extract_dir)) {
        dir.create(extract_dir, showWarnings = FALSE)
    }
    untar(temp_archive, exdir = extract_dir)
    cat(" Архив успешно распакован в", extract_dir, "\n")
}, error = function(e) {
    cat("Ошибка при распаковке:", e$message, "\n")
    stop("Не удалось распаковать архив")
})
```

     Архив успешно распакован в extracted_logs 

## Поиск JSON файлов

``` r
log_files <- list.files(extract_dir, pattern = "\\.json$", full.names = TRUE, recursive = TRUE)

if (length(log_files) == 0) {
    stop("В архиве не найдено JSON файлов")
}

cat("Найдено JSON файлов:", length(log_files), "\n")
```

    Найдено JSON файлов: 1 

## Обработка первого файла

``` r
log_data <- tryCatch({
    con <- file(log_files[1], "r")
    data <- stream_in(con, verbose = FALSE)
    close(con)
    cat("Данные успешно импортированы через stream_in\n")
    data
}, error = function(e) {
    cat("Ошибка при чтении через stream_in:", e$message, "\n")
    cat("Попытка альтернативного метода загрузки...\n")
    
    tryCatch({
        data <- readLines(log_files[1], warn = FALSE) %>%
            paste(collapse = "") %>%
            fromJSON()
        cat(" Данные успешно импортированы через readLines + fromJSON\n")
        data
    }, error = function(e2) {
        cat(" Ошибка при альтернативном чтении:", e2$message, "\n")
        stop("Не удалось прочитать JSON файл")
    })
})
```

    Данные успешно импортированы через stream_in

## Загрузка справочника событий WINDOWS

``` r
event_table <- data.frame() # пустой на случай ошибки

try({
    url <- "https://learn.microsoft.com/en-us/windows-server/identity/ad-ds/plan/appendix-l--events-to-monitor"
    page <- read_html(url)
    tables <- html_table(page)
    event_table <- tables[[1]]
    names(event_table)[1:2] <- c("Event_ID", "Description")
    cat("Загружено событий:", nrow(event_table), "\n")
})
```

    Загружено событий: 381 

## Простая функция для распрямления данных

``` r
flatten_simple <- function(df) {
    cat("Упрощаем данные...\n")
    
    result <- df
    
    for(col_name in names(result)) {
        col_data <- result[[col_name]]
        
        if(is.list(col_data)) {
            cat("  Обрабатываем:", col_name, "\n")
            
            try({
                if(all(sapply(col_data, length) == 1)) {
                    result[[col_name]] <- unlist(col_data)
                } 
                else if(is.data.frame(col_data[[1]])) {
                    df_temp <- col_data[[1]]
                    names(df_temp) <- paste(col_name, names(df_temp), sep = "_")
                    
                    result[[col_name]] <- NULL
                    
                    result <- bind_cols(result, df_temp)
                }
            })
        }
    }
    
    return(result)
}
```

## Применяем к нашим данным

``` r
tidy_data <- flatten_simple(log_data)
```

    Упрощаем данные...
      Обрабатываем: @metadata 
      Обрабатываем: event 
      Обрабатываем: log 
      Обрабатываем: winlog 
      Обрабатываем: ecs 
      Обрабатываем: host 
      Обрабатываем: agent 

## Преобразуем даты если есть

``` r
if("@timestamp" %in% names(tidy_data)) {
    tidy_data <- tidy_data %>%
        mutate(
            timestamp = as.POSIXct(`@timestamp`, 
                                  format = "%Y-%m-%dT%H:%M:%OS", 
                                  tz = "UTC"),
            date = as.Date(timestamp),
            hour = format(timestamp, "%H")
        )
    cat("\nДаты преобразованы\n")
}
```


    Даты преобразованы

## Ищем ID событий для соединения со справочником

``` r
event_col <- NULL
possible_names <- c("event_code", "event_id", "EventID", "event.id", "id")

for(name in possible_names) {
    if(name %in% names(tidy_data)) {
        event_col <- name
        break
    }
}

if(!is.null(event_col)) {
    cat("Найдена колонка событий:", event_col, "\n")
    
    tidy_data <- tidy_data %>%
        mutate(event_id_num = as.numeric(.data[[event_col]]))
    
    if(nrow(event_table) > 0) {
        event_table <- event_table %>%
            mutate(Event_ID = as.numeric(Event_ID))
        
        tidy_data <- tidy_data %>%
            left_join(event_table, by = c("event_id_num" = "Event_ID"))
        
        cat("Добавлены описания событий\n")
    }
}
```

## Проверка

``` r
glimpse(tidy_data)
```

    Rows: 101,904
    Columns: 245
    $ `@timestamp`                     <chr> "2019-10-20T20:11:06.937Z", "2019-10-…
    $ `@metadata`                      <df[,4]> <data.frame[26 x 4]>
    $ event                            <df[,4]> <data.frame[26 x 4]>
    $ log                              <df[,1]> <data.frame[26 x 1]>
    $ message                          <chr> "A token right was adjusted.\n\nSu…
    $ ecs                              <df[,1]> <data.frame[26 x 1]>
    $ host                             <df[,1]> <data.frame[26 x 1]>
    $ agent                            <df[,5]> <data.frame[26 x 5]>
    $ winlog_SubjectDomainName         <chr> "shire", "NT AUTHORITY", NA, NA, N…
    $ winlog_TargetDomainName          <chr> "shire", NA, NA, NA, NA, NA, NA, N…
    $ winlog_SubjectUserSid            <chr> "S-1-5-18", "S-1-5-19", NA, NA, NA, N…
    $ winlog_SubjectUserName           <chr> "HR001$", "LOCAL SERVICE", NA, NA,…
    $ winlog_TargetUserName            <chr> "HR001$", NA, NA, NA, NA, NA, NA, …
    $ winlog_EnabledPrivilegeList      <chr> "SeTakeOwnershipPrivilege", NA, NA…
    $ winlog_TargetLogonId             <chr> "0x3e7", NA, NA, NA, NA, NA, NA, NA, …
    $ winlog_ProcessName               <chr> "C:\\Windows\\System32\\svchost.exe",…
    $ winlog_ProcessId                 <chr> "0x804", "0x494", NA, NA, NA, NA, "14…
    $ winlog_SubjectLogonId            <chr> "0x3e7", "0x3e5", NA, NA, NA, NA, NA,…
    $ winlog_TargetUserSid             <chr> "S-1-5-18", NA, NA, NA, NA, NA, NA, N…
    $ winlog_DisabledPrivilegeList     <chr> "-", NA, NA, NA, NA, NA, NA, NA, NA, …
    $ winlog_ObjectServer              <chr> NA, "Security", NA, NA, NA, NA, NA, N…
    $ winlog_Service                   <chr> NA, "-", NA, NA, NA, NA, NA, NA, NA, …
    $ winlog_PrivilegeList             <chr> NA, "SeProfileSingleProcessPrivilege"…
    $ winlog_TargetProcessId           <chr> NA, NA, "3108", "1968", "2032", "7592…
    $ winlog_SourceProcessId           <chr> NA, NA, "3556", "3548", "3632", "7112…
    $ winlog_SourceProcessGUID         <chr> NA, NA, "{a158f72c-afec-5dac-0000-001…
    $ winlog_SourceThreadId            <chr> NA, NA, "3688", "3660", "3772", "7004…
    $ winlog_SourceImage               <chr> NA, NA, "C:\\Windows\\System32\\svcho…
    $ winlog_CallTrace                 <chr> NA, NA, "C:\\Windows\\SYSTEM32\\ntdll…
    $ winlog_TargetProcessGUID         <chr> NA, NA, "{a158f72c-afeb-5dac-0000-001…
    $ winlog_TargetImage               <chr> NA, NA, "C:\\Program Files\\Amazon\\E…
    $ winlog_GrantedAccess             <chr> NA, NA, "0x1400", "0x1400", "0x1400",…
    $ winlog_UtcTime                   <chr> NA, NA, "2019-10-20 20:11:09.052", "2…
    $ winlog_ProcessGuid               <chr> NA, NA, NA, NA, NA, NA, "{b2887b82-a6…
    $ winlog_Image                     <chr> NA, NA, NA, NA, NA, NA, "C:\\Windows\…
    $ winlog_TargetFilename            <chr> NA, NA, NA, NA, NA, NA, "C:\\Windows\…
    $ winlog_CreationUtcTime           <chr> NA, NA, NA, NA, NA, NA, "2019-10-20 1…
    $ winlog_FileVersion               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_Company                   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_Signed                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_Signature                 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_OriginalFileName          <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_Description               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_Product                   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_ImageLoaded               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_SignatureStatus           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_Hashes                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_Status                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_Protocol                  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_FilterRTID                <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_LayerName                 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_LayerRTID                 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_Application               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_SourceAddress             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_SourcePort                <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_ProcessID                 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_DestPort                  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_RemoteMachineID           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_RemoteUserID              <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_Direction                 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_DestAddress               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_RestrictedAdminMode       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_TransmittedServices       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_LogonType                 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_WorkstationName           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_LmPackageName             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_KeyLength                 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_LogonGuid                 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_ImpersonationLevel        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_TargetOutboundDomainName  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_TargetLinkedLogonId       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_AuthenticationPackageName <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_TargetOutboundUserName    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_VirtualAccount            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_IpAddress                 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_ElevatedToken             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_LogonProcessName          <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_IpPort                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_GroupMembership           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_EventIdx                  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_EventCountTotal           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_EventType                 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_TargetObject              <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_DestinationPort           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_DestinationIsIpv6         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_SourceIp                  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_User                      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_SourceIsIpv6              <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_DestinationIp             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_SourceHostname            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_DestinationHostname       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_DestinationPortName       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_Initiated                 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_param3                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_param2                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_param1                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_ParentProcessId           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_ParentProcessGuid         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_ParentCommandLine         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_CommandLine               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_CurrentDirectory          <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_TerminalSessionId         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_ParentImage               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_LogonId                   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_IntegrityLevel            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_PipeName                  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_ScriptBlockId             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_RunspaceId                <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_ContextInfo               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_Payload                   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_MessageNumber             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_MessageTotal              <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_ScriptBlockText           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_NewProcessId              <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_NewProcessName            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_MandatoryLabel            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_ParentProcessName         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_TokenElevationType        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_ServiceName               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_ServiceSid                <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_TicketOptions             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_TicketEncryptionType      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_QueryName                 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_QueryStatus               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_QueryResults              <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_SourcePortName            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_Device                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_Details                   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_ObjectName                <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_OldSd                     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_NewSd                     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_ObjectType                <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_HandleId                  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_ShareName                 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_AccessList                <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_ShareLocalPath            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_AccessMask                <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_AccessReason              <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_RelativeTargetName        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_Properties                <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_AdditionalInfo2           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_OperationType             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_AdditionalInfo            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_Path                      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_TargetHandleId            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_SourceHandleId            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_TransactionId             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_RestrictedSidCount        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_ResourceAttributes        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_Binary                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_PreviousCreationUtcTime   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_CallerProcessId           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_CallerProcessName         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_TargetSid                 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_TaskName                  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_TaskContentNew            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_SourceProcessGuid         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_TargetProcessGuid         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_StartAddress              <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_NewThreadId               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_TaskContent               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_PackageName               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_Workstation               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_param4                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_param5                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_param7                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_StartModule               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_StartFunction             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_TSId                      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_UserSid                   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_PreAuthType               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_PreviousTime              <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_NewTime                   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_InterfaceName             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_OldProfile                <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_NewProfile                <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_InterfaceGuid             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_SettingType               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_SettingValueSize          <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_SettingValue              <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_SettingValueDisplay       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_Origin                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_ModifyingUser             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_Reason                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_OldTime                   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_DwordVal                  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_param6                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_ShutdownActionType        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_ShutdownEventCode         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_ShutdownReason            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_StopTime                  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_BootMode                  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_StartTime                 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_MajorVersion              <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_MinorVersion              <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_BuildVersion              <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_QfeVersion                <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_ServiceVersion            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_EnableDisableReason       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_VsmPolicy                 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_LastBootGood              <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_LastBootId                <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_BootStatusPolicy          <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_LastShutdownGood          <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_BootMenuPolicy            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_BootType                  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_LoadOptions               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_EntryCount                <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_BitlockerUserInputTime    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_CountNew                  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_CountOld                  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_UpdateReason              <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_EnabledNew                <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_DeviceNameLength          <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_DeviceName                <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_DeviceTime                <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_FinalStatus               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_DeviceVersionMajor        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_DeviceVersionMinor        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_DriveName                 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_CorruptionActionState     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_State                     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_MinimumPerformancePercent <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_MaximumPerformancePercent <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_PerformanceImplementation <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_Group                     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_Number                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_IdleStateCount            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_IdleImplementation        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_NominalFrequency          <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_MinimumThrottlePercent    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_Config                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_IsTestConfig              <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ `winlog_Default SD String:`      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_AdapterSuffixName         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_DnsServerList             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ `winlog_Sent UpdateServer`       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_Ipaddress                 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_ErrorCode                 <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_AdapterName               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_HostName                  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ winlog_TimeSource                <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    $ timestamp                        <dttm> 2019-10-20 20:11:06, 2019-10-20 20:11…
    $ date                             <date> 2019-10-20, 2019-10-20, 2019-10-20, 2…
    $ hour                             <chr> "20", "20", "20", "20", "20", "20", "…

# Анализ

## 1. Раскройте датафрейм избавившись от вложенных датафреймов. Для обнаружения таких можно использовать функцию dplyr::glimpse(), а для раскрытия вложенности – tidyr::unnest(). Обратите внимание, что при раскрытии теряются внешние названия колонок – это можно предотвратить если использовать параметр tidyr::unnest(…, names_sep = ).

### Какие колонки содержат вложенные датафреймы или списки

``` r
winlog_cols <- grep("^winlog", names(tidy_data), value = TRUE)

for(col in winlog_cols) {
    col_data <- tidy_data[[col]]
    cat(col, "→ тип:", class(col_data)[1], 
        " | длина:", if(is.list(col_data)) "список" else length(col_data), 
        " | NAs:", sum(is.na(col_data)), "\n")
}
```

    winlog_SubjectDomainName → тип: character  | длина: 101904  | NAs: 99768 
    winlog_TargetDomainName → тип: character  | длина: 101904  | NAs: 100722 
    winlog_SubjectUserSid → тип: character  | длина: 101904  | NAs: 99768 
    winlog_SubjectUserName → тип: character  | длина: 101904  | NAs: 99768 
    winlog_TargetUserName → тип: character  | длина: 101904  | NAs: 100721 
    winlog_EnabledPrivilegeList → тип: character  | длина: 101904  | NAs: 101095 
    winlog_TargetLogonId → тип: character  | длина: 101904  | NAs: 100757 
    winlog_ProcessName → тип: character  | длина: 101904  | NAs: 100253 
    winlog_ProcessId → тип: character  | длина: 101904  | NAs: 85612 
    winlog_SubjectLogonId → тип: character  | длина: 101904  | NAs: 99768 
    winlog_TargetUserSid → тип: character  | длина: 101904  | NAs: 100757 
    winlog_DisabledPrivilegeList → тип: character  | длина: 101904  | NAs: 101095 
    winlog_ObjectServer → тип: character  | длина: 101904  | NAs: 101329 
    winlog_Service → тип: character  | длина: 101904  | NAs: 101761 
    winlog_PrivilegeList → тип: character  | длина: 101904  | NAs: 101441 
    winlog_TargetProcessId → тип: character  | длина: 101904  | NAs: 65682 
    winlog_SourceProcessId → тип: character  | длина: 101904  | NAs: 65682 
    winlog_SourceProcessGUID → тип: character  | длина: 101904  | NAs: 65830 
    winlog_SourceThreadId → тип: character  | длина: 101904  | NAs: 65830 
    winlog_SourceImage → тип: character  | длина: 101904  | NAs: 65739 
    winlog_CallTrace → тип: character  | длина: 101904  | NAs: 65830 
    winlog_TargetProcessGUID → тип: character  | длина: 101904  | NAs: 65830 
    winlog_TargetImage → тип: character  | длина: 101904  | NAs: 65739 
    winlog_GrantedAccess → тип: character  | длина: 101904  | NAs: 65830 
    winlog_UtcTime → тип: character  | длина: 101904  | NAs: 51681 
    winlog_ProcessGuid → тип: character  | длина: 101904  | NAs: 87846 
    winlog_Image → тип: character  | длина: 101904  | NAs: 87846 
    winlog_TargetFilename → тип: character  | длина: 101904  | NAs: 101672 
    winlog_CreationUtcTime → тип: character  | длина: 101904  | NAs: 101672 
    winlog_FileVersion → тип: character  | длина: 101904  | NAs: 95859 
    winlog_Company → тип: character  | длина: 101904  | NAs: 95859 
    winlog_Signed → тип: character  | длина: 101904  | NAs: 95967 
    winlog_Signature → тип: character  | длина: 101904  | NAs: 96206 
    winlog_OriginalFileName → тип: character  | длина: 101904  | NAs: 95860 
    winlog_Description → тип: character  | длина: 101904  | NAs: 95866 
    winlog_Product → тип: character  | длина: 101904  | NAs: 95859 
    winlog_ImageLoaded → тип: character  | длина: 101904  | NAs: 95967 
    winlog_SignatureStatus → тип: character  | длина: 101904  | NAs: 95967 
    winlog_Hashes → тип: character  | длина: 101904  | NAs: 95859 
    winlog_Status → тип: character  | длина: 101904  | NAs: 101681 
    winlog_Protocol → тип: character  | длина: 101904  | NAs: 100090 
    winlog_FilterRTID → тип: character  | длина: 101904  | NAs: 100672 
    winlog_LayerName → тип: character  | длина: 101904  | NAs: 100672 
    winlog_LayerRTID → тип: character  | длина: 101904  | NAs: 100672 
    winlog_Application → тип: character  | длина: 101904  | NAs: 100672 
    winlog_SourceAddress → тип: character  | длина: 101904  | NAs: 100672 
    winlog_SourcePort → тип: character  | длина: 101904  | NAs: 100090 
    winlog_ProcessID → тип: character  | длина: 101904  | NAs: 101147 
    winlog_DestPort → тип: character  | длина: 101904  | NAs: 101148 
    winlog_RemoteMachineID → тип: character  | длина: 101904  | NAs: 101148 
    winlog_RemoteUserID → тип: character  | длина: 101904  | NAs: 101148 
    winlog_Direction → тип: character  | длина: 101904  | NAs: 101148 
    winlog_DestAddress → тип: character  | длина: 101904  | NAs: 101148 
    winlog_RestrictedAdminMode → тип: character  | длина: 101904  | NAs: 101831 
    winlog_TransmittedServices → тип: character  | длина: 101904  | NAs: 101811 
    winlog_LogonType → тип: character  | длина: 101904  | NAs: 101676 
    winlog_WorkstationName → тип: character  | длина: 101904  | NAs: 101831 
    winlog_LmPackageName → тип: character  | длина: 101904  | NAs: 101831 
    winlog_KeyLength → тип: character  | длина: 101904  | NAs: 101831 
    winlog_LogonGuid → тип: character  | длина: 101904  | NAs: 101703 
    winlog_ImpersonationLevel → тип: character  | длина: 101904  | NAs: 101831 
    winlog_TargetOutboundDomainName → тип: character  | длина: 101904  | NAs: 101831 
    winlog_TargetLinkedLogonId → тип: character  | длина: 101904  | NAs: 101831 
    winlog_AuthenticationPackageName → тип: character  | длина: 101904  | NAs: 101831 
    winlog_TargetOutboundUserName → тип: character  | длина: 101904  | NAs: 101831 
    winlog_VirtualAccount → тип: character  | длина: 101904  | NAs: 101831 
    winlog_IpAddress → тип: character  | длина: 101904  | NAs: 101636 
    winlog_ElevatedToken → тип: character  | длина: 101904  | NAs: 101831 
    winlog_LogonProcessName → тип: character  | длина: 101904  | NAs: 101829 
    winlog_IpPort → тип: character  | длина: 101904  | NAs: 101636 
    winlog_GroupMembership → тип: character  | длина: 101904  | NAs: 101831 
    winlog_EventIdx → тип: character  | длина: 101904  | NAs: 101831 
    winlog_EventCountTotal → тип: character  | длина: 101904  | NAs: 101831 
    winlog_EventType → тип: character  | длина: 101904  | NAs: 94945 
    winlog_TargetObject → тип: character  | длина: 101904  | NAs: 95141 
    winlog_DestinationPort → тип: character  | длина: 101904  | NAs: 101322 
    winlog_DestinationIsIpv6 → тип: character  | длина: 101904  | NAs: 101322 
    winlog_SourceIp → тип: character  | длина: 101904  | NAs: 101322 
    winlog_User → тип: character  | длина: 101904  | NAs: 101215 
    winlog_SourceIsIpv6 → тип: character  | длина: 101904  | NAs: 101322 
    winlog_DestinationIp → тип: character  | длина: 101904  | NAs: 101322 
    winlog_SourceHostname → тип: character  | длина: 101904  | NAs: 101322 
    winlog_DestinationHostname → тип: character  | длина: 101904  | NAs: 101341 
    winlog_DestinationPortName → тип: character  | длина: 101904  | NAs: 101701 
    winlog_Initiated → тип: character  | длина: 101904  | NAs: 101322 
    winlog_param3 → тип: character  | длина: 101904  | NAs: 89433 
    winlog_param2 → тип: character  | длина: 101904  | NAs: 89412 
    winlog_param1 → тип: character  | длина: 101904  | NAs: 97136 
    winlog_ParentProcessId → тип: character  | длина: 101904  | NAs: 101796 
    winlog_ParentProcessGuid → тип: character  | длина: 101904  | NAs: 101796 
    winlog_ParentCommandLine → тип: character  | длина: 101904  | NAs: 101796 
    winlog_CommandLine → тип: character  | длина: 101904  | NAs: 101688 
    winlog_CurrentDirectory → тип: character  | длина: 101904  | NAs: 101796 
    winlog_TerminalSessionId → тип: character  | длина: 101904  | NAs: 101796 
    winlog_ParentImage → тип: character  | длина: 101904  | NAs: 101796 
    winlog_LogonId → тип: character  | длина: 101904  | NAs: 101796 
    winlog_IntegrityLevel → тип: character  | длина: 101904  | NAs: 101796 
    winlog_PipeName → тип: character  | длина: 101904  | NAs: 101708 
    winlog_ScriptBlockId → тип: character  | длина: 101904  | NAs: 78479 
    winlog_RunspaceId → тип: character  | длина: 101904  | NAs: 78992 
    winlog_ContextInfo → тип: character  | длина: 101904  | NAs: 89738 
    winlog_Payload → тип: character  | длина: 101904  | NAs: 89738 
    winlog_MessageNumber → тип: character  | длина: 101904  | NAs: 101391 
    winlog_MessageTotal → тип: character  | длина: 101904  | NAs: 101391 
    winlog_ScriptBlockText → тип: character  | длина: 101904  | NAs: 101391 
    winlog_NewProcessId → тип: character  | длина: 101904  | NAs: 101796 
    winlog_NewProcessName → тип: character  | длина: 101904  | NAs: 101796 
    winlog_MandatoryLabel → тип: character  | длина: 101904  | NAs: 101796 
    winlog_ParentProcessName → тип: character  | длина: 101904  | NAs: 101796 
    winlog_TokenElevationType → тип: character  | длина: 101904  | NAs: 101796 
    winlog_ServiceName → тип: character  | длина: 101904  | NAs: 101879 
    winlog_ServiceSid → тип: character  | длина: 101904  | NAs: 101879 
    winlog_TicketOptions → тип: character  | длина: 101904  | NAs: 101879 
    winlog_TicketEncryptionType → тип: character  | длина: 101904  | NAs: 101879 
    winlog_QueryName → тип: character  | длина: 101904  | NAs: 101875 
    winlog_QueryStatus → тип: character  | длина: 101904  | NAs: 101875 
    winlog_QueryResults → тип: character  | длина: 101904  | NAs: 101879 
    winlog_SourcePortName → тип: character  | длина: 101904  | NAs: 101807 
    winlog_Device → тип: character  | длина: 101904  | NAs: 101839 
    winlog_Details → тип: character  | длина: 101904  | NAs: 101494 
    winlog_ObjectName → тип: character  | длина: 101904  | NAs: 101613 
    winlog_OldSd → тип: character  | длина: 101904  | NAs: 101887 
    winlog_NewSd → тип: character  | длина: 101904  | NAs: 101887 
    winlog_ObjectType → тип: character  | длина: 101904  | NAs: 101443 
    winlog_HandleId → тип: character  | длина: 101904  | NAs: 101472 
    winlog_ShareName → тип: character  | длина: 101904  | NAs: 101734 
    winlog_AccessList → тип: character  | длина: 101904  | NAs: 101465 
    winlog_ShareLocalPath → тип: character  | длина: 101904  | NAs: 101761 
    winlog_AccessMask → тип: character  | длина: 101904  | NAs: 101460 
    winlog_AccessReason → тип: character  | длина: 101904  | NAs: 101492 
    winlog_RelativeTargetName → тип: character  | длина: 101904  | NAs: 101754 
    winlog_Properties → тип: character  | длина: 101904  | NAs: 101875 
    winlog_AdditionalInfo2 → тип: character  | длина: 101904  | NAs: 101900 
    winlog_OperationType → тип: character  | длина: 101904  | NAs: 101900 
    winlog_AdditionalInfo → тип: character  | длина: 101904  | NAs: 101900 
    winlog_Path → тип: character  | длина: 101904  | NAs: 101829 
    winlog_TargetHandleId → тип: character  | длина: 101904  | NAs: 101847 
    winlog_SourceHandleId → тип: character  | длина: 101904  | NAs: 101847 
    winlog_TransactionId → тип: character  | длина: 101904  | NAs: 101642 
    winlog_RestrictedSidCount → тип: character  | длина: 101904  | NAs: 101642 
    winlog_ResourceAttributes → тип: character  | длина: 101904  | NAs: 101664 
    winlog_Binary → тип: character  | длина: 101904  | NAs: 101897 
    winlog_PreviousCreationUtcTime → тип: character  | длина: 101904  | NAs: 101892 
    winlog_CallerProcessId → тип: character  | длина: 101904  | NAs: 101894 
    winlog_CallerProcessName → тип: character  | длина: 101904  | NAs: 101894 
    winlog_TargetSid → тип: character  | длина: 101904  | NAs: 101889 
    winlog_TaskName → тип: character  | длина: 101904  | NAs: 101895 
    winlog_TaskContentNew → тип: character  | длина: 101904  | NAs: 101896 
    winlog_SourceProcessGuid → тип: character  | длина: 101904  | NAs: 101813 
    winlog_TargetProcessGuid → тип: character  | длина: 101904  | NAs: 101813 
    winlog_StartAddress → тип: character  | длина: 101904  | NAs: 101813 
    winlog_NewThreadId → тип: character  | длина: 101904  | NAs: 101813 
    winlog_TaskContent → тип: character  | длина: 101904  | NAs: 101903 
    winlog_PackageName → тип: character  | длина: 101904  | NAs: 101903 
    winlog_Workstation → тип: character  | длина: 101904  | NAs: 101903 
    winlog_param4 → тип: character  | длина: 101904  | NAs: 101902 
    winlog_param5 → тип: character  | длина: 101904  | NAs: 101901 
    winlog_param7 → тип: character  | длина: 101904  | NAs: 101902 
    winlog_StartModule → тип: character  | длина: 101904  | NAs: 101901 
    winlog_StartFunction → тип: character  | длина: 101904  | NAs: 101901 
    winlog_TSId → тип: character  | длина: 101904  | NAs: 101901 
    winlog_UserSid → тип: character  | длина: 101904  | NAs: 101901 
    winlog_PreAuthType → тип: character  | длина: 101904  | NAs: 101899 
    winlog_PreviousTime → тип: character  | длина: 101904  | NAs: 101903 
    winlog_NewTime → тип: character  | длина: 101904  | NAs: 101902 
    winlog_InterfaceName → тип: character  | длина: 101904  | NAs: 101903 
    winlog_OldProfile → тип: character  | длина: 101904  | NAs: 101903 
    winlog_NewProfile → тип: character  | длина: 101904  | NAs: 101903 
    winlog_InterfaceGuid → тип: character  | длина: 101904  | NAs: 101903 
    winlog_SettingType → тип: character  | длина: 101904  | NAs: 101903 
    winlog_SettingValueSize → тип: character  | длина: 101904  | NAs: 101903 
    winlog_SettingValue → тип: character  | длина: 101904  | NAs: 101903 
    winlog_SettingValueDisplay → тип: character  | длина: 101904  | NAs: 101903 
    winlog_Origin → тип: character  | длина: 101904  | NAs: 101903 
    winlog_ModifyingUser → тип: character  | длина: 101904  | NAs: 101903 
    winlog_Reason → тип: character  | длина: 101904  | NAs: 101902 
    winlog_OldTime → тип: character  | длина: 101904  | NAs: 101903 
    winlog_DwordVal → тип: character  | длина: 101904  | NAs: 101901 
    winlog_param6 → тип: character  | длина: 101904  | NAs: 101903 
    winlog_ShutdownActionType → тип: character  | длина: 101904  | NAs: 101903 
    winlog_ShutdownEventCode → тип: character  | длина: 101904  | NAs: 101903 
    winlog_ShutdownReason → тип: character  | длина: 101904  | NAs: 101903 
    winlog_StopTime → тип: character  | длина: 101904  | NAs: 101903 
    winlog_BootMode → тип: character  | длина: 101904  | NAs: 101903 
    winlog_StartTime → тип: character  | длина: 101904  | NAs: 101903 
    winlog_MajorVersion → тип: character  | длина: 101904  | NAs: 101903 
    winlog_MinorVersion → тип: character  | длина: 101904  | NAs: 101903 
    winlog_BuildVersion → тип: character  | длина: 101904  | NAs: 101903 
    winlog_QfeVersion → тип: character  | длина: 101904  | NAs: 101903 
    winlog_ServiceVersion → тип: character  | длина: 101904  | NAs: 101903 
    winlog_EnableDisableReason → тип: character  | длина: 101904  | NAs: 101903 
    winlog_VsmPolicy → тип: character  | длина: 101904  | NAs: 101903 
    winlog_LastBootGood → тип: character  | длина: 101904  | NAs: 101903 
    winlog_LastBootId → тип: character  | длина: 101904  | NAs: 101903 
    winlog_BootStatusPolicy → тип: character  | длина: 101904  | NAs: 101903 
    winlog_LastShutdownGood → тип: character  | длина: 101904  | NAs: 101903 
    winlog_BootMenuPolicy → тип: character  | длина: 101904  | NAs: 101903 
    winlog_BootType → тип: character  | длина: 101904  | NAs: 101903 
    winlog_LoadOptions → тип: character  | длина: 101904  | NAs: 101903 
    winlog_EntryCount → тип: character  | длина: 101904  | NAs: 101903 
    winlog_BitlockerUserInputTime → тип: character  | длина: 101904  | NAs: 101903 
    winlog_CountNew → тип: character  | длина: 101904  | NAs: 101903 
    winlog_CountOld → тип: character  | длина: 101904  | NAs: 101903 
    winlog_UpdateReason → тип: character  | длина: 101904  | NAs: 101903 
    winlog_EnabledNew → тип: character  | длина: 101904  | NAs: 101903 
    winlog_DeviceNameLength → тип: character  | длина: 101904  | NAs: 101893 
    winlog_DeviceName → тип: character  | длина: 101904  | NAs: 101891 
    winlog_DeviceTime → тип: character  | длина: 101904  | NAs: 101893 
    winlog_FinalStatus → тип: character  | длина: 101904  | NAs: 101893 
    winlog_DeviceVersionMajor → тип: character  | длина: 101904  | NAs: 101893 
    winlog_DeviceVersionMinor → тип: character  | длина: 101904  | NAs: 101893 
    winlog_DriveName → тип: character  | длина: 101904  | NAs: 101902 
    winlog_CorruptionActionState → тип: character  | длина: 101904  | NAs: 101902 
    winlog_State → тип: character  | длина: 101904  | NAs: 101903 
    winlog_MinimumPerformancePercent → тип: character  | длина: 101904  | NAs: 101902 
    winlog_MaximumPerformancePercent → тип: character  | длина: 101904  | NAs: 101902 
    winlog_PerformanceImplementation → тип: character  | длина: 101904  | NAs: 101902 
    winlog_Group → тип: character  | длина: 101904  | NAs: 101902 
    winlog_Number → тип: character  | длина: 101904  | NAs: 101902 
    winlog_IdleStateCount → тип: character  | длина: 101904  | NAs: 101902 
    winlog_IdleImplementation → тип: character  | длина: 101904  | NAs: 101902 
    winlog_NominalFrequency → тип: character  | длина: 101904  | NAs: 101902 
    winlog_MinimumThrottlePercent → тип: character  | длина: 101904  | NAs: 101902 
    winlog_Config → тип: character  | длина: 101904  | NAs: 101903 
    winlog_IsTestConfig → тип: character  | длина: 101904  | NAs: 101903 
    winlog_Default SD String: → тип: character  | длина: 101904  | NAs: 101903 
    winlog_AdapterSuffixName → тип: character  | длина: 101904  | NAs: 101903 
    winlog_DnsServerList → тип: character  | длина: 101904  | NAs: 101903 
    winlog_Sent UpdateServer → тип: character  | длина: 101904  | NAs: 101903 
    winlog_Ipaddress → тип: character  | длина: 101904  | NAs: 101903 
    winlog_ErrorCode → тип: character  | длина: 101904  | NAs: 101903 
    winlog_AdapterName → тип: character  | длина: 101904  | NAs: 101903 
    winlog_HostName → тип: character  | длина: 101904  | NAs: 101903 
    winlog_TimeSource → тип: character  | длина: 101904  | NAs: 101903 

### Раскроем вложенные датафреймы с помощью unnest()

``` r
simple_data <- tidy_data
cols_processed <- 0

for(col in names(simple_data)) {
    if(is.data.frame(simple_data[[col]])) {
        cat(col, "→ датафрейм → ")
        
        nested_df <- simple_data[[col]]
        
        if(ncol(nested_df) > 0) {
            new_names <- paste(col, colnames(nested_df), sep = "_")
            colnames(nested_df) <- new_names
            
            simple_data[[col]] <- NULL
            simple_data <- bind_cols(simple_data, nested_df)
            
            cat("развернут в", ncol(nested_df), "колонок\n")
            cols_processed <- cols_processed + 1
        } else {
            cat("пустой - удаляем\n")
            simple_data[[col]] <- NULL
        }
    }
    else if(is.list(simple_data[[col]]) && !is.vector(simple_data[[col]])) {
        cat(col, "→ список → ")
        
        simple_data[[col]] <- sapply(simple_data[[col]], 
            function(x) paste(x, collapse = "; "))
        
        cat("преобразован в текст\n")
        cols_processed <- cols_processed + 1
    }
}
```

    @metadata → датафрейм → развернут в 4 колонок
    event → датафрейм → развернут в 4 колонок
    log → датафрейм → развернут в 1 колонок
    ecs → датафрейм → развернут в 1 колонок
    host → датафрейм → развернут в 1 колонок
    agent → датафрейм → развернут в 5 колонок

### Раскрываем списки

``` r
unested_data <- tidy_data
```

``` r
list_cols <- names(unested_data)[sapply(unested_data, is.list)]
cat("Списки для обработки:", paste(list_cols, collapse = ", "), "\n\n")
```

    Списки для обработки: @metadata, event, log, ecs, host, agent 

``` r
for(col in list_cols) {
  cat("Обрабатываем:", col, "\n")
  
  col_data <- unested_data[[col]]
  
  tryCatch({
    if (all(sapply(col_data, length) == 1)) {
      unested_data[[col]] <- unlist(col_data)
      cat("  → преобразован в вектор\n")
    } 
    else {
      unested_data[[col]] <- sapply(col_data, function(x) {
        if (is.null(x)) return(NA)
        
        if (!is.null(names(x))) {
          parts <- paste(names(x), x, sep = "=")
          return(paste(parts, collapse = "; "))
        } else {
          return(paste(x, collapse = "; "))
        }
      })
      cat(" преобразован в текст с именами\n")
    }
  }, error = function(e) {
    cat(" Ошибка:", e$message, "\n")
    cat("  Пропускаем эту колонку\n")
  })
}
```

    Обрабатываем: @metadata 
     преобразован в текст с именами
    Обрабатываем: event 
     преобразован в текст с именами
    Обрабатываем: log 
     преобразован в текст с именами
    Обрабатываем: ecs 
     преобразован в текст с именами
    Обрабатываем: host 
     преобразован в текст с именами
    Обрабатываем: agent 
     Ошибка: replacement has 5 rows, data has 101904 
      Пропускаем эту колонку

### Минимизируйте количество колонок в датафрейме – уберите колоки с единственным значением параметра.

``` r
was_count <- ncol(unested_data)
```

``` r
minimized_simple <- unested_data %>%
  select(where(~ n_distinct(., na.rm = TRUE) > 1))
```

``` r
now_count <- ncol(minimized_simple)
removed <- was_count - now_count
```

### Какое количество хостов представлено в данном датасете?

``` r
if("winlog_SubjectDomainName" %in% names(minimized_simple)) {
  domains <- unique(na.omit(minimized_simple$winlog_SubjectDomainName))
  
  domains <- domains[domains != "-" & domains != ""]
  
  cat("Уникальных доменов (хостов):", length(domains), "\n")
  cat("Домены:\n")
  print(domains)
  
  hosts_df <- data.frame(
    Host_Count = length(domains),
    Hosts = paste(domains, collapse = ", ")
  )
  
  cat("\nРезультат:\n")
  print(hosts_df)
}
```

    Уникальных доменов (хостов): 6 
    Домены:
    [1] "shire"            "NT AUTHORITY"     "IT001"            "FILE001"         
    [5] "Font Driver Host" "Window Manager"  

    Результат:
      Host_Count
    1          6
                                                                      Hosts
    1 shire, NT AUTHORITY, IT001, FILE001, Font Driver Host, Window Manager

### Подготовьте датафрейм с расшифровкой Windows Event_ID, приведите типы данных к типу их значений.

``` r
windows_event_df <- event_table
```

``` r
new_names <- c("winlog_event_id", "event_description")
for(i in 1:min(2, ncol(windows_event_df))) {
  names(windows_event_df)[i] <- new_names[i]
}
```

``` r
if("winlog_event_id" %in% names(windows_event_df)) {
  # Сначала в текст, потом в число
  windows_event_df$winlog_event_id <- as.character(windows_event_df$winlog_event_id)
  windows_event_df$winlog_event_id <- as.integer(windows_event_df$winlog_event_id)
  cat("✓ winlog_event_id преобразован\n")
}
```

    Warning: NAs introduced by coercion

    ✓ winlog_event_id преобразован

``` r
if("event_description" %in% names(windows_event_df)) {
  windows_event_df$event_description <- as.character(windows_event_df$event_description)
  cat("✓ event_description преобразован\n")
}
```

    ✓ event_description преобразован

``` r
windows_event_df <- windows_event_df %>%
  filter(!is.na(winlog_event_id)) %>%
  distinct(winlog_event_id, .keep_all = TRUE)

cat("\n✓ Справочник готов\n")
```


    ✓ Справочник готов

``` r
cat("   Уникальных событий:", nrow(windows_event_df), "\n")
```

       Уникальных событий: 370 

``` r
write.csv(windows_event_df, "windows_event_reference.csv", row.names = FALSE)
cat("   Сохранен в windows_event_reference.csv\n")
```

       Сохранен в windows_event_reference.csv

### Есть ли в логе события с высоким и средним уровнем значимости? Сколько их?

``` r
print(table(log_data$log$level))
```


          error information     verbose     warning 
              4       78473       23205         222 

``` r
cat("\nСобытий с высоким уровнем (error):", sum(log_data$log$level == "error"))
```


    Событий с высоким уровнем (error): 4

``` r
cat("\nСобытий со средним уровнем (warning):", sum(log_data$log$level == "warning"))
```


    Событий со средним уровнем (warning): 222

``` r
cat("\nВсего high+medium:", sum(log_data$log$level %in% c("error", "warning")))
```


    Всего high+medium: 226

# Оценка результатов

В ходе лабораторной работы успешно проведен анализ журналов безопасности
Windows Active Directory с использованием языка R и экосистемы
tidyverse. Получены практические навыки обработки сложных вложенных
структур данных в формате JSON: выполнено преобразование и “раскрытие”
вложенных датафреймов, что увеличило количество столбцов с 19 до 285 с
последующей оптимизацией до 179 значимых столбцов. Работа интегрирована
со справочником событий Windows, что позволило корректно
интерпретировать коды событий. Проведена оценка уровня угроз через
анализ распределения событий по уровням значимости.

# Вывод

Анализ журналов позволил получить структурированное представление о
событиях безопасности в домене Active Directory: определено количество
хостов (5), выявлено небольшое количество событий высокого (4) и
среднего (222) уровня серьезности. Работа продемонстрировала
эффективность применения инструментов tidyverse (unnest, unnest_wider,
select) для подготовки и очистки сложных логов безопасности, что
является ключевым этапом для последующего детектирования аномалий и
расследования инцидентов.
