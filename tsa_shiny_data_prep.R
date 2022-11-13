##most recent year
file_id <- as.numeric(format(Sys.Date(), '%Y'))-1

##variables
bbarea <- c("BB", "C", "CRC", "I", "R")
labloc <- c("B", "C", "CRC", "I", "R")
products <- c("RC", "PLTG", "PLG", "CRYG", "MFP")
product_labels <- c("RBCs", "Platelets", "Plasma", "Cryoprecipitate", "Manufactured")

##read data
##query in tsa_query
path <- "C:/Users/marie/Documents/MastersDegree/3_DATA824_DataVisualization/Final"
bb_if_data <- readRDS(file = paste0(path, "/Data/bb_if_data_fy", file_id, ".rds"))
bb_is_data <- readRDS(file = paste0(path, "/Data/bb_is_data_fy", file_id, ".rds"))
bb_t_data <- readRDS(file = paste0(path, "/Data/bb_t_data_fy", file_id, ".rds"))

##data prep
bb_if_data <- bb_if_data |> 
  filter(UnitArea %in% bbarea) |>
  mutate(Month = month(IssueDate_ODBC, label = TRUE),
         Weekday = wday(IssueDate_ODBC, label = TRUE),
         Components = factor(CompGroup, 
                             levels = products,
                             labels = product_labels))

bb_is_data <- bb_is_data |> 
  filter(UnitArea %in% bbarea) |>
  mutate(Month = month(IssueDate, label = TRUE),
         Weekday = wday(IssueDate, label = TRUE),
         Hour = hour(IssueTime),
         Components = factor(CompGroup, 
                             levels = products,
                             labels = product_labels))

bb_t_data <- bb_t_data |>
  filter(ResultLab %in% labloc,
         CurrentResult == "Yes",
         Result != "HIDE") |> 
  mutate(Month = month(ReceiveDateODBC, label = TRUE),
         Weekday = wday(ReceiveDateODBC, label = TRUE),
         Hour = hour(ReceiveTimeODBC),
         Priority = case_when(PriorityCodes == "S" ~ "Stat",
                              TRUE ~ "Routine"),
         TAT = round(as.numeric(difftime(as.POSIXct(paste(ResultDateODBC, ResultTimeODBC)),
                                         as.POSIXct(paste(ReceiveDateODBC, ReceiveTimeODBC)),
                                         units = c("mins")))),
         Battery = case_when(BatTstCode == "XM" ~ "Crossmatch",
                             BatTstCode == "TYSC" ~ "Type and Screen",
                             TRUE ~ "Other %AS battery"),
         Battery = factor(Battery, levels = c("Crossmatch", "Type and Screen", "Other %AS battery")),
         Results = case_when(!(is.na(ResultModifiers)) ~ "Requires Follow up",
                             Result == "%NEG" ~ "Negative",
                             TRUE ~ "Requires Follow up"),
         Method = case_when(ResultMethod == "%BPW" ~ "Bench",
                            ResultMethod == "EC2" ~ "Echo",
                            ResultMethod == "NE1" ~ "Neo",
                            TRUE ~ "Other")) |> 
  filter(TAT > 24 & TAT < 1440)   ##remove data not representative of normal bb processes

##quality check
if(min(bb_is_data$IssueDate) == min(bb_if_data$IssueDate_ODBC) & min(bb_t_data$ReceiveDateODBC) == min(bb_is_data$IssueDate)) {
  mindate <- format(min(bb_is_data$IssueDate), "%B %Y")
} else {
  stop("Check report start dates.")
}

if(max(bb_is_data$IssueDate) == max(bb_if_data$IssueDate_ODBC) & max(bb_t_data$ReceiveDateODBC) == max(bb_is_data$IssueDate)) {
  maxdate <- format(max(bb_is_data$IssueDate), "%B %Y")
} else {
  stop("Check report end dates.")
}

if(min(year(bb_is_data$IssueDate)) != max(year(bb_is_data$IssueDate))){
  fiscal_months <- c("Jul", "Aug",  "Sep",  "Oct",  "Nov",  "Dec", "Jan",  "Feb",  "Mar",  "Apr",  "May",  "Jun")
  bb_if_data <- bb_if_data |> 
    mutate(Month = factor(Month, levels = fiscal_months))
  bb_is_data <- bb_is_data |> 
    mutate(Month = factor(Month, levels = fiscal_months))
  bb_t_data <- bb_t_data |> 
    mutate(Month = factor(Month, levels = fiscal_months))
}


file_id <- as.numeric(format(Sys.Date(), '%Y'))-1
saveRDS(bb_if_data, file = paste0("bb_if_data_fy", file_id, ".rds"))
saveRDS(bb_is_data, file = paste0("bb_is_data_fy", file_id, ".rds"))
saveRDS(bb_t_data, file = paste0("bb_t_data_fy", file_id, ".rds"))
