ICES_full = read.csv("Data/ICES assessment PB2.csv")

ICES_full$Stock[ICES_full$FishStock == "cod.27.6a"] <- "Cod6a"
ICES_full$Stock[ICES_full$FishStock == "cod.27.7a"] <- "Cod7a"
ICES_full$Stock[ICES_full$FishStock == "cod.27.7e-k"] <- "Cod7ek"
ICES_full$Stock[ICES_full$FishStock == "cod.27.5b1"] <- "CodFaroe"
ICES_full$Stock[ICES_full$FishStock == "cod.27.47d20"] <- "CodNS"

ICES_full$Stock[ICES_full$FishStock == "had.27.6b"] <- "Had6b"
ICES_full$Stock[ICES_full$FishStock == "had.27.7b-k"] <- "Had7bk"
ICES_full$Stock[ICES_full$FishStock == "had.27.46a20"] <- "HadNS"

ICES_full$Stock[ICES_full$FishStock == "ple.27.7a"] <- "Ple7a"
ICES_full$Stock[ICES_full$FishStock == "ple.27.7fg"] <- "Ple7fg"
ICES_full$Stock[ICES_full$FishStock == "ple.27.7h-k"] <- "Ple7hk"

ICES_full$Stock[ICES_full$FishStock == "sol.27.7a"] <- "Sol7a"
ICES_full$Stock[ICES_full$FishStock == "sol.27.7fg"] <- "Sol7fg"
ICES_full$Stock[ICES_full$FishStock == "sol.27.7h-k"] <- "Sol7hk"
ICES_full$Stock[ICES_full$FishStock == "sol.27.20-24"] <- "Sol2024"

ICES_full$Stock[ICES_full$FishStock == "whg.27.6a"] <- "Whg6a"
ICES_full$Stock[ICES_full$FishStock == "whg.27.7a"] <- "Whg7a"
ICES_full$Stock[ICES_full$FishStock == "whg.27.7b-ce-k"] <- "Whg7bk"
ICES_full$Stock[ICES_full$FishStock == "whg.27.47d"] <- "WhgNS"

unique(ICES_full$FishStock)
unique(ICES_full$Stock)

ICES = ICES_full[!ICES_full$Stock == "Ple7fg", ] 

ICES$B_BMSY = ICES$StockSize/(ICES$MSYBtrigger * 2)
ICES$F_FMSY = ICES$FishingPressure/ ICES$FMSY
