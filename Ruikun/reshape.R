data1204 = readRDS("../data1204.rds")
nwnames = merged[house=="Night's Watch", name]
data1204$region[data1204$name%in%nwnames]="Night's Watch"
data1204$region[is.na(data1204$region)]="Unknown"
reshaped = copy(data1204)
reshaped$region = factor(reshaped$region)
reshaped[, c(levels(reshaped$region), "region") := 
           c(lapply(levels(region), function(x) as.integer(x == region)), .(NULL))]
reshaped[, c("south_region", "middle_region", "north_region") :=
           .(Dorne + Reach + Stormlands,
             Crownlands + Riverlands + Westerlands + `Vale of Arryn` + `Iron Islands`,
             North + `Night's Watch`)]
saveRDS(reshaped, file="../reshaped.rds")