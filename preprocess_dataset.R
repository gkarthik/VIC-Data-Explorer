l <- read.csv("./data/master_log.csv", na.strings=c("?", "nd"), strip.white=TRUE, row.names=c("Ab"))
subl <- l[!rownames(l) %in% c("135"),] #Remove 135 becuase no Neut_micro
## Remove empty level
subl$Epitope_Class <- factor(subl$Epitope_Class)
subl <- subset(subl, select=-c(Protect_binary, Epitope_Class_ELISA, Endotoxin, Endotoxin.1, mW_Loss, aTTD, Epitope.Class..assigning.method.)) #Remove columns with empty values.
subl[,"Escape..code"] <- as.factor(subl[,"Escape..code"])
subl[,"Makona.binding"] <- as.factor(subl[,"Makona.binding"])

## Make Neut +ve correlation with Protection
subl[,"unNeutFrac"] <- 1- subl[,"unNeutFrac"]
subl[,"Neut_dVP30"] <- 1- subl[,"Neut_dVP30"]
subl[,"Neut_VSV"] <- (100 - subl[,"Neut_VSV"])/100

## Separate out IgG1 human and mouse
levels(subl[,"Isotype"]) <- c(levels(subl[,"Isotype"]), "HumanIgG1")
subl[subl[,"Isotype"]=="IgG1" & subl[,"Species"]=="human", "Isotype"] <- "HumanIgG1"
levels(subl[,"Isotype"]) <- c(levels(subl[,"Isotype"]), "MouseIgG1")
subl[subl[,"Isotype"]=="IgG1" & subl[,"Species"]=="mouse", "Isotype"] <- "MouseIgG1"
subl[,"Isotype"] <- droplevels(subl[,"Isotype"])

## Polyfunctionality
subl[,"Polyfunctionality"] <- as.numeric(subl[,"Polyfunctionality"])

## Add Tiers
subl[subl$Epitope_Class %in% c("Cap", "GP1/Head", "Mucin"), "Epitope_Tier"] <- "Tier1"
subl[subl$Epitope_Class %in% c("Base", "GP1/Core", "Fusion"), "Epitope_Tier"] <- "Tier2"
subl[subl$Epitope_Class %in% c("GP1/2", "HR2"), "Epitope_Tier"] <- "Tier3"
subl[subl$Epitope_Class %in% c("Unknown"), "Epitope_Tier"] <- "TierUnknown"

## Add ID
subl[,"id"] <- rownames(subl)
