library(tidyr)
library(magrittr)
library(data.table)
library(dplyr)
t0 <- proc.time()
# Read data files
setwd("~/School/2016/16_Fall/Predictive Analytics I/Project 3")
prescriber.info <- read.csv("Medicare_Provider_Utilization_and_Payment_Data__2014_Part_D_Prescriber.csv")
meta <- read.csv("Medicare_Provider_Utilization_and_Payment_Data__Part_D_Prescriber_Summary_Table_CY2014.csv")

#We're just gonna use all the drugs.
drugs <- as.character(unique(prescriber.info$DRUG_NAME))

# Combine the prescriptions for drugs that are repeated (multiple entries for the same drug for the same prescriber)
prescriber.info <- prescriber.info %>%
  group_by(NPI,NPPES_PROVIDER_LAST_ORG_NAME,NPPES_PROVIDER_FIRST_NAME,DRUG_NAME) %>%
  mutate(TOTAL_CLAIM_COUNT=sum(TOTAL_CLAIM_COUNT,na.rm=TRUE)) %>%
  filter(!duplicated(DRUG_NAME)) %>%
  ungroup()

# Convert from long to wide format and collapse the rows to one row per prescriber with the number of prescriptions written for each drug
prescriber.info <- prescriber.info %>% 
  select(NPI,DRUG_NAME, TOTAL_CLAIM_COUNT) %>%
  spread(key=DRUG_NAME, value=TOTAL_CLAIM_COUNT,fill=0) %>%
  select(NPI, one_of(drugs))

head(prescriber.info %>% arrange(NPI),n=10)

#Add county data to 'meta' real quick
meta[meta$nppes_provider_city == "MOULTONBORO",]$nppes_provider_city <- "MOULTONBOROUGH"
meta[meta$nppes_provider_city == "PLAINSTOW",]$nppes_provider_city <- "PLAISTOW"
meta[meta$nppes_provider_city == "RASHUA",]$nppes_provider_city <- "NASHUA"
meta[meta$nppes_provider_city == "LONDONBERRY",]$nppes_provider_city <- "LONDONDERRY"
meta[meta$nppes_provider_city == "AMHURST",]$nppes_provider_city <- "AMHERST"
meta[meta$nppes_provider_city == "NEW LOND ON",]$nppes_provider_city <- "NEW LONDON"
meta[meta$nppes_provider_city == "PORTSMOOTH",]$nppes_provider_city <- "PORTSMOUTH"

belknap <- c("ALTON","BARNSTEAD","BELMONT","CENTER HARBOR","GILFORD","LACONIA","MEREDITH","NEW HAMPTON","SANBORNTON","TILTON")
carroll <- c("SANBORNVILLE", "WONALANCET","ALBANY","BARTLETT","BROOKFIELD","CHATHAM","CONWAY","EATON","EFFINGHAM", "FREEDOM","HART'S LOCATION","JACKSON","MADISON","MOULTONBOROUGH","OSSIPEE","SANDWICH","TAMWORTH","TUFTONBORO","WAKEFIELD","WOLFEBORO")
cheshire <- c("MARLBOROUGH","ALSTEAD","CHESTERFIELD","DUBLIN","FITZWILLIAM","GILSUM","HARRISVILLE","HINSDALE","JAFFREY","KEENE","MALBOROUGH","MARLOW","RICHMOND","RINDGE","ROXBURY","STODDARD","SURRY","SWANZEY","TROY","WALPOLE","WESTMORELAND","WINCHESTER")
coos <- c("GROVETON", "NORTH STRATFORD", "TWIN MOUNTAIN","BERLIN","CARROLL","CLARKSVILLE","COLEBROOK","COLUMBIA","DALTON","DUMMER","ERROL","GORHAM","JEFFERSON","LANCASTER","MILAN","NORTHUMBERLAND","PITTSBURG","RANDOLPH","SHELBURNE","STARK","STEWARTSTOWN","WHITEFIELD")
grafton <- c("WOODSVILLE", "BRISTOL", "BETHLEHEM", "ETNA","ALEXANDRIA","ASHLAND","BATH","BENTON","CAMPTON","CANAAN","DORCHESTER","EASTON","ELLSWORTH","ENFIELD","FRANCONIA","GRAFTON","GROTON","HANOVER","HAVERHILL","HEBRON","HOLDERNESS","LANDAFF","LEBANON","LINCOLN","LISBON","LITTLETON","LYMAN","LYME","MONROE","ORANGE","ORFORD","PIERMONT","PLYMOUTH","RUMNEY","SUGAR HILL","THORNTON","WARREN", "WATERVILLE VALLEY","WENTWORTH","WOODSTOCK")
hillsborough <- c("NEW IPSWICH","AMHERST","ANTRIM","BEDFORD","BENNINGTON","BROOKLINE","DEERING","FRANCESTOWN","GOFFSTOWN","GREENFIELD","GREENVILLE","HANCOCK","HILLSBOROUGH","HOLLIS","HUDSON","LITCHFIELD","LYNDEBOROUGH","MANCHESTER","MASON","MERRIMACK","MILFORD","MONT VERNON","NASHUA","NEW BOSTON","NEW IPSWITCH","PELHAM","PETERBOROUGH","SHARON","TEMPLE", "WEARE","WILTON","WINDSOR")
merrimack <-  c("PITTSFIELD", "BRADFORD", "PENACOOK", "CONTOOCOOK","ALLENSTOWN","ANDOVER","BOSCAWEN","BOW","CANTERBURY","CHICHESTER","CONCORD","DANBURY","DUNBARTON","EPSOM","FRANKLIN","HENNIKER","HILL","HOOKSETT","HOPKINTON","LOUDON","NEW LONDON","NEWBURY","PEMBROKE","SALISBURY","SUTTON","WARNER","WEBSTER","WILMOT")
rockingham <- c("ATKINSON","AUBURN","BRENTWOOD","CANDIA","CHESTER","DANVILLE","DEERFIELD","DERRY","EAST KINGSTON","EPPING","EXETER","FREMONT","GREENLAND","HAMPSTEAD","HAMPTON","HAMPTON FALLS","KENSINGTON","KINGSTON","LONDONDERRY","NEW CASTLE","NEWFIELDS","NEWINGTON","NEWMARKET","NEWTON","NORTH HAMPTON","NORTHWOOD","NOTTINGHAM","PLAISTOW","PORTSMOUTH","RAYMOND","RYE","SALEM", "SANDOWN","SEABROOK","SOUTH HAMPTON", "STRATHAM","WINDHAM")
strafford <-  c("BARRINGTON","DOVER","DURHAM","FARMINGTON","LEE", "MADBURY","MIDDLETON","MILTON","NEW DURHAM","ROCHESTER","ROLLINSFORD","SOMERSWORTH","STRAFFORD")
sullivan <-  c("ACWORTH","CHARLESTOWN","CLAREMONT","CORNISH","CROYDON","GOSHEN","GRANTHAM","LANGDON","LEMPSTER","NEWPORT","PLAINFIELD","SPRINGFIELD","SUNAPEE","UNITY","WASHINGTON")

meta$County <- 0
meta[meta$nppes_provider_city %in% (grep(paste(belknap,collapse="|"), 
                                         meta$nppes_provider_city, value=TRUE)), 'County'] <- "Belknap"
meta[meta$nppes_provider_city %in% (grep(paste(carroll,collapse="|"), 
                                         meta$nppes_provider_city, value=TRUE)), 'County'] <- "Carroll"
meta[meta$nppes_provider_city %in% (grep(paste(cheshire,collapse="|"), 
                                         meta$nppes_provider_city, value=TRUE)), 'County'] <- "Cheshire"
meta[meta$nppes_provider_city %in% (grep(paste(coos,collapse="|"), 
                                         meta$nppes_provider_city, value=TRUE)), 'County'] <- "Coos"
meta[meta$nppes_provider_city %in% (grep(paste(grafton,collapse="|"), 
                                         meta$nppes_provider_city, value=TRUE)), 'County'] <- "Grafton"
meta[meta$nppes_provider_city %in% (grep(paste(hillsborough,collapse="|"), 
                                         meta$nppes_provider_city, value=TRUE)), 'County'] <- "Hillsborough"
meta[meta$nppes_provider_city %in% (grep(paste(merrimack,collapse="|"), 
                                         meta$nppes_provider_city, value=TRUE)), 'County'] <- "Merrimack"
meta[meta$nppes_provider_city %in% (grep(paste(rockingham,collapse="|"), 
                                         meta$nppes_provider_city, value=TRUE)), 'County'] <- "Rockingham"
meta[meta$nppes_provider_city %in% (grep(paste(strafford,collapse="|"), 
                                         meta$nppes_provider_city, value=TRUE)), 'County'] <- "Strafford"
meta[meta$nppes_provider_city %in% (grep(paste(sullivan,collapse="|"), 
                                         meta$nppes_provider_city, value=TRUE)), 'County'] <- "Sullivan"

# Merge with metadata about the prescriber
prescriber.info <- prescriber.info %>% 
  merge(meta, by.x="NPI", by.y = "npi") %>%
  mutate(Opioid.Prescriber=ifelse( (opioid_bene_count<10 | is.na(opioid_bene_count)) & (opioid_claim_count<10 | is.na(opioid_claim_count)) ,0,1)) %>%
  select(NPI, Gender=nppes_provider_gender, County=County, Credentials=nppes_credentials, Specialty=specialty_description, one_of(drugs), Opioid.Prescriber)

# head(prescriber.info %>% arrange(NPI),n=10)
write.csv(prescriber.info,'prescriber-info.csv',row.names=FALSE)
print(sprintf("Finished in %f seconds",(proc.time()-t0)[3]))