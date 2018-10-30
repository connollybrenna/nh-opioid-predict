library(choroplethrMaps)
library(choroplethr)

setwd("~/School/2016/16_Fall/Predictive Analytics I/Project 3")
info <- read.csv("Medicare_Provider_Utilization_and_Payment_Data__2014_Part_D_Prescriber.csv")
opiods <- read.csv("opioids.csv")
meta <- read.csv("Medicare_Provider_Utilization_and_Payment_Data__Part_D_Prescriber_Summary_Table_CY2014.csv")
meta[is.na(meta)] <- 0

#meta$County[which(meta$nppes_provider_city %in% c())] <- "NAME"

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
                                         meta$nppes_provider_city, value=TRUE)), 'County'] <- "belknap"
meta[meta$nppes_provider_city %in% (grep(paste(carroll,collapse="|"), 
                                         meta$nppes_provider_city, value=TRUE)), 'County'] <- "carroll"
meta[meta$nppes_provider_city %in% (grep(paste(cheshire,collapse="|"), 
                                         meta$nppes_provider_city, value=TRUE)), 'County'] <- "cheshire"
meta[meta$nppes_provider_city %in% (grep(paste(coos,collapse="|"), 
                                         meta$nppes_provider_city, value=TRUE)), 'County'] <- "coos"
meta[meta$nppes_provider_city %in% (grep(paste(grafton,collapse="|"), 
                                         meta$nppes_provider_city, value=TRUE)), 'County'] <- "grafton"
meta[meta$nppes_provider_city %in% (grep(paste(hillsborough,collapse="|"), 
                                         meta$nppes_provider_city, value=TRUE)), 'County'] <- "hillsborough"
meta[meta$nppes_provider_city %in% (grep(paste(merrimack,collapse="|"), 
                                         meta$nppes_provider_city, value=TRUE)), 'County'] <- "merrimack"
meta[meta$nppes_provider_city %in% (grep(paste(rockingham,collapse="|"), 
                                         meta$nppes_provider_city, value=TRUE)), 'County'] <- "rockingham"
meta[meta$nppes_provider_city %in% (grep(paste(strafford,collapse="|"), 
                                         meta$nppes_provider_city, value=TRUE)), 'County'] <- "strafford"
meta[meta$nppes_provider_city %in% (grep(paste(sullivan,collapse="|"), 
                                         meta$nppes_provider_city, value=TRUE)), 'County'] <- "sullivan"

data(county.regions)
mapframe <- county.regions[county.regions$state.abb == "NH",]

tmp <- sum(meta[meta$County == "carroll",]$opioid_claim_count)/sum(meta[meta$County == "belknap",]$total_claim_count)
tmp <- append(tmp, (sum(meta[meta$County == "carroll",]$opioid_claim_count)/sum(meta[meta$County == "carroll",]$total_claim_count)))
tmp <- append(tmp, (sum(meta[meta$County == "cheshire",]$opioid_claim_count)/sum(meta[meta$County == "cheshire",]$total_claim_count)))
tmp <- append(tmp, (sum(meta[meta$County == "coos",]$opioid_claim_count)/sum(meta[meta$County == "coos",]$total_claim_count)))
tmp <- append(tmp, (sum(meta[meta$County == "grafton",]$opioid_claim_count)/sum(meta[meta$County == "grafton",]$total_claim_count)))
tmp <- append(tmp, (sum(meta[meta$County == "hillsborough",]$opioid_claim_count)/sum(meta[meta$County == "hillsborough",]$total_claim_count)))
tmp <- append(tmp, (sum(meta[meta$County == "merrimack",]$opioid_claim_count)/sum(meta[meta$County == "merrimack",]$total_claim_count)))
tmp <- append(tmp, (sum(meta[meta$County == "rockingham",]$opioid_claim_count)/sum(meta[meta$County == "rockingham",]$total_claim_count)))
tmp <- append(tmp, (sum(meta[meta$County == "strafford",]$opioid_claim_count)/sum(meta[meta$County == "strafford",]$total_claim_count)))
tmp <- append(tmp, (sum(meta[meta$County == "sullivan",]$opioid_claim_count)/sum(meta[meta$County == "sullivan",]$total_claim_count)))



mapframe$value <- tmp
mapframe <- mapframe[,c(1,7)]
county_choropleth(mapframe, num_colors = 1, state_zoom = "new hampshire")
