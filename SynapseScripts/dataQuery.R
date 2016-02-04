require(synapseClient)
synapseLogin()

#####
## QUERY FOR THE RAW DATA FILES
# q <- synapseQuery('SELECT id, name FROM file WHERE parentId=="syn3348064"')
# print(q)

##### Training data

CoreTable_synapse_entity <- synGet("syn3346724")
CoreTable_training <- read.csv(getFileLocation(CoreTable_synapse_entity), header=T, na.strings=".", as.is=c("RPT","AGEGRP"), colClasses=c(STOMACH="factor"))

LabValue_synapse_entity <- synGet("syn3346726")
LabValue_training <- read.csv(getFileLocation(LabValue_synapse_entity), header=T, na.strings=".")

LesionMeasure_synapse_entity <- synGet("syn3346728")
LesionMeasure_training <- read.csv(getFileLocation(LesionMeasure_synapse_entity), header=T, na.strings=".")

MedHistory_synapse_entity <- synGet("syn3346730")
MedHistory_training <- read.csv(getFileLocation(MedHistory_synapse_entity), header=T, na.strings=".")

PriorMed_synapse_entity <- synGet("syn3346732")
PriorMed_training <- read.csv(getFileLocation(PriorMed_synapse_entity), header=T, na.strings=".")

VitalSign_synapse_entity <- synGet("syn3346734")
VitalSign_training <- read.csv(getFileLocation(VitalSign_synapse_entity), header=T, na.strings=".")

##### Leaderboard data

CoreTable_synapse_entity <- synGet("syn3346736")
CoreTable_leaderboard <- read.csv(getFileLocation(CoreTable_synapse_entity), header=T, na.strings=".", as.is=c("RPT","AGEGRP"), colClasses=c(STOMACH="factor"))

LabValue_synapse_entity <- synGet("syn3346738")
LabValue_leaderboard <- read.csv(getFileLocation(LabValue_synapse_entity), header=T, na.strings=".")

LesionMeasure_synapse_entity <- synGet("syn3346745")
LesionMeasure_leaderboard <- read.csv(getFileLocation(LesionMeasure_synapse_entity), header=T, na.strings=".")

MedHistory_synapse_entity <- synGet("syn3346747")
MedHistory_leaderboard <- read.csv(getFileLocation(MedHistory_synapse_entity), header=T, na.strings=".")

PriorMed_synapse_entity <- synGet("syn3346750")
PriorMed_leaderboard <- read.csv(getFileLocation(PriorMed_synapse_entity), header=T, na.strings=".")

VitalSign_synapse_entity <- synGet("syn3346740")
VitalSign_leaderboard <- read.csv(getFileLocation(VitalSign_synapse_entity), header=T, na.strings=".")

##### Validation data

CoreTable_synapse_entity <- synGet("syn3346753")
CoreTable_validation <- read.csv(getFileLocation(CoreTable_synapse_entity), header=T, na.strings=".", as.is=c("RPT","AGEGRP"), colClasses=c(STOMACH="factor"))

LabValue_synapse_entity <- synGet("syn3346742")
LabValue_validation <- read.csv(getFileLocation(LabValue_synapse_entity), header=T, na.strings=".")

LesionMeasure_synapse_entity <- synGet("syn3346755")
LesionMeasure_validation <- read.csv(getFileLocation(LesionMeasure_synapse_entity), header=T, na.strings=".")

MedHistory_synapse_entity <- synGet("syn3346757")
MedHistory_validation <- read.csv(getFileLocation(MedHistory_synapse_entity), header=T, na.strings=".")

PriorMed_synapse_entity <- synGet("syn3346761")
PriorMed_validation <- read.csv(getFileLocation(PriorMed_synapse_entity), header=T, na.strings=".")

VitalSign_synapse_entity <- synGet("syn3346759")
VitalSign_validation <- read.csv(getFileLocation(VitalSign_synapse_entity), header=T, na.strings=".")


