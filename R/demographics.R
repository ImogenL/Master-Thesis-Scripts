ADcontrolage = subset(index.add$age , index.add$label == "AD control")
ADcontrolagesd = sd(ADcontrolage)
ADcontrolagemean = mean(ADcontrolage ) 

ADcontrolsex = subset(index.add["sex"], index.add$label == "AD control")
ADcontrolsexmale = sum(ADcontrolsex["sex"] == "Male")
ADcontrolsexfemale = sum(ADcontrolsex["sex"] == "Female")

#
ADage = subset(index.add$age , index.add$label == "Alzheimer's disease")
ADagesd = sd(ADage)
ADagemean = mean(ADage ) 

ADsex = subset(index.add["sex"], index.add$label == "Alzheimer's disease")
ADsexmale = sum(ADsex["sex"] == "Male")
ADsexfemale = sum(ADsex["sex"] == "Female")

#
controlage = subset(index.add$age , index.add$label == "Control")
controlage <- na.omit(controlage)
controlagesd = sd(controlage)
controlagemean = mean(controlage) 

controlsex = subset(index.add["sex"], index.add$label == "Control")
controlsex <- na.omit(controlsex)
controlsexmale = sum(controlsex["sex"] == "Male")
controlsexfemale = sum(controlsex["sex"] == "Female")

#
subcogage = subset(index.add$age , index.add$label == "Healthy / subjective cognitive complaints")
subcogage <- na.omit(subcogage)
subcogagesd = sd(subcogage)
subcogagemean = mean(subcogage ) 

subcogsex = subset(index.add["sex"], index.add$label == "Healthy / subjective cognitive complaints")
subcogsex <- na.omit(subcogsex)
subcogsexmale = sum(subcogsex["sex"] == "Male")
subcogsexfemale = sum(subcogsex["sex"] == "Female")

#
SZage = subset(index.add$age , index.add$label == "Schizophrenia")
SZage <- na.omit(SZage)
SZagesd = sd(SZage)
SZagemean = mean(SZage) 

SZsex = subset(index.add["sex"], index.add$label == "Schizophrenia")
SZsex <- na.omit(SZsex)
SZsexmale = sum(SZsex["sex"] == "Male")
SZsexfemale = sum(SZsex["sex"] == "Female")

#
SZcontrolage = subset(index.add$age , index.add$label == "SZ control")
SZcontrolagesd = sd(SZcontrolage)
SZcontrolagemean = mean(SZcontrolage ) 

SZcontrolsex = subset(index.add["sex"], index.add$label == "SZ control")
SZcontrolsexmale = sum(SZcontrolsex["sex"] == "Male")
SZcontrolsexfemale = sum(SZcontrolsex["sex"] == "Female")

#
MDDage = subset(index.add$age , index.add$label == "Major depressive disorder")
MDDage <- na.omit(MDDage)
MDDagesd = sd(MDDage)
MDDagemean = mean(MDDage) 

MDDsex = subset(index.add["sex"], index.add$label == "Major depressive disorder")
MDDsex <- na.omit(MDDsex)
MDDsexmale = sum(MDDsex["sex"] == "Male")
MDDsexfemale = sum(MDDsex["sex"] == "Female")