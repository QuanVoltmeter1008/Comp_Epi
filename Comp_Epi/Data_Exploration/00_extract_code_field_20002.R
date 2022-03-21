# Extract Code from ukb Field 20002----------- 
# From ukb field 20002, impute key word of the diagnosis
# Extract all codes under that diagnosis

# load packages----------- 
library(tidyverse)
library(data.table)

# read data----------- 
setwd("/rds/general/project/hda_21-22/live/TDS/Group_2/Comp_Epi/Data_Exploration")
coding = fread('../ukb_coding_tables/coding6.tsv')
View(coding)

# create function search_id, search all 4 levels of the code----------- 
search_id <- function(disease) {
  code = NULL
  idx = coding$meaning==paste0(disease)
  find = coding[idx,]
  code = as.vector(c(code,find$coding))
  parent = as.vector(find$node_id)
  for (node in parent){
    idx = coding$parent_id==paste0(node)
    find = coding[idx,]
    code = c(code,find$coding)
    parent = as.vector(find$node_id)
    for (node in parent){
      idx = coding$parent_id==paste0(node)
      find = coding[idx,]
      code = c(code,find$coding)
      parent = as.vector(find$node_id)
      for (node in parent){
        idx = coding$parent_id==paste0(node)
        find = coding[idx,]
        code = c(code,find$coding)
        parent = as.vector(find$node_id)
        for (node in parent){
          idx = coding$parent_id==paste0(node)
          find = coding[idx,]
          code = c(code,find$coding)
          parent = as.vector(find$node_id)
          
        } 
      }
    }
  }
  return (sort(unique(code)))
}

# results----------- 
cardiovascular = search_id('cardiovascular')
cardiovascular

hypertension = search_id('hypertension')
hypertension

diabetes = search_id('diabetes')
diabetes

asthma = search_id('asthma')
asthma

COPD = c(search_id('chronic obstructive airways disease/copd'),search_id('emphysema/chronic bronchitis'),
         search_id('bronchiectasis'))
sort(COPD)

resp = c(search_id('interstitial lung disease'),search_id('other respiratory problems'))
sort(resp)

pneumonia = search_id('pneumonia')
pneumonia

infections = search_id('infections')
infections

liver = c(search_id('hepatitis'),search_id('liver failure/cirrhosis'),
          search_id('bile duct disease'),search_id('gall bladder disease'))
sort(liver)

digestive = search_id('gastrointestinal/abdominal')
digestive

musc = c(search_id('bone disorder'),
         search_id('back problem'),
         search_id('joint disorder'),
         search_id('muscle/soft tissue problem'))
sort(musc)

psycho = search_id('psychological/psychiatric problem')
psycho

uri = search_id('renal/urology')
uri

haematology = search_id('haematology')
haematology

dermatology = search_id('dermatology')
dermatology

neurology = search_id('neurology')
neurology
