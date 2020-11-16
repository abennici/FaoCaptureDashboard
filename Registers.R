#SP register
req = readr::read_csv("https://raw.githubusercontent.com/openfigis/RefData/gh-pages/species/CL_FI_SPECIES_ITEM.csv", col_names = T)
#colnames(req) <- c("Alpha_Code", "Identifier", "Name_En", "Name_Fr", "Name_Es", "Name_Ar", "Name_Cn", "Name_Ru", "Unit", "Scientific_Name", "Author", "Taxonomic_Code")
sp_register <- data.frame(
  species = req$Alpha3_Code,
  label = paste0(req$Name_En," [",req$Alpha_Code,"]"),
  stringsAsFactors = FALSE
)

l_sp<-readr::read_csv("https://raw.githubusercontent.com/openfigis/RefData/gh-pages/species/CL_FI_SPECIES_ITEM.csv", col_names = T)
l_sp<-l_sp[,c("Alpha3_Code","Scientific_Name")]
colnames(l_sp)<-c("species","sp_name")

l_flag<-readr::read_csv("https://raw.githubusercontent.com/openfigis/RefData/gh-pages/country/CL_FI_COUNTRY_ITEM.csv", guess_max = 0)
l_flag<-l_flag[,c("ISO3_Code","Name_En")]
colnames(l_flag)<-c("flag","flag_name")