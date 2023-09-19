# Load package from Aud's GitHub ----

# library(remotes)
# remotes::install_github("audhalbritter/dataDocumentation")
library(dataDocumentation)

get_started(path = "data_dic")

data_dic = make_data_dictionary(data = durin,
              description_table = description_table,
              table_ID = "leaf-traits",
              keep_table_ID = FALSE)
