
#df
# Banque Populaire
df_bp <- eff_banq_ze(label_banque = "Banque Populaire")
sum(df_bp$nb_eff)

# Crédit Agricole
df_ca <- eff_banq_ze(label_banque = "Crédit Agricole")
sum(df_ca$nb_eff)

# Crédit Mutuelle
df_cm <- eff_banq_ze(label_banque = "Crédit Mutuel")
sum(df_cm$nb_eff)

# Société Générale 
df_sg <- eff_banq_ze(label_banque = "Société Générale")
sum(df_sg$nb_eff)

# BNP Paribas 
df_bnp <- eff_banq_ze(label_banque = "BNP Paribas")
sum(df_bnp$nb_eff)


#df1

# Banque Populaire
df1_bp <- eff_banq_zeV1(label_banque = "Banque Populaire")
sum(df1_bp$nb_eff)

# Crédit Agricole
df1_ca <- eff_banq_zeV1(label_banque = "Crédit Agricole")
sum(df1_ca$nb_eff)

# Crédit Mutuelle
df1_cm <- eff_banq_zeV1(label_banque = "Crédit Mutuel")
sum(df1_cm$nb_eff)

# Société Générale 
df1_sg <- eff_banq_zeV1(label_banque = "Société Générale")
sum(df1_sg$nb_eff)

# BNP Paribas 
df1_bnp <- eff_banq_zeV1(label_banque = "BNP Paribas")
sum(df1_bnp$nb_eff)


recup.longlat <- function(indice){
  list.long <- vector(length = 0)
  list.lat <- vector(length = 0)
  for (k in 1:length(bdd_zese[[35]][[indice]])){
    list.long <- c(list.long, bdd_zese[[35]][[indice]][[k]][[1]][,1])
    list.lat <- c(list.lat, bdd_zese[[35]][[indice]][[k]][[1]][,2])
  }
  coord.test <- data.frame(Longitude = list.long, Latitude = list.lat)
}

viz.banq.ze <- function(df){
  mapboxToken <- paste("pk.eyJ1IjoiZ3JhbnQyNDk5IiwiYSI6ImNremZ6enYweDJjbjAybm8xejVqN3IwemQifQ.UTVkE6hkSjPESfp-0CPD7Q", collapse="")
  Sys.setenv("MAPBOX_TOKEN" = mapboxToken)
  fig <- plot_mapbox(df, x = ~Longitude, y = ~Latitude)
  fig <- fig %>% add_paths(size = I(2))
  fig <- fig %>% add_markers(data = banque, x=~Longitude, y=~Latitude, split=~Banque)
  fig <- fig %>% layout(mapbox = list(style = 'dark'))
  fig <- fig %>% config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))
}

test_bq_ze <- function(df_banque,df1_banque){
  df <- data.frame(nb_df = df_banque$nb_eff, nb_df1 = df1_banque$nb_eff)
  ind.vrai <- vector(length = 0)
  ind.faux <- vector(length = 0)
  for(i in 1:length(df$nb_df)){
    if(df$nb_df[i] == df$nb_df1[i]){
      ind.vrai <- c(ind.vrai,i)
    }
    else{
      ind.faux <- c(ind.faux,i)
    }
  }
  cat('Vrais : ',ind.vrai,'\n','Faux : ',ind.faux)
}





