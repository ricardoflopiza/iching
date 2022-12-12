texto <- pdftools::pdf_text("iching.pdf")

texto <- texto[15:191]

lineas_hexagramas <- crossing(trigrama1 = c("111","000","101","010","100","011","001","110"),trigrama2 = c("111","000","101","010","100","011","001","110")) %>% 
  mutate(sintesis = paste0(trigrama1,trigrama2))

nom_trigramas <- data.frame(orden = c(1,2,7,4,5,8,3,6),
                            lineas = c("111","000","101","010","100","011","001","110"),
                            nombres = c("ch'ien", "k'un","li","k'an","ken","tui","chen","sun")) %>% 
  arrange(orden)

nom_trigramas <- nom_trigramas %>% 
  mutate(nombre2 = c("lo creativo","lo receptivo","lo que despierta","el abismo","manteniendose quieto",
                     "lo suave, lo docil","lo oscilante","lo gozoso"),
         atributo = c("fuerza","devocion, docilidad","incita el movimiento","peligro","reposo",
                      "penetración", "dar luz", "alegría"),
         imagen = c("cielo","tierra","trueno","agua","montaña","viento, madera",
                    "fuego","lago"),
         relacion_familiar = c("padre","madre","primer hijo","segundo hijo","tercer hijo","primera hija","segunda hija","tercera hija"))

lineas_hexagramas <- lineas_hexagramas %>% 
  left_join(nom_trigramas %>% select(lineas,imagen), by = c("trigrama1" = "lineas")) %>% 
  left_join(nom_trigramas %>% select(lineas,imagen), by = c("trigrama2" = "lineas"))

### extraemos trigrama de cada hexagrama ####
superior <- texto %>% stringr::str_extract("Superior .*") %>% unlist()
superior[65] <-  "Superior : Ken, La Inmovilidad, la Montaña."
superior <- superior[!is.na(superior)]

inferior <- texto %>% stringr::str_extract("Inferior .*") %>% unlist()
inferior <- inferior[!is.na(inferior)]

df_trim_hexagram <- as_tibble(superior, inferior) %>% 
  mutate(superior = tolower(superior) %>% str_replace_all("llama","fuego"),
         inferior = tolower(inferior) %>% str_replace_all("llama","fuego"))

imagenes <-  c("cielo","tierra","trueno","agua","montaña","viento","fuego","lago") 

df_trim_hexagram <- df_trim_hexagram %>% 
 mutate(orden = 1:64,
        imagen_sup = stringr::str_extract_all(df_trim_hexagram$superior,paste0(imagenes,collapse = "|")),
        imagen_inf = stringr::str_extract_all(df_trim_hexagram$inferior,paste0(imagenes,collapse = "|")),
        imagenes = paste(imagen_sup,imagen_inf)) %>% 
  mutate(imagen_sup = unlist(imagen_sup),
         imagen_inf = unlist(imagen_inf))

df_trim_hexagram %>% 
  group_by(imagen_sup) %>% 
  summarise(n = n())

iching <- lineas_hexagramas %>% 
  mutate(imagenes = paste(imagen.x,imagen.y) %>% str_remove_all(", madera")) %>% 
  inner_join(df_trim_hexagram) %>% arrange(orden) 

hexagramas <- readRDS("iching.rds")

hexa <- NA
for (i in 1:72) {
  hexa[i] <- print(hexagramas[2][[1]][i][[1]][[1]])
}

hexa <- hexa[9:72]

iching <- iching %>% 
  mutate(nombre_hexagrama = hexa) %>% 
  rename(hexagrama = sintesis) %>% 
  select(orden,nombre_hexagrama,hexagrama , trigrama1, trigrama2, superior, inferior, imagen_sup, imagen_inf, imagenes)

saveRDS(iching,"lineas_hexagrama.rds")

iching <- readRDS("lineas_hexagrama.rds")

 hex = "111000"
 times = 10

nuclear_hex <- function(hex,times){

nuc_sup <- str_sub(hex,2,4)
nuc_inf <- str_sub(hex,3,5)

df2 <- bind_cols(origen = "inicial" ,
iching[iching$hexagrama == hex,c("orden","nombre_hexagrama","hexagrama","imagenes")])

for(j in 1:times){

  nuc_sup <- str_sub(hex,2,4)
  nuc_inf <- str_sub(hex,3,5)
  
  df2 <- df2 %>% 
    bind_rows(
  bind_cols(origen = paste0("nucleo",j) ,
  iching[iching$hexagrama == paste0(nuc_sup,nuc_inf),c("orden","nombre_hexagrama","hexagrama","imagenes")])
)

  hex = paste0(nuc_sup,nuc_inf)
    
if(last(df2$hexagrama)  == "101010"){
   message("Se encontró el final")
 break()
   
 }
  
}
df2
}

nuclear_hex("101010",4)

nuclear_hex(hex = "000000",4)

nuclear_hex(hex = "111000",4)



map(iching$hexagrama,~ nuclear_hex(.,10))






