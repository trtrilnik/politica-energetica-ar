setwd("D:/Documentos/1FCEYS/tesis/text_mining/R")

rm(list=ls())

# Instalación de paquetes
{if(!requireNamespace("pdftools", quietly = TRUE)) install.packages("pdftools")
if(!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
if(!requireNamespace("stopwords", quietly = TRUE)) install.packages("stopwords")
if(!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
if(!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if(!requireNamespace("forcats", quietly = TRUE)) install.packages("forcats")
if(!requireNamespace("wordcloud2", quietly = TRUE)) install.packages("wordcloud2")
if(!requireNamespace("igraph", quietly = TRUE)) install.packages("igraph")
if(!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if(!requireNamespace("ggpubr", quietly = TRUE)) install.packages("ggpubr")
if(!requireNamespace("Matrix", quietly = TRUE)) install.packages("Matrix")
if(!requireNamespace("slam", quietly = TRUE)) install.packages("slam")
if(!requireNamespace("bench", quietly = TRUE)) install.packages("bench")
if(!requireNamespace("tm", quietly = TRUE)) install.packages("tm")
if(!requireNamespace("udpipe", quietly = TRUE)) install.packages("udpipe")
if(!requireNamespace("tidytext", quietly = TRUE)) install.packages("tidytext")
if(!requireNamespace("text2vec", quietly = TRUE)) install.packages("text2vec")
if(!requireNamespace("topicmodels", quietly = TRUE)) install.packages("topicmodels")
if(!requireNamespace("ggrepel", quietly = TRUE)) install.packages("ggrepel")
if(!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
}

{library(ggrepel)
  library(text2vec)
  library(topicmodels)
  library(udpipe)
  library(tm)
  library(dplyr)
  library(wordcloud2)
  library(ggplot2)
  library(forcats)
  library(pdftools)
  library(stringr)
  library(tidytext)
  library(stopwords)
  library(readr)
  library(tidyverse)
  library(igraph)
  library(ggraph)
  library(SnowballC)
  library(ggpubr)
  library(Matrix)
  library(slam)
  library(bench)
  }


# 1.
# Selección del directorio con la carpeta que están todos los pdfs a analizar

directorio_pdfs <- "textos_input"

# Obtener una lista de los PDFs en el directorio
todoslos_pdf <- list.files(
  path = directorio_pdfs,
  pattern = "\\.pdf$",
  full.names = TRUE,
  ignore.case = TRUE
)
todoslos_pdf # Control 

# 2.
# Función global que permite leer cada uno de los pdfs dispoonibles en la carpeta sin unificarlos

leer_todoslospdf <- function(path_pdf) {
  paste(pdf_text(path_pdf), collapse = " ")
}

rm(limpiar_texto)  #opcional para asegurar que no queda grabado en la memoria ninguna función similar para limpiar textos

# ====================================================
# 3.LIMPIEZA DE TEXTO
# ====================================================

{# Función de limpieza (URLs, fórmulas, emojis, acentos, etc.)

limpiar_texto <- function(txt) {
  
  txt <- str_replace_all(txt, "[\u00AD\u2010-\u2015]", "-") # Solución para palabras partidas 
                                                            # Patrón: un guión (-) seguido de opcionales espacios (\s*) y un salto de línea (\n).
                                                            # se reemplaza por un string vacío ("") para unir las partes de la palabra.
                                                            # a. \\s* busca espacios opcionales antes del guión.
                                                            # b. [-–] busca guión corto (-) O guión medio (–). 
                                                            # c. \\s* busca espacios opcionales después del guión (antes del salto)
                                                            # d. [\r\n]+ busca el salto de línea
                                                            # e. \\s* busca la sangría de la siguiente línea
  
  txt <- str_replace_all(txt, "([[:alpha:]])-\\s+([[:alpha:]])", "\\1\\2") # Normalización de guiones y caracteres invisibles
                                                                           # Se reemplazan los soft-hyphens (\u00AD), guiónes largos, medios, etc. por un guión normal "-"
                                                                           # Esto es fundamental porque a veces el guión no es el que se ve en el teclado.
  
                                                                           # 2. Unión inteligente usando Grupos de Captura
                                                                           # Explicación del patrón: "([[:alpha:]])-\\s+([[:alpha:]])"
                                                                           # - ([[:alpha:]]) : Captura la última letra de la palabra de arriba (Grupo 1)
                                                                           # - -: El guion
                                                                           # - \\s+: Cualquier espacio, salto de línea o sangría que haya en medio
                                                                           # - ([[:alpha:]]) : Captura la primera letra de la palabra de abajo (Grupo 2)
                                                                           # Reemplaza: "\\1\\2" (Pega el Grupo 1 y el Grupo 2, eliminando lo del medio)
  
  txt <- tolower(txt)
  txt <- gsub("\n", " ", txt, fixed = TRUE)
  
  # URLs
  txt <- gsub("https?://\\S+|www\\.[^\\s]+", " ", txt)
  
  # Bloques LaTeX/markdown
  txt <- gsub("\\$[^$]+\\$", " ", txt)        # $...$
  txt <- gsub("\\\\\\(.*?\\\\\\)", " ", txt)  # \( ... \)
  txt <- gsub("\\\\\\[.*?\\\\\\]", " ", txt)  # \[ ... \]
  
  # Emojis / emoticones
  txt <- gsub("[\\p{So}\\p{Cn}]+", " ", txt, perl = TRUE)
  txt <- gsub("[:;=8][-~]?[\\)D\\]\\[pPoO3]", " ", txt)
  
  # Acentos
  txt <- gsub("[áàäâ]", "a", txt)
  txt <- gsub("[éèëê]", "e", txt)
  txt <- gsub("[íìïî]", "i", txt)
  txt <- gsub("[óòöô]", "o", txt)
  txt <- gsub("[úùüû]", "u", txt)
  txt <- gsub("hidrocarra", "hidrocarburos", txt)
  
  # Solo letras (incluye ñ) y espacios
  txt <- gsub("[^a-zñ ]", " ", txt)
  
  # Números aislados
  txt <- gsub("\\b\\d+\\b", " ", txt)
  
  # Operadores matemáticos (alternación segura)
  txt <- gsub("(=|\\+|\\*|/|<|>|%|\\^|\\-)+", " ", txt, perl = TRUE)
  
  # Espacios
  txt <- gsub("\\s+", " ", txt)
  trimws(txt)
  
  # Otros elementos. En este caso fueron incluidos encabezados y pies de página repetitivos en los documentos utilizados
  # Se debe tener en cuenta que el contenido debe estar escrito considerando que se han eliminado las tildes.
  
  txt <- gsub("bnef", "beneficio", txt, perl = TRUE)
  txt <- gsub("bustible", "combustible", txt, perl = TRUE)
  txt <- gsub("cepal", " ", txt, perl = TRUE)
  txt <- gsub("politicas industriales y tecnologicas en america latina", " ", txt, perl = TRUE)
  txt <- gsub("comision economica para america latina y el caribe", " ", txt, perl = TRUE)
  txt <- gsub("sociedad y economia no pp", " ", txt, perl = TRUE)
  txt <- gsub("la disponibilidad de informacion para el monitor", " ", txt, perl = TRUE)
  txt <- gsub("modelo de publicacion sin fines de lucro para conservar la naturaleza academica y abierta de la comunicacion cientifica pdf generado a partir de xmljatsr", " ", txt, perl = TRUE)
  txt <- gsub("amelica", " ", txt, perl = TRUE)
  txt <- gsub("faces vol num enerojunio issn issn", " ", txt, perl = TRUE)
  txt <- gsub("la gobernanza de la eficiencia energetica una politica publica efectiva para fortalecer la transicion energetica hacia modelos camarda", " ", txt, perl = TRUE)
  txt <- gsub("revista administracion publica y sociedad apysiifapfcsunc n enerojunio issn", " ", txt, perl = TRUE)
  txt <- gsub("politicas climaticas en america latina y el caribe", " ", txt, perl = TRUE)
  txt <- gsub("la energia renovable en argentina como estrategia de politica energetica e industrial", " ", txt, perl = TRUE)
  txt <- gsub("aprendizaje e ennovacion en las industrias de energia de fuentes renovables en argentina mercado tecnologia organizacion e instituciones", " ", txt, perl = TRUE)
  txt <- gsub("revista tempo do mundo rtm n ago", " ", txt, perl = TRUE)
  txt <- gsub("politica energetica argentina un balance del periodo", " ", txt, perl = TRUE)
  txt <- gsub("practicas de oficio v n dic jun ides org ar publicaciones practicasdeoficio", " ", txt, perl = TRUE)
  txt <- gsub("hacia una definicion de transicion energetica para sudamerica antropoceno geopolitica y posdesarrollo", " ", txt, perl = TRUE)
  txt <- gsub("avances en energias renovables y medio ambiente vol pp impreso en la argentina issn trabajo selecionado de actas asades", " ", txt, perl = TRUE)
  txt <- gsub("politica energetica y desarrollo socioeconomico una aplicacion", " ", txt, perl = TRUE)
  txt <- gsub("energias renovables y procesos de desarrollo sustentable nuevas", " ", txt, perl = TRUE)
  txt <- gsub("ecuador estrategias y politicas publicas en energia", " ", txt, perl = TRUE)
  txt <- gsub("aproximacion a la geopolitica de las energias renovables", " ", txt, perl = TRUE)
  txt <- gsub("integracion energetica regional en mercosur los casos de la argentina y", " ", txt, perl = TRUE)
  txt <- gsub("energias renovables acceso energetico y capital social", " ", txt, perl = TRUE)
  txt <- gsub("capital social instituciones y redes", " ", txt, perl = TRUE)
  txt <- gsub("franco david hessling herrera emilce ethel ottavianelli carlos alberto cadena legislacion ambiental en energias renovables y transicion energetica en argentina hasta leyes nacionales y provinciales", " ", txt, perl = TRUE)
  txt <- gsub("estudios del habitat vol num", " ", txt, perl = TRUE)
  txt <- gsub("la crisis energetica de la argentina origenes y perspectivas", " ", txt, perl = TRUE)
  txt <- gsub("revista universitaria de geografia", " ", txt, perl = TRUE)
  txt <- gsub("ada graciel nogar et al argentina en el contexto de crisis y transicion energetica", " ", txt, perl = TRUE)
  txt <- gsub("jornadas ite facultad de ingenieria unlp", " ", txt, perl = TRUE)
  txt <- gsub("revista de economia institucional vol n primer semestre pp", " ", txt, perl = TRUE)
  txt <- gsub("la inversion en energias renovables en argentina", " ", txt, perl = TRUE)
  txt <- gsub("cuadernos de investiagacion serie economia numero", " ", txt, perl = TRUE)
  txt <- gsub("fecha de recepcion", " ", txt, perl = TRUE)
  txt <- gsub("fecha de aceptacion", " ", txt, perl = TRUE)
  txt <- gsub("politics in the age of transboundary crises", " ", txt, perl = TRUE)
  txt <- gsub("regulacion energetica y consumo industrial en argentina", " ", txt, perl = TRUE)
  txt <- gsub("los efectos estructurales de la politica energetica en la economia argentina", " ", txt, perl = TRUE)
  txt <- gsub("redes vol n bernal junio de pp", " ", txt, perl = TRUE)
  txt <- gsub("rev est de politicas publicas diciembre junio", " ", txt, perl = TRUE)
  txt <- gsub("politicas publicas para la transicion energetica argentina", " ", txt, perl = TRUE)
  txt <- gsub("esteban serrani mariano a barrera", " ", txt, perl = TRUE)
  txt <- gsub("introduccion", " ", txt, perl = TRUE)
  txt <- gsub("abordando la transicion energetica en argentina un analisis de la sostenibilidad ambiental", " ", txt, perl = TRUE)
  txt <- gsub("faces vol num enerojunio issn", " ", txt, perl = TRUE)
}
}

# ====================================================
# 4. Lectura y unificación de textos
# ====================================================

{# Lectura de todos los PDFs

docs_sinunir <- lapply(todoslos_pdf, leer_todoslospdf)
str(docs_sinunir)

# 5. 
# Aplicación de la limpieza

docs_sinunirlimpios <- lapply(docs_sinunir, limpiar_texto)
docs_sinunirlimpios # cada uno de los textos limpios pero no sin unificar

# 6. 
# Unificación de los pdfs en un solo documento

corpus<- paste(unlist(docs_sinunirlimpios), collapse = " ")
corpus # Control

# 7. 
# Guardado del corpus para tenerlo antes de comenzar la tokenización

writeLines(corpus, "corpuslimpios.txt", useBytes = TRUE)
}
# ====================================================
# 8. STOPWORDS
# ====================================================

{# Eliminación de las stopwords y nombres propios.

# Se obtienen las stopwords en español y se combinan con otras que sean detectadas en el analisis de los documentos seleccionados.

stop_es <- stopwords("es")
stop_es # Muestra las stopwords incluidas en el paquete
stop_extra <- c(
  "able", "abril", "abstract", "academica", "acceso", "acciones", "accion", "acoplando",
  "actividad", "acto", "actores", "actual", "acuerdo", "acusa", "adelante","adema","ademas", "administracion",
  "advisory", "agency", "agentes", "agosto", "ahi", "aires", "al", "alc", "alcanzar", "algo",
  "algun", "alguna", "algunas", "alguno","algunos", "alto", "ambiciosos", "ambito", "analisis","analizar",
  "and", "anexo", "ante", "anterior", "antes", "antioquia", "anual", "año",
  "años", "apoyo", "apys", "aquel", "aquellas", "aquellos", "aqui", "archivo",
  "archivese", "area", "argentina","ario", "articulo", "asegurar", "asi", "aspecto", "aspectos",
  "aumenta", "aumento", "aun", "aunque", "author", "autor","autora","autores", "avance", "avances",
  "bajo", "balance", "bariloche", "barreras", "base", "bdte", "beneficios", "best",
  "bibliografia", "bien", "bienes", "biogas", "biomasa", "boiteux", "boletin", "bolivia",
  "brasil", "brazil", "buen","buenos", "buratovich", "busca","buscar", "busqueda", "cacion", "cada",
  "cambio", "cambios", "cammesa", "cantidad", "capitulo", "carbon", "care", "carga",
  "caribe", "cas", "caso", "casos", "catalunya", "ceb", "central", "centro",
  "cepal", "chile", "ciencia", "cien","cienti", "ciente", "cientifica", "ciento", "cion",
  "cios", "cit", "cita", "citada", "citado", "civil", "clase", "clave",
  "cliente", "cnd", "cobertura", "coe","colombia", "cols", "comercial", "comision", "committee",
  "como", "comodoro", "complejo", "componentes", "comuniquese", "concepto", "conclusi", "conclusion",
  "condiciones", "confianza", "conjunto", "consecuencia", "consejo", "conservar", "considera", "consideran",
  "considerando", "constituye", "construido", "construir", "consumidores", "consumo", "content", "contexto",
  "contrario", "control", "cooperativas", "corresponde", "correspondiente", "corte", "cosa", "cosas",
  "costo", "costos", "covid","creciente", "crecimiento", "crisis", "cuales", "cualquier", "cuando",
  "cuanto", "cuatro", "cuenta", "cuestion", "cuestiones", "cutral","cts", "cuadro", "cumplimiento","cuyo",
  "dado", "dar", "dato","datos", "debate", "debe", "deben", "deber","deberia", "debido",
  "debiera", "decada", "decadas", "decir", "decision", "decreto", "deda","defic", "definir",
  "dejar", "del", "demanda", "demas", "democracia", "dentro", "depende", "der","derechos",
  "derogada", "desafio", "desafios", "desarrollar", "desde", "despues", "destacar", "developing",
  "dia", "dias", "dice", "dicha", "dicho", "diciembre", "diferencia", "diferentes",
  "dificil", "digital", "digithum", "dimension", "direccion", "directa", "diseño", "disponible",
  "disponibilidad", "disposicion", "distintas", "distintos", "distribuidores", "diversas", "diversos", "docente",
  "documento", "doi", "dolares", "domestica", "domestico", "domesticos", "donde", "dos",
  "durante", "e", "economia", "economica", "economicas", "economico", "economicos", "ed","edi",
  "educacion", "edf", "efecto", "efectos", "eficiencia", "eficiente", "efm", "efr", "eje",
  "ejecutivo", "ejemplo", "elaboracion","elementos", "ello", "embargo", "emissions", "emisiones", "emotions", "empleo", "empresa",
  "empresas", "en", "energy", "enero", "enfoque", "enseñanza", "entonces", "entorno", "entre", "era", "es",
  "escala", "escenarios","escenario", "escribano", "esfuerzo", "espacio", "especial", "especi", "espera",
  "esta", "establece", "establecer", "estaban", "estado", "estados", "estan", "estas",
  "estatal", "este", "estimacion", "estrategia", "estrategias", "estructura", "estudio", "estudios",
  "et", "etapa", "europa", "evaluacion", "evidentemente", "evitar", "ex", "existe",
  "existencia", "existen", "expansion", "explotacion", "exportacion", "extranjera", "extranjero", "faces",
  "factible", "factores", "falta", "fceys", "fcs", "febrero", "figura", "fin",
  "final", "finales", "financiamiento", "fines", "firma", "fiscales", "flexible", "fnre","foreign",
  "forma", "formas", "fomentar", "formacion", "fossil", "fpv", "frente", "from", "fuente",
  "fuentes", "fuerte", "fundamental", "funcion", "fuoc", "futuro", "gases", "gasi",
  "gcp", "gei", "general", "generacion", "generar", "generation", "gestion", "global",
  "globales", "gobernanza", "gobierno", "grado", "grafico", "gran", "grandes", "gratuito",
  "greenpeace", "grupo", "grupos", "gura", "ha", "haber","hace", "hacen", "hacer","hacia", "han",
  "hasta", "hay", "heat", "hecho", "hogares", "hora", "hoste", "hoy",
  "hubo", "iapg", "ibidem", "idem", "identi", "iesct", "iifap", "imagen",
  "imaginario", "imaginarios", "impacto", "impactos", "implementacion", "implica", "importancia", "importante",
  "importantes", "impuesto", "impuestos", "impulsar", "incertidumbre", "inciso", "incluir","incluso", "incremento",
  "indicador", "indicadores", "indice", "industria", "industrial", "industrias", "informacion", "informe",
  "infraestructura", "ing", "ingreso", "ingresos", "iniciativa", "instalacion", "instalada", "institucional",
  "instituciones", "instrumentos", "integral", "integrante", "interes", "intereses", "interno", "internacional",
  "introduccion", "inversion", "inversiones", "investigacion", "issn", "items", "izq", "jats",
  "jornada", "judicial", "julio", "junio", "justicia", "km", "la", "lado",
  "largo", "las", "latina", "le", "legislativo", "lewis", "ley", "leyes",
  "libre", "linea", "literatura", "litio", "llevar", "llo", "lo", "local","locales",
  "lograr", "lograron", "london", "los", "lucro", "luego","lugar", "lund", "lundqvist", "lll",
  "manera", "manuela", "maquinaria", "marco", "marcos", "margen", "marginal", "market",
  "martinez", "marzo", "mas", "materia", "materiales", "matriz", "maximo", "mayo",
  "mayor", "mayoria", "mayoritariamente", "mayores", "mbtu", "mecanismos", "mediante", "medida",
  "medidas", "medio", "medios", "mejor", "mejora", "mejorar", "menos", "menor",
  "menores", "mercado", "mercados", "mes", "meses", "metas", "metodologia", "metros",
  "mientras", "mil", "miles", "milia", "millones", "minem", "mineria", "ministerio",
  "misma", "mismas", "mismo", "mismos", "mitigacion", "mmm", "modelo", "modelos",
  "modificatoria", "modifico", "modo", "momento", "moneda", "mostrar","movilidad", "muchas", "muchos",
  "mundial", "mundo", "municipal", "muy", "nacion", "nacional", "nanciero", "nanciacion",
  "naturaleza", "necesaria", "necesario", "necesidad", "necesidades", "nen", "nes", "nicion", "ningun",
  "nivel", "niveles", "no", "norma", "normativa", "norte", "nota", "noviembre",
  "nuestra", "nuestro", "nueva", "nuevas", "nuevo", "nuevos", "num", "numero",
  "nunca", "o", "objetivo", "objetivos", "oberta", "observar", "observaciones", "obstante",
  "obtencion", "octubre", "oferta", "oficial", "orden", "org", "origen", "otorgan",
  "otra", "otras", "otro", "otros", "pags", "pais", "paises", "pagina",
  "papel", "para", "parecer","parque", "parte", "participacion", "particular", "partir", "pasado",
  "paso", "patron", "pbi", "pdf", "per","permitir","permitiria", "pequeñas", "perfil", "periodo",
  "periodos", "periodicidad", "permite", "permiten", "pero", "perspectiva", "pesar", "pese",
  "petroleo", "pib", "pist", "plan", "planificacion", "plani", "plazo", "poblacion",
  "poca", "poco", "poder", "podia", "podria", "podrian",
  "politico", "politicos", "por", "porcentaje", "porque", "posdesarrollo","poseer", "posibilidad", "posible",
  "post", "potencial", "potencia", "power", "practicas", "preci", "precio", "precios",
  "presentan", "presentar", "presente", "press", "previo","primer", "primera", "primero", "principal",
  "principales", "prioridad", "privada", "privadas", "privado", "problema", "problemas", "proceso",
  "procesos", "produccion", "productiva", "productivo", "producto", "productos", "productores", "profundacer","programa",
  "programas", "promedio", "promulga", "promover", "propia", "propicio", "propio", "propone","proponer",
  "propuesta", "provenientes", "provincia", "provincial", "provincias", "provoco", "proyecto", "proyectos",
  "publica", "publicas", "publico", "publicos", "puede", "pueden", "puesto", "punto",
  "que", "queda", "quien", "quienes", "quieren", "racional", "rda", "real",
  "realidad", "realizar", "realizado", "reciente", "recursos", "red", "redes", "referencias",
  "referentes", "region", "regional", "regiones", "reglas", "reglamento", "relacion", "relaciones",
  "relevantes", "renovable", "renovables", "renovar", "reporte", "representa", "republica", "requiere",
  "requieren", "requisitos", "reservas", "residencial", "resolucion", "respecto", "respuesta", "resultado",
  "resultados", "resulta","resultar", "resuelve", "resumen", "rev", "revista", "riesgo", "rivadavia",
  "rol", "rmar","rubro", "sanciona", "scpl", "scienti", "scientific", "se", "seccion",
  "sector", "sectores", "secretaria", "seguir","segun", "segunda", "segundo", "seguridad", "seis",
  "sellos", "sentido", "septiembre", "ser", "sera", "seria", "serie", "servicios",
  "sfv", "si", "sido", "siempre", "siendo", "significativo", "siguiente", "siguientes",
  "silva", "simenr","similares", "sin", "sino", "sistema", "sistemas", "situacion", "sobre",
  "social", "sociales", "sociedad", "solares", "solo", "solucion", "son", "sostenible",
  "spot", "spivak", "su", "subsecretaria", "subsidios", "suerte", "suma", "suministro",
  "super", "superior", "supuesto", "sur", "sus", "sustentable", "systems", "tabla",
  "tal", "tambien", "tanto", "tasa", "tco", "tecnocienti", "tecnologia", "tecnologias",
  "tema", "temas", "tematica", "tematicos", "tendencia", "tener", "tenerse", "teoria",
  "terminos", "territorio", "tes","textos", "tgs", "the", "thermal", "tiempo", "tiene",
  "tienen", "tierra", "tipo", "tipos", "titulo", "toda", "todas", "todo",
  "todos", "toma","tomar", "toneladas", "too", "torno", "total", "trabajo", "trade",
  "tradicionales", "traduccion", "transicion", "transmission", "transporte", "tras", "trata","tratar", "tratamiento",
  "trave","traves", "tres", "ubicado","ultimo", "ultimos", "un", "una", "unc", "unidas",
  "unidos", "unidad", "unidades", "universidad", "universitat", "university", "unlp", "unmdp",
  "unq", "unruh", "uno", "unos", "uoc", "urbano", "url", "usa",
  "usd", "uso", "usos", "usuarios", "usgs", "util", "utilizacion", "utilizar","va",
  "valor", "valores", "variacion", "varios", "vease", "vecinos", "velocidad", "venta",
  "ver", "vez", "via", "vida", "vigencia", "vinculados", "vision", "visto",
  "vital", "vivienda", "vol", "volumen", "vuelto", "wind", "wiser", "xml",
  "y", "ya", "yacimientos", "ypf", "zhang", "zona", "zonas"
) # Pueden agregarse las palabras que no sean relevantes para el estudio 

stop_es <- unique(c(stopwords("es"), stop_extra)) # Unificación de todos los stopwords.

# normalización de las stopwords para quitar tildes

stop_es <- chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", stop_es)

#stopwords en minúscula

stop_es <- tolower(stop_es)

# Inclucición de nombres propios para limpieza adicional

nombres_propios <- c(
  "a", "aarhus", "abadie", "abascal", "aguado", "aichele", "alexander", "alicia", "amba",
  "ana", "andersen", "arias", "barcena", "barrera", "bianchetti", "blanco", "bora","bouille",
  "bravo", "bruno", "buitrago", "buratovich", "burg","cacioppo", "camarda", "canada", "cantarero",
  "carina", "carlos", "caruana", "castelao", "catelen", "ceppi", "claromeco","clementi","crespi", "cristiano",
  "cristina", "daniel", "darwin", "david", "davidson", "del", "deloitte", "diaz", "dr",
  "duhalde", "ejecutiva", "ekman", "enrique", "esteban", "eugenia", "felbermayr", "feng","fernandez",
  "ferrer", "florencia", "florini", "fodis","fornillo", "freier", "gabriela", "galindo", "gardner",
  "garrido", "goliat", "graaf", "grubler", "gudynas", "guerrero", "guzowski", "hessling","hoste",
  "hubert", "ibarra", "indec", "james", "japon", "jasanoff", "jenkins", "jemse","jimenez", "katz",
  "kazimierski", "kim", "kirchner","kirchnerismo", "kozulj", "lacaze", "lambert", "laura", "leandro",
  "levenson", "lia", "lopez", "luca", "maria", "mariano", "marin", "marina",
  "matthieu", "mauricio","menem", "mercosur", "moralejo", "natalia", "navarro", "nestor", "oatley",
  "pendon", "pereira","pilar", "pistonesi", "porcelli", "recalde", "repsol", "rioja","roberto", "rocha",
  "rosemberg", "rosetti", "rossetti", "samaniego", "sanchez", "santos","santiago", "saul", "secretaria",
  "serrani", "shen", "simensen", "singh", "spivak", "vaca", "valle", "vanegas",
  "verre", "victoria", "vogel", "william", "yesica", "ypfb", "zabaloy"
)
          
# normalización de los nombres propios para quitar tildes

nombres_propios <- chartr("áéíóú", "aeiou", nombres_propios)
}

# ====================================================
# 9.Tokenización y lematización
# ====================================================

{# Tokenización y lematización automatizada con UDPIPE del archivo corpus

# Se descarga el modelo en español (se guarda en la carpeta de trabajo). Solo se necesita internet la primera vez.

model_file <- udpipe_download_model(language = "spanish")
ud_model_es <- udpipe_load_model(model_file$file_model)

# 'corpus' es la variable donde se encuentra todo el texto unido y limpio

x <- udpipe_annotate(ud_model_es, x = corpus)
x_df <- as.data.frame(x)

# Extracción de la columna 'lemma' que es la raíz de la palabra (ej. convierte "políticas" -> "política", "leyes" -> "ley")

tokens2 <- x_df$lemma

# Limpieza post lematización

# a. Converción a minúsculas (el lematizador puede devolver mayúsculas al inicio)

tokens2 <- tolower(tokens2)

# b. Filtros básicos de calidad sobre los lemas

# Eliminación de puntuación que se haya colado y tokens vacíos

tokens2 <- tokens2[!is.na(tokens2)]
solo_letras <- grepl("^[a-zñ]+$", tokens2) # Solo letras
tokens2 <- tokens2[solo_letras]
tokens2 <- tokens2[nchar(tokens2) > 2] # Longitud mínima

# 10.
# Aplicación y  eliminación de las stopwords y nombres propios

tokens2 <- tokens2[!(tokens2 %in% stop_es)]
tokens2 <- tokens2[!(tokens2 %in% nombres_propios)]
tokens2 # Control

# 11. 
# Guardado de los tokens de los PDFs (uno por fila)

tokens_3pdf <- data.frame(token = tokens2, stringsAsFactors = FALSE)
write.csv(tokens_3pdf, "tokens_3pdf.csv", row.names = FALSE)

# 12.
# Frecuencias de tokens y guardado

freq3pdf <- sort(table(tokens2), decreasing = TRUE)
freq_3pdf <- data.frame(palabra = names(freq3pdf), n = as.integer(freq3pdf), row.names = NULL)
write.csv(freq_3pdf, "frecuencias_tokens_3pdf.csv", row.names = FALSE)
}

# ====================================================
# 13. Armado del cuerpo de trabajo
# ====================================================

{# Construcción y guardado del corpus limpio para trabajar luego con wordvec.

corpus_final <- paste(tokens2, collapse = " ")
writeLines(corpus_final, "corpus_final_sin_stopwords.txt", useBytes = TRUE)
}

# ====================================================
# 14. Nube de Palabras 
# ====================================================

{# Formato correcto: columna 'word' y 'freq' y por tokens

wc_data <- freq_3pdf
colnames(wc_data) <- c("word", "freq")
wc_data <- wc_data[wc_data$freq > 50, ] # Solo muestra palabras que se repitan 50 veces o más

# Grafico de nube de palabras

set.seed(5424)
wordcloud2(
  data = wc_data,
  size = 1.2,                # tamaño global
  color = "random-dark",     # colores aleatorios oscuros
  backgroundColor = "white", # fondo blanco
  shape = "circle"           # otras formas: "star", "diamond", "pentagon", "cardioid"
)
}

# ====================================================
# 16. Cálculo y gráfico de TF–IDF
# ====================================================

{# El cálculo de TF–IDF se realiza sobre los textos limpios, pero sin unificarlos: se utilizan los archivos individuales (docs_sinunirlimpios), 
# que representan cada PDF. Esto permite identificar las palabras más distintivas de cada documento.

# Se crea un data frame con nombre de archivo y texto limpio

tokens_TF <- tibble(
  documento = basename(todoslos_pdf),
  texto = docs_sinunirlimpios
)

# Tokenización de cada documento

tokens_TF <- tokens_TF %>%
  unnest_tokens(palabra, texto) %>%
  filter(str_detect(palabra, "^[a-zñ]+$")) %>%
  filter(nchar(palabra) > 2) %>%
  filter(!palabra %in% stop_es) %>%
  filter(!palabra %in% nombres_propios)

# Conteo de ocurrencias por documento

frecuencias <- tokens_TF %>%
  count(documento, palabra, sort = TRUE)

# Calculamos TF–IDF

tfidf_TF <- frecuencias %>%
  bind_tf_idf(term = palabra, document = documento, n = n) %>%
  arrange(desc(tf_idf))

# Guardado del resultado

write.csv(tfidf_TF, "TFIDF_tokens_por_documento.csv", row.names = FALSE)
}

# ====================================================
# 17. Gráfico de palabras más distintivas por documento (TF–IDF)
# ====================================================

{top_tfidf <- tfidf_TF %>%
  group_by(documento) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  mutate(palabra = reorder_within(palabra, tf_idf, documento))

ggplot(top_tfidf, aes(x = palabra, y = tf_idf, fill = documento)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ documento, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(
    title = "Palabras más distintivas por documento (TF–IDF)",
    subtitle = "TF–IDF identifica los términos relevantes y específicos en cada PDF",
    x = "Término",
    y = "Peso TF–IDF"
  ) +
  theme_minimal(base_size = 13)
}

# ====================================================
# 18. BOW
# ====================================================

{# 18.1. Tokenización para bow
  # ------------------------------------------------------
  tokens_bow <- tibble(texto = corpus_final) %>%
    unnest_tokens(palabra, texto, token = "words")
  
  # Recordar que %>% (pipe) Es el operador pipe de dplyr. Significa: “pasa este resultado al siguiente paso”.
  #unnest_tokens(palabra, texto, token = "words")
  #Esta función del paquete tidytext “desanida” el texto.
  #Convierte cada palabra (token) en una fila distinta.
  #Crea una nueva columna llamada palabra.
  #El argumento token = "words" indica que queremos dividir por palabras (no por frases, ni n-gramas).
 
 # 18.2. Limpieza de tokens_bow
  
  tokens_bow_limpios <- tokens_bow %>%
    filter(!palabra %in% stop_es) %>%          # quitar stopwords
    filter(!palabra %in% nombres_propios) %>%  # quitar nombres propios
    # eliminar números, símbolos, emoticones y mantener solo letras
    filter(str_detect(palabra, "^[a-zñáéíóú]+$")) %>%
    # eliminar palabras muy cortas (como "aa", "xx" o letras sueltas)
    filter(str_length(palabra) > 2)
  

  # ------------------------------------------------------
  # 18.3. Frecuencias de palabras
  # ------------------------------------------------------
  frecuencias <- tokens_bow %>%
    count(palabra, sort = TRUE)
  
  # Mostrar las primeras 20 palabras más frecuentes
  print(head(frecuencias, 20))
  
  # ------------------------------------------------------
  # 18.4. Guardar tokens y frecuencias
  # ------------------------------------------------------
  write_csv(tokens_bow_limpios, "tokens_bow.csv")
  write_csv(frecuencias, "frecuencias_bow.csv")
  
  # ------------------------------------------------------
  # 7. Bolsa de Palabras (Bag of Words)
  # ------------------------------------------------------
  # La BoW simplemente cuenta cuántas veces aparece cada palabra
  # en el documento.Nos interesa saber, "qué palabras" y "cuán frecuentes son".
  
  bow_frecuencias <- tokens_bow %>%
    count(palabra, sort = TRUE)
  
  # Ver las 20 palabras más frecuentes
  head(bow_frecuencias, 20)
  
  # Guardamos para análisis posterior
  write_csv(bow_frecuencias, "bow_frecuencias.csv")
  
  # ------------------------------------------------------
  # 18.5. BIGRAMAS desde tokens_bow_limpios
  # ------------------------------------------------------
  
  tokens_con_pos <- tokens_bow %>%
    mutate(posicion = row_number())
  
  #mutate() agrega una nueva columna.
  #row_number() genera una secuencia: 1, 2, 3, 4,
  #Cuando queremos formar bigramas (pares de palabras consecutivas),
  #necesitamos saber qué palabra sigue a cuál.
  
  # Unimos cada palabra con la siguiente palabra según el orden original
  bigramas_df <- tokens_con_pos %>%
    mutate(palabra_siguiente = lead(palabra)) %>%
    filter(!is.na(palabra_siguiente)) %>%
    transmute(
      bigrama = paste(palabra, palabra_siguiente, sep = " ")
    )
  
  #tokens_con_pos contiene las palabras ordenadas según su posición en el texto. 
  #Luego, mutate(palabra_siguiente = lead(palabra)) crea una nueva columna con la palabra siguiente a cada una, 
  #usando lead() para “desplazar” la columna una fila hacia arriba. 
  #Así, cada palabra queda emparejada con la que la sigue.
  #transmute(bigrama = paste(palabra, palabra_siguiente, sep = " ")) combina las dos columnas (palabra y palabra_siguiente) en una sola, 
  #y están separadas por un espacio, generando una columna de bigramas como "emocion razon", "miedo control".
  
  # Ahora contamos frecuencia de cada bigrama
  bigramas_frecuencias <- bigramas_df %>%
    count(bigrama, sort = TRUE)
  
  # Observamos los top 20 bigramas
  head(bigramas_frecuencias, 20)
  
  # Guardamos bigramas en formato csv
  write_csv(bigramas_frecuencias, "bigramas_frecuencias.csv")
  
  # ------------------------------------------------------
  # 18.6. GRAFICOS BOW - BIGRAMAS y RESULTADOS GUARDADOS 
  # ------------------------------------------------------
  
  # Observamos los nombres de los archivos guardados 
  #y cargamos según nuestra ruta de archivo
  
  bow_frecuencias <- read_csv("bow_frecuencias.csv")
  bigramas_frecuencias <- read_csv("bigramas_frecuencias.csv")
  
  
  # ------------------------------------------------------
  # 18.7. Seleccionamos los 15 primeros términos para visualizar/graficar
  # ------------------------------------------------------
  
  # Top 15 palabras individuales
  top_bow <- bow_frecuencias %>%
    slice_max(n, n = 15)
  
  #slice_max(n, n = 15) ordena el data frame por la variable n (frecuencia) de mayor a menor 
  #y devuelve solo las 15 filas con valores más altos.
  
  # Top 15 bigramas
  top_bigrams <- bigramas_frecuencias %>%
    slice_max(n, n = 15)
  
  # ------------------------------------------------------
  # 18.8. Realizamos un gráfico de barras - Bolsa de Palabras
  # ------------------------------------------------------
  
  ggplot(top_bow, aes(x = fct_reorder(palabra, n), y = n)) +
    geom_col(fill = "#1f78b4") +
    coord_flip() +
    labs(
      title = "Palabras más frecuentes en 'Politica energetica'",
      subtitle = "Bolsa de Palabras (Bag of Words)",
      x = "Palabra",
      y = "Frecuencia"
    ) +
    theme_minimal(base_size = 14)
  
  # ------------------------------------------------------
  # 18.9. Realizamos un gráfico de barras - Bigramas
  # ------------------------------------------------------
  
  ggplot(top_bigrams, aes(x = fct_reorder(bigrama, n), y = n)) +
    geom_col(fill = "#ff7f00") +
    coord_flip() +
    labs(
      title = "Bigramas más frecuentes en 'Politica energetica'",
      subtitle = "Secuencias de dos palabras consecutivas",
      x = "Bigrama",
      y = "Frecuencia"
    ) +
    theme_minimal(base_size = 14)
  
  # ------------------------------------------------------
  # 18.20.- Graficamos una nube de palabras (Wordcloud)
  # ------------------------------------------------------
  # wordcloud2() requiere un data frame con dos columnas:
  #   - word (texto)
  #   - freq (frecuencia)
  
  wc_palabras <- bow_frecuencias %>%
    arrange(desc(n)) %>%
    slice_head(n = 100) %>%           # tomamos las 100 más frecuentes para la nube
    select(word = palabra, freq = n)
  
  wordcloud2(
    data = wc_palabras,
    size = 1.2,
    color = "random-light",
    backgroundColor = "black",
    shape = "star"
  )
  
  # ------------------------------------------------------
  # 18.21.  Graficamos una nube de bigramas
  # ------------------------------------------------------
  # Hacemos lo mismo pero con bigramas. En este caso, cada "palabra"
  # es en realidad una pareja de palabras como "emocion razon"
  
  wc_bigramas <- bigramas_frecuencias %>%
    arrange(desc(n)) %>%
    slice_head(n = 100) %>%
    select(word = bigrama, freq = n)
  
  wordcloud2(
    data = wc_bigramas,
    size = 1.2,
    color = "random-light",
    backgroundColor = "black",
    shape = "star"
  )
  
}

# ====================================================
# WORD EMBEDDINGS 
# ====================================================

{# Vamos a utilizar el archivo limpio y unificado corpus_final

# El modelo Word Embeddings (Word2Vec) es una técnica de aprendizaje automático que convierte las palabras en vectores numéricos 
# capaces de capturar su significado y las relaciones semánticas entre ellas.
# Word2Vec aprende a partir de los contextos en que aparecen las palabras, de modo que términos que suelen aparecer en conjunto
# quedan representados por vectores cercanos en un espacio multidimensional.
# Este modelo permite explorar cómo se agrupan los significados, identificar palabras con sentido próximo o contrario.


# Creación de un data.table con oraciones simuladas (ventanas de contexto)

tokens_split <- strsplit(corpus_final, " ")
help(strsplit)

# Token iterator o recorredor para text2vec

it <- itoken(tokens_split, progressbar = FALSE)

# Vocabulario

vocab <- create_vocabulary(it)
vocab <- prune_vocabulary(vocab, term_count_min = 5)  # filtra palabras raras
vocab

# Vectorización

vectorizer <- vocab_vectorizer(vocab)

# Modelo Word2Vec (skip-gram)

tcm <- create_tcm(it, vectorizer, skip_grams_window = 5)
glove <- GlobalVectors$new(rank = 50, x_max = 10)
w2v <- glove$fit_transform(tcm, n_iter = 10)

# Combinación de matrices (contexto + palabra)

word_vectors <- w2v + t(glove$components)

# Palabras más similares a "energia" (sin tilde ya que el texto fue sujeto al proceso de limpieza)

similarity <- sim2(x = word_vectors, y = word_vectors["energia", , drop = FALSE], method = "cosine", norm = "l2")
head(sort(similarity[,1], decreasing = TRUE), 10)

# Exploración de las relaciones semánticas.

# Busqueda de palabras similares

sim2(word_vectors, word_vectors["politica", , drop = FALSE], method = "cosine")

#Visualización  el espacio semántico.
# Reducción de las dimensiones con PCA o t-SNE:
# Se conservan solo las 100 palabras más frecuentes

freq_top <- freq_3pdf %>% slice_max(n, n = 100)

# Filtrado de los embeddings según esas palabras

words_to_plot <- rownames(word_vectors) %in% freq_top$palabra
df_pca <- as.data.frame(prcomp(word_vectors[words_to_plot, ])$x[, 1:2])
df_pca$word <- rownames(word_vectors[words_to_plot, ])

ggplot(df_pca, aes(PC1, PC2, label = word)) +
  geom_text(size = 3, alpha = 0.7) +
  labs(title = "Mapa semántico (Word2Vec reducido por PCA)") +
  theme_minimal()

# Cluster (grupos de palabras afines)

set.seed(123)
cl <- kmeans(word_vectors, centers = 5)
table(cl$cluster)

# Visualización del cluster

pca <- prcomp(word_vectors)
df_pca <- as.data.frame(pca$x[, 1:2])
df_pca$cluster <- factor(cl$cluster)

ggplot(df_pca, aes(PC1, PC2, color = cluster)) +
  geom_point(alpha = 0.7) +
  labs(title = "Clusters  de Word2Vec (PCA reducido)") +
  theme_minimal()

# Función para calcular distancia al centroide

dist_to_center <- function(cluster_id) {
  centroide <- colMeans(word_vectors[cl$cluster == cluster_id, ])
  distancias <- sim2(
    x = word_vectors[cl$cluster == cluster_id, ],
    y = matrix(centroide, ncol = ncol(word_vectors)),
    method = "cosine",
    norm = "l2"
  )
  orden <- sort(distancias[, 1], decreasing = TRUE)
  return(names(orden)[1:10])  # 10 más cercanas al centroide
}

# Obtener las 10 palabras más representativas por cluster

for (i in 1:5) {
  cat("\n Cluster", i, "→ palabras más representativas:\n")
  print(dist_to_center(i))
}

# Gráfico de los clusters (reducción con PCA)

# Reducción a 2 dimensiones

pca <- prcomp(word_vectors)
df_pca <- as.data.frame(pca$x[, 1:2])
df_pca$cluster <- factor(cl$cluster)
df_pca$word <- rownames(word_vectors)

ggplot(df_pca, aes(PC1, PC2, color = cluster)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_text_repel(aes(label = word), size = 3, alpha = 0.8, max.overlaps = 15) + # Para que las etiquetas no se encimen y quede mejor la visualización.
  labs(
    title = "Mapa de Word2Vec (reducido por PCA)",
    subtitle = "Cada color representa un cluster  detectado por k-means",
    x = "Componente principal 1",
    y = "Componente principal 2"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

####

# Se seleccionan solo algunas palabras por cluster (por ejemplo, 15)

set.seed(123)
df_pca_sample <- df_pca %>%
  group_by(cluster) %>%
  sample_n(15, replace = FALSE)

ggplot(df_pca, aes(PC1, PC2, color = cluster)) +
  geom_point(alpha = 0.4) +
  geom_text_repel(
    data = df_pca_sample,
    aes(label = word),
    size = 3,
    max.overlaps = Inf
  ) +
  labs(
    title = "Mapa  de Word2Vec (muestras representativas)",
    subtitle = "Etiquetas reducidas para evitar superposición"
  ) +
  theme_minimal()
}

# ====================================================
######### GLOVE#########
# ====================================================

{############################################################
# WORD EMBEDDINGS CON GloVe - Corpus en Español
# Objetivo: entrenar vectores semánticos con GloVe
# Se utiliza corpus_final (limpio y sin stopwords)
############################################################

# Se convierte el texto a formato lista de tokens

tokensG <- word_tokenizer(corpus_final)

# Creación de un iterador para recorrer los tokens

itG <- itoken(tokensG, progressbar = FALSE)

# Creación del vocabulario y la matriz de co-ocurrencias

# Creación del vocabulario (diccionario de palabras)

vocabG <- create_vocabulary(itG)

# Opcional: se filtran palabras muy raras o muy frecuentes

vocabG <- prune_vocabulary(vocabG, term_count_min = 15)

# Vectorización del vocabulario

vectorizerG <- vocab_vectorizer(vocabG)

# Creación de la matriz de co-ocurrencias (TCM: Term-Co-occurrence Matrix)

# ventana = 5 palabras hacia adelante y atrás

tcmG <- create_tcm(itG, vectorizerG, skip_grams_window = 5L)

#  Entrenamiento del modelo GloVe

# rank = dimensión del vector (ej. 50 o 100)

# x_max = parámetro de suavizado

glove_model <- GlobalVectors$new(rank = 50, x_max = 10)
word_vectors_main <- glove_model$fit_transform(tcmG, n_iter = 20) #Puede ser recomendable iterar entre 50 y 100

# Los vectores contextuales también aportan información

word_vectors_context <- glove_model$components
word_vectors <- word_vectors_main + t(word_vectors_context)

#  Exploración de los resultados

# Palabras más cercanas a "energia" - target

objetivo <- "energia"

# Se calculan similitudes coseno

similitudes <- sim2(
  x = word_vectors,
  y = word_vectors[objetivo, , drop = FALSE],
  method = "cosine",
  norm = "l2"
)

# Se observan las 10 palabras más similares

head(sort(similitudes[, 1], decreasing = TRUE), 10)

# Agrupamiento de las palabras en clústeres semánticos (opcional)

set.seed(123)
clG <- kmeans(word_vectors, centers = 5)
table(clG$cluster)

# Listado de algunas palabras representativas por clúster

for (i in 1:5) {
  cat("\n Cluster", i, "→ palabras más representativas:\n")
  print(head(rownames(word_vectors[clG$cluster == i, ]), 10))
}

# Visualización de los embeddings con PCA

pcaG <- prcomp(word_vectors)
df_pcaG <- as.data.frame(pcaG$x[, 1:2])
df_pcaG$word <- rownames(word_vectors)
df_pcaG$cluster <- factor(clG$cluster)

# Para evitar etiquetas amontonadas, se grafica una muestra

set.seed(123)
df_sample <- df_pcaG %>%
  group_by(cluster) %>%
  sample_n(size = min(15, n()), replace = FALSE)

ggplot(df_pcaG, aes(PC1, PC2, color = cluster)) +
  geom_point(alpha = 0.4, size = 1.8) +
  geom_text_repel(
    data = df_sample,
    aes(label = word),
    size = 3,
    max.overlaps = Inf
  ) +
  labs(
    title = "Mapa semántico (Word Embeddings con GloVe)",
    subtitle = "Corpus en español: agrupación semántica por proximidad",
    x = "Componente principal 1",
    y = "Componente principal 2"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

#  Guardamdo de resultados para uso posterior

saveRDS(word_vectors, "embeddings_glove_esp.rds")
write.csv(df_pca, "pca_glove_embeddings.csv", row.names = FALSE)
}

# ====================================================
# COMPARACIÓN SEMÁNTICA: Word2Vec vs GloVe
# ====================================================

{# Objetivo: visualizar diferencias en la organización del significado

# Reducción de ambos modelos a 2 dimensiones (PCA)

# Word2Vec (usa objeto word_vectors)

pca_w2v <- prcomp(word_vectors)
df_w2v <- as.data.frame(pca_w2v$x[, 1:2])
df_w2v$word <- rownames(word_vectors)
df_w2v$model <- "Word2Vec"

# GloVe (usa word_vectors_main + word_vectors_context)

word_vectors_glove <- word_vectors_main + t(word_vectors_context)
pca_glove <- prcomp(word_vectors_glove)
df_glove <- as.data.frame(pca_glove$x[, 1:2])
df_glove$word <- rownames(word_vectors_glove)
df_glove$model <- "GloVe"

# Se unen ambos para graficar en conjunto

df_comparativo <- rbind(df_w2v, df_glove)

# Selección de una muestra representativa de palabras

set.seed(123)
palabras_muestra <- sample(unique(df_comparativo$word), 50)
df_sample <- df_comparativo[df_comparativo$word %in% palabras_muestra, ]

# Gráfico de ambos modelos lado a lado

ggplot(df_sample, aes(PC1, PC2, color = model)) +
  geom_point(size = 2, alpha = 0.6) +
  geom_text_repel(aes(label = word), size = 3, max.overlaps = Inf) +
  facet_wrap(~model) +
  labs(
    title = "Comparación de espacios semánticos: Word2Vec vs GloVe",
    subtitle = "Cada punto representa una palabra en el espacio vectorial (reducido con PCA)",
    x = "Componente principal 1",
    y = "Componente principal 2",
    color = "Modelo"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# Guardado de los resultados para informes o visualizaciones

write.csv(df_comparativo, "comparacion_word2vec_glove.csv", row.names = FALSE)

# COMPARACIÓN SUPERPUESTA: Word2Vec y GloVe

# Ambos modelos visualizados en el mismo espacio 2D

# Preparación de los datos PCA para ambos modelos

# Word2Vec
pca_w2v <- prcomp(word_vectors)
df_w2v <- as.data.frame(pca_w2v$x[, 1:2])
df_w2v$word <- rownames(word_vectors)
df_w2v$model <- "Word2Vec"

# GloVe
word_vectors_glove <- word_vectors_main + t(word_vectors_context)
pca_glove <- prcomp(word_vectors_glove)
df_glove <- as.data.frame(pca_glove$x[, 1:2])
df_glove$word <- rownames(word_vectors_glove)
df_glove$model <- "GloVe"

# Unificación de ambos modelos

df_comparativo <- rbind(df_w2v, df_glove)

# Muestra representativa de palabras

set.seed(123)
palabras_muestra <- sample(unique(df_comparativo$word), 40)
df_sample <- df_comparativo[df_comparativo$word %in% palabras_muestra, ]


# Gráfico en un mismo plano

ggplot(df_sample, aes(PC1, PC2, color = model)) +
  geom_point(alpha = 0.7, size = 2.2) +
  geom_text_repel(aes(label = word), size = 3, alpha = 0.8, max.overlaps = Inf) +
  labs(
    title = "Comparación superpuesta de espacios semánticos: Word2Vec y GloVe",
    subtitle = "Ambos modelos en el mismo plano PCA — Corpus en español",
    x = "Componente principal 1",
    y = "Componente principal 2",
    color = "Modelo"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

#Guardado para documentación

write.csv(df_comparativo, "comparacion_superpuesta_word2vec_glove.csv", row.names = FALSE)
}