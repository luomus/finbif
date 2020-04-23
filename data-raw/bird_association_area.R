bird_assoc_area <- read.csv(text = "
  id,                                                             name, code
  ML.1088,                           'Ålands Fågelskyddsförening r.f.', AFF
  ML.1095,            'Etelä-Karjalan Lintutieteellinen Yhdistys r.y.', EKLY
  ML.1101,               'Etelä-Savon Lintuharrastajat - Oriolus r.y.', Oriolus
  ML.1091, 'Helsingin Seudun Lintutieteellinen Yhdistys - Tringa r.y.', Tringa
  ML.1110,                   'Kainuun Lintutieteellinen Yhdistys r.y.', KLY
  ML.1097,              'Kanta-Hämeen Lintutieteellinen Yhdistys r.y.', KHLY
  ML.1111,                'Kemi-Tornion Lintuharrastajat - Xenus r.y.', Xenus
  ML.1092,  'Keski- ja Pohjois-Uudenmaan Lintuharrastajat - Apus r.y.', Apus
  ML.1108,          'Keski-Pohjanmaan Lintutieteellinen Yhdistys r.y.', KPLY
  ML.1104,              'Keski-Suomen Lintutieteellinen Yhdistys r.y.', KSLY
  ML.1113,                                       'Kuusamon lintukerho', Kuusamo
  ML.1094,              'Kymenlaakson Lintutieteellinen Yhdistys r.y.', KyLY
  ML.1112,                     'Lapin Lintutieteellinen Yhdistys r.y.', LLY
  ML.1114,            'Lohjan Lintutieteellinen Yhdistys - Hakki r.y.', Hakki
  ML.1096,                      'Lounais-Hämeen Lintuharrastajat r.y.', LHLH
  ML.1107,               'Merenkurkun Lintutieteellinen Yhdistys r.y.', MLY
  ML.1098,             'Päijät-Hämeen Lintutieteellinen Yhdistys r.y.', PHLY
  ML.1099,                'Pirkanmaan Lintutieteellinen Yhdistys r.y.', PiLY
  ML.1103,         'Pohjois-Karjalan Lintutieteellinen Yhdistys r.y.', PKLY
  ML.1109,        'Pohjois-Pohjanmaan Lintutieteellinen Yhdistys r.y.', PPLY
  ML.1102,                 'Pohjois-Savon Lintuyhdistys - Kuikka r.y.', Kuikka
  ML.1090,                     'Porin Lintutieteellinen Yhdistys r.y.', PLY
  ML.1093,                         'Porvoon Seudun Lintuyhdistys r.y.', PSLY
  ML.1116,                       'Rauman Seudun Lintuharrastajat r.y.', RSLH
  ML.1127,                                'Suomen ulkopuoliset alueet', SUA
  ML.1105,               'Suomenselän Lintutieteellinen Yhdistys r.y.', SSLTY
  ML.1106,              'Suupohjan Lintutietieteellinen Yhdistys r.y.', SpLY
  ML.1089,                     'Turun Lintutieteellinen Yhdistys r.y.', TLY
", stringsAsFactors = FALSE, strip.white = TRUE, row.names = 1L, quote = "'")

class(bird_assoc_area[["name"]]) <- "translation"
class(bird_assoc_area[["code"]]) <- "translation"

n <- 1000
baa <- finbif:::api_get(
  "areas",
  list(type = "birdAssociationArea", lang = "multi", page = 1L, pageSize = n),
  FALSE
)

stopifnot(n > baa[["content"]][["total"]])

baa <- sapply(baa[["content"]][["results"]], getElement, "id")

stopifnot(identical(sort(row.names(bird_assoc_area)), sort(baa)))

