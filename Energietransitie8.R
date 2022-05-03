### De Energietransitie Doorgerekend - berekeningen horende bij de tekst
rm(list = ls())
#setwd("D:/Dropbox/Maas/klimaat/klimaatR")

# --- load necessary packages
used.packages = c("xts","reshape2","ggplot2","ggpubr","grid","gridExtra","waffle","lubridate","data.table","Rcpp","dplyr")
# install.packages(used.packages)
invisible(lapply(used.packages, library, character.only=TRUE, quietly=TRUE, verbose=FALSE))
# --- apply necessary environmental settings
Sys.setenv(TZ='UTC')
options(scipen=999)

LOCALRUN = FALSE

# https://bookdown.org/yihui/rmarkdown-cookbook/custom-blocks.html#block-shaded
# https://pandoc.org/MANUAL.html#variables-for-latex
# GitHub info: https://holtzy.github.io/Pimp-my-rmd/#use_dt_for_tables (scroll down)

# --- INITIALISATIE ------------------------------------------------------------

# defaults voor figuren etc
ag = c("#64AFFD","#4A89DC", "#4FC1E9","#31A1CB", "#48CFAD","#37BC9B", "#A0D468","#BCC152", "#FFCE54","#F68B42", "#FC6E51","#E9573F",
       "#ED5565","#DA4453", "#AC92EC","#967ADC", "#ECB7C0","#D770AD", "#F5F7FA","#E6E9ED", "#CCD1D9","#AAB2BD", "#656D7B","#434A54")
# https://rud.is/b/2015/03/18/making-waffle-charts-in-r-with-the-new-waffle-package/
rud = c("#A88855","#747372","#EDD535","#B7C99F","#a0d0de","#DF7754","#97b5cf","#CFD6E9", "#F0F1F3")
bkgrnd             = "#F3F1ED"
figuur.achtergrond = "#F6EEDD" #  #E6E7E8 "#F6EEDD" # lichter: "#F3F1ED"
plot.achtergrond   = "white"
bloe               = "#E4F1F6"
donkerbloe         = "#013A4D"
# kleuren energiemix
kleur.verliezen  = "#E8C4C4"
kleur.opgeslagen = "#C6E1BF"
kleur.hulpbron   = "#56B4E9"
kleur.ontrokken  = "darkgreen"
kleur.wind       = "#39B54A"
kleur.zon        = "#FFDE17"
# themes voor figuren. Latex/pdf stijl.
theme_maas <- function (base_size = 11, base_family = "") {
  theme_bw() %+replace% 
    theme(
      plot.title        = element_text(size = 12, face = "bold", hjust=0, margin=margin(10,0,7,0)),
      plot.subtitle     = element_text(size = 11, color="black", hjust=0, margin=margin(0,0,7,0)),
      plot.background   = element_rect(fill = figuur.achtergrond, color = NA),
      plot.margin       = margin(5, 7, 5, 7, "mm"),
      panel.grid.major  = element_line(color = figuur.achtergrond),
      panel.background  = element_rect(fill  = plot.achtergrond),
      panel.border      = element_rect(color = plot.achtergrond, fill = NA),
      axis.line         = element_line(color = figuur.achtergrond),
      axis.ticks        = element_line(color = "black"),
      axis.text         = element_text(color = "black"),
      #axis.title        = element_text(face="bold"),  # deze toegevoegd
      legend.background   = element_rect(fill  = figuur.achtergrond, color = NA),
    )
}
theme_fig_infobox <- function (base_size = 11, base_family = "") {
  figuur.achtergrond = "#D8EAF1"
  theme_bw() %+replace% 
    theme(
      plot.title        = element_text(size = 12, face = "bold", hjust = 0, margin=margin(0,0,3,0)),
      plot.subtitle     = element_text(size = 11, color="black", hjust=0, margin=margin(0,0,7,0)),
      plot.background   = element_rect(fill = figuur.achtergrond, color = NA),
      plot.margin       = margin(5, 7, 5, 7, "mm"),
      panel.grid.major  = element_line(color = figuur.achtergrond),
      panel.background  = element_rect(fill  = plot.achtergrond),
      panel.border      = element_rect(color = plot.achtergrond, fill = NA),
      axis.line         = element_line(color = figuur.achtergrond),
      axis.ticks        = element_line(color = "black"),
      axis.text         = element_text(color = "black"),
      legend.background   = element_rect(fill  = figuur.achtergrond, color = NA),
    )
}
# andere styling dingen voor ggplot
element_custom <- function() {
  structure(list(), class = c("element_custom", "element_text"))
}
element_grob.element_custom <- function(element, label="", ...)  {
  disect <- strsplit(label, "//n")[[1]]
  labels <- lapply(disect, function(x) tryCatch(parse(text=x), 
                                                error = function(e) x))
  hl <-  unit(rep(1, length(labels)), 'strheight', data=labels) + unit(0.1,"line")
  yl <- c(list(unit(0,"line")), 
          lapply(seq_along(labels[-length(labels)]), function(ii) sum(hl[1:ii])))
  
  cl <- do.call(gList, Map(function(label, ii) 
    textGrob(label, y = unit(1,"npc")-yl[[ii]], hjust=0, x=0, vjust=1), 
    label = labels, ii = seq_along(labels)))
  
  gTree(children = cl, cl="sl", heights = hl, gp=gpar(col="grey50",fontsize=8))
}
# hulpfuncties tabellen
linesep <- function(x,y=character()) {
  if(!length(x))
    return(y)
  linesep(x[-length(x)], c(rep('',x[length(x)]-1),'//addlinespace',y))  
}


# --- constanten en gekozen waarden voor de berekeningen ---
rho.air          = 1.3  # kg/m2
separatieafstand = 7    # separatie tussen windmolens in wiekdiameter
efficientie      = 0.6  # de efficientie van een veld met molens die elkaars opbrengst beïnvloeden (Meyers & Meneveau)
wiekdiameter     = 127  # energie die je uit de wind haalt is onafhankelijk van wiekdiameter. Het maakt dus niet uit wat je hier kiest!
ashoogte         = 135  # ashoogte maakt enorm uit voor de gemiddelde windsnelheid die de molen ervaart
rendement        = 0.2  # het gekozen rendement voor zonnecellen, hier nu 20%

# --- HULPFUNCTIES -------------------------------------------------------------

cel <- function(x) {as.numeric(as.vector(x))}
periodiciteit.per.jaar <- function(dataset) {
  z=periodicity(dataset)$scale
  perio=0
  if (z=="monthly") perio=12; if (z=="weekly") perio=52; if (z=="daily") perio=365; if (z=="10min") perio = 24*365
  if (z=="minute") perio = 24*365*6 # assume 10min data 
  return(perio)
}
tell <- function(...) print(paste0(...))
print.xts <- function(x, ...) {
  options(datatable.print.nrows=20)
  #options(datatable.print.rownames=FALSE)
  options(datatable.print.trunc.cols=TRUE)
  print(data.table::as.data.table(round(x, 5)))
}
print.data.frame <- function(x, ...) {
  options(datatable.print.nrows=20)
  #options(datatable.print.rownames=FALSE)
  options(datatable.print.trunc.cols=TRUE)
  print(data.table::as.data.table(x))
}
xtsmelt <- function(x) {
  df <- data.frame(
    index(x),
    coredata(x),
    stringsAsFactors=TRUE
  )
  colnames(df)[1] <- "Date"
  m <- reshape2::melt(df,"Date")
  return(m)
}
r2 <- function(x) round(x,2)
r1 <- function(x) round(x,1)
r0 <- function(x) round(x,0)
nicer <- function(x) { # "nice round", functie die getallen prettig leesbaar maakt voor de mensch
  rv = x
  if (between(x,-2, 2))          rv = round(x, 1)
  if (between(x, 2, 1000))       rv = round(x, 0)
  if (between(x, 1000, 10000))          rv = 100* round(x/100, 0)
  if (between(x, 10000, 1000000))       rv = 1000* round(x/1000, 0)
  if (between(x, 1000000, 1000000000))  rv = paste(round(x/1000000, 1), " miljoen")
  if (x>1000000000)                     rv = paste(round(x/1000000000, 1), " miljard")
  return(rv)
}

# Fast bounded cumsum, in Cpp. Anders meer dan 3 kwartier met 10 jr data. Dit
# gaat in seconden. 
cppFunction('NumericVector cumsumBounded(NumericVector x, double startvalue, double low, double high) {
    NumericVector res(x.size());
    double acc = startvalue;
    for (int i=0; i < x.size(); ++i) {
      acc += x[i];
      if (acc < low)  acc = low;
      else if (acc > high)  acc = high;
      res[i] = acc;
    }
    return res;
  }')

# --- VERBRUIK NL --------------------------------------------------------------

#https://opendata.cbs.nl/statline/#/CBS/nl/dataset/83140NED/table?dl=38AA0
# NIEUW: Zet de originele tabel van het CBS om in iets bruikbaars
Energieverbruik.NL.Jaar = "2019"
CBS2019 = fread(file="CBS_energieomzettingEnVerbruik_2019.csv", skip=3, header=TRUE)
CBS2019 = CBS2019[2:nrow(CBS2019), ]
CBS2019[CBS2019=="."] <- NA
CBS2019[CBS2019==""] <- NA
CBS2019[] = lapply(CBS2019, function(l){gsub(",",".", l)})
CBS2019[, 3:ncol(CBS2019)] = lapply(CBS2019[, 3:ncol(CBS2019)], function(l){as.numeric(l)})
# TO DO: omzetting naar ebalans zoals hieronder zodat energieverbuik waffles gemaakt kunnen worden


# data is oorspronkelijk van CBS website, en omgezet in een CSV.
# Data lezen, schoonmaken en converteren
ebalans <- read.csv(file="omzetting naar verbruik 2019.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
# De totale hoeveelheid gebruikte energie in NL in 2019: 3047 PJ.
balanstotaal = ebalans$Totaal[1]

# hernieuwbare energie uitgesplitst 
balansdeel.hernieuwbaar <- read.csv(file="CBS 2019 Hernieuwbare energie.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
balansdeel.hernieuwbaar = balansdeel.hernieuwbaar[2:ncol(balansdeel.hernieuwbaar)]

# totaalverbruik in de loop der jaren

verbruikshistorie <- read.csv(file="Energiebalans_tot energieverbruik in de loop der jaren.csv", header=TRUE, sep=",", stringsAsFactors = TRUE)
colnames(verbruikshistorie) = gsub("X", "", colnames(verbruikshistorie))


# --- ALGEMENE CIJFERS EN DEFINITIES  ------------------------------------------

# --- algemeen
Oppervlakte.Nederland.km2                         = 41873
Oppervlakte.exclusieve.economische.zone.Nederland = 57000 # bron: https://nl.wikipedia.org/wiki/Nederlandse_Exclusieve_Economische_Zone
aantal.inwoners.NL                                = 17000000
m.per.km2                                         = 1000^2
J.per.MJ = 1e6
J.per.PJ = 1e15
terawatt = 1e12; gigawatt = 1e9; megawatt = 1e6; kilowatt = 1e3

# --- definities uit MacKay / Sustainable Energy ---
kWh.in.J = 3.6e6 # 3600*1000  
W.naar.kWh.per.dag  <- function(watt) { (watt/1000)*24 }
kWh.per.jaar.naar.W <- function(kWh.jaar) { 1000 * (kWh.jaar/365/24) }
TWh.jr.naar.GW      <- function(x) { 1000 * x/(365*24)}

# Verbruikscijfers Nederland
Energieverbruik.NL.PJ              = CBS2019$`Totaal energiedragers`[1] - CBS2019$`Totaal energiedragers`[nrow(CBS2019)]  # trek niet-energetisch verbruik af van totaalverbruik (2019) 
Energieverbruik.NL.kWh             = Energieverbruik.NL.PJ * J.per.PJ / kWh.in.J
Energieverbruik.NL.TWh             = Energieverbruik.NL.kWh/1e9
Energieverbruik.NL.GW              = (Energieverbruik.NL.kWh/1e6)/24/365
Energieverbruik.NL.in.kWh.per.dag  = Energieverbruik.NL.PJ * J.per.PJ / 365 / kWh.in.J
Energieverbruik.NL.in.kWh.per.jaar = Energieverbruik.NL.in.kWh.per.dag*365
Energieverbruik.NL.in.TWh.per.jaar = (Energieverbruik.NL.in.kWh.per.jaar*1000)/1e12
Energieverbruik.NL.in.kWh.per.dag.per.persoon = Energieverbruik.NL.in.kWh.per.dag / aantal.inwoners.NL

# --- Pumped Hydro Storage 
wereld.geinstalleerde.capaciteit.kWh = 1.6*1e9 # 1.6 TWh, TW=1e12, kW = 1e3 
jaarfractie                   = wereld.geinstalleerde.capaciteit.kWh/Energieverbruik.NL.in.kWh.per.jaar
aantaldagenopslag             = jaarfractie * 365
PSH.mondiaal.aantalurenopslag = aantaldagenopslag*24
TWh.2.mondiale.cap <- function(TWh) { TWh/1.6 }

# lithium
TWh.2.Hornsdale   <- function(TWh) { 1000 * TWh / 0.194 } # Hornsdale heeft een capaciteit van 194 MW, dus 0,194 GW 
TWh.2.NLwagenpark <- function(TWh) { TWh / ((82*8.7e6)/1e9)}   # 82 kWh https://en.wikipedia.org/wiki/Tesla_Model_3


# --- WINDENERGIE --------------------------------------------------------------
# praktijk: https://www.windparknoordoostpolder.nl/achtergrondinformatie/feiten-en-cijfers/

# Deze functie rekent bij een bepaalde gegeven windsnelheid het geïnterpoleerde
# vermogen uit. Dat gebeurd door de waarden uit de power curve (bijv. de Enercon
# 126) lineair te interpoleren. 
interpolate.E126.power.kW <- function(
  target = 13,
  lookup = Power.curve.Enercon.126$Wind.ms,
  values = Power.curve.Enercon.126$power.kW) 
{
  ptr     = length(lookup[lookup <= target])
  percent = (target-lookup[ptr])/(lookup[ptr+1] - lookup[ptr])
  answer  = values[ptr] + percent*(values[ptr+1]-values[ptr]) 
  return(answer)
}  

# uit MacKay (2009)
separatie.windmolens <- function(diameter) { separatieafstand * diameter }
wind.power.W.per.m2.rotoroppervlak <- function(wind.speed.ms) { 0.5 * rho.air * wind.speed.ms^3}
wind.power.per.wiekdiameter <- function(diameter, efficientie, gem.windsnelheid) {efficientie * wind.power.W.per.m2.rotoroppervlak(gem.windsnelheid) * 0.25 * pi * diameter^2 }
grondoppervlakte.windmolen.m2 <- function(diameter) { separatie.windmolens(diameter)^2 }

# ZIE OOK VOOR WIND OP ZEE EN GROTE TURBINES: https://www.maritiemnederland.com/nieuws/ook-op-de-noordzee-geldt-meten-is-weten
# https://geminiwindpark.nl/about-gemini-wind-park.html#o2 (op zee)
# Gemini: 600 MW capacity, 2.6 TWh/jaar, 68 km2
productie.gemini.W.m2 = kWh.per.jaar.naar.W(2.6e9) / (68*1000^2)

# windkaart NL 100m hoogte http://www.klimaatatlas.nl/klimaatatlas.php?wel=wind&ws=kaart&wom=Gemiddelde%20windsnelheid%20100%20m
gem.windsnelheid.NL.2004.2013 = rbind( # http://bibliotheek.knmi.nl/knmipubTR/TR351.pdf
  c("Cabauw",10,4.2),
  c("Cabauw",40,5.2),
  c("Cabauw",80,6.8),
  c("Cabauw",100,7.25),
  c("Cabauw",200,8.5),
  c("Wieringermeer",10,5.1),
  c("Wieringermeer",40,6.6),
  c("Wieringermeer",80,7.6),
  c("Wieringermeer",100,8.1),
  c("Wieringermeer",200,9.2),
  c("OWEZ",10,8),
  c("OWEZ",40,8.75),
  c("OWEZ",80,9.25),
  c("OWEZ",100,9.5),
  c("OWEZ",200,11)
)
gem.windsnelheid.NL.2004.2013 = data.frame(
  meetmast     = gem.windsnelheid.NL.2004.2013[,1],
  hoogte       = as.numeric(gem.windsnelheid.NL.2004.2013[,2]),
  windsnelheid = as.numeric(gem.windsnelheid.NL.2004.2013[,3]),
  stringsAsFactors = FALSE
)
windsnelheid.op.ashoogte <- function(ashoogte, meetmast) {
  # lineair extrapoleren van data
  dta = gem.windsnelheid.NL.2004.2013[gem.windsnelheid.NL.2004.2013$meetmast==meetmast,]
  bot = tail(dta[dta$hoogte<=ashoogte, ], 1)
  top = head(dta[dta$hoogte>=ashoogte, ], 1)
  if (bot$hoogte==top$hoogte) {
    interpolatie = dta$windsnelheid[dta$hoogte==ashoogte & dta$meetmast==meetmast]
  } else {
    verschil.ashoogte.bot = ashoogte - bot$hoogte
    verschil.bot.top = top$hoogte - bot$hoogte
    procentverschil = verschil.ashoogte.bot/verschil.bot.top
    interpolatie = procentverschil*(top$windsnelheid-bot$windsnelheid) + bot$windsnelheid
  }
  return(interpolatie)
}


# --- OPBRENGST WINDMOLEN IN DE PRAKTIJK 

# Power Curve Enercon-126 (hub 135m, rotordiam. 126m). Powercurve van enercon zelf.
# Windsnelheid op hubhoogte. Uitgeschakeld tussen 28 en 34 ms. We gaan maar uit van 34.  
Power.curve.Enercon.126 = read.csv(file="enercon_126_power_curve.csv", sep=",", skip=1)

# --- check enercon 126 tov wet van Betz
# windmolen levert 0.3 v^3 / s^2 W/m2, s=7
enercon.126.in.W.m2 <- function(windspeed, separatieafstand=7, efficientie=0.6) { 
  kW = interpolate.E126.power.kW(windspeed)     # power in kW. Deze stap is langzaam 
  enercon.bezet.m2 = (separatieafstand*126)^2   # sd^2
  W.m2 = (kW * 1000) / enercon.bezet.m2         # maak van kilowatt watt en deel door aantal vierkante meters
  
  # Dan de efficientie van een windmolenpark bij een bepaalde separatieafstand.
  # Hier gebruiken we voorlopig Horns Rev, die op s=7 een efficiëntie van 60% bereikt
  if (separatieafstand==7) { W.m2 = W.m2 * efficientie } 
  
  return(W.m2)
}
Betz.in.W.m2        <- function(windspeed, separatieafstand=7, efficientie=0.6) { 
  W.m2 = 0.3*windspeed^3 / separatieafstand^2
  
  # Dan de efficientie van een windmolenpark bij een bepaalde separatieafstand.
  # Hier gebruiken we voorlopig Horns Rev, die op s=7 een efficiëntie van 60% bereikt
  # 
  # ONDERZOEKER: https://pages.jh.edu/cmeneve1/ mailen voor data separatie vs efficientie?
  if (separatieafstand==7) { W.m2 = W.m2 * efficientie } 
  
  return(W.m2)
}

# breid de tabel uit zodat we enercon in w/m2 hebben en Betz ernaast
e126.W.m2 = sapply(Power.curve.Enercon.126$Wind.ms, FUN=enercon.126.in.W.m2)
betz.W.m2 = sapply(Power.curve.Enercon.126$Wind.ms, FUN=Betz.in.W.m2)
Power.curve.Enercon.126$e126.W.m2 = e126.W.m2
Power.curve.Enercon.126$betz.W.m2 = betz.W.m2
Power.curve.Enercon.126[is.na(Power.curve.Enercon.126)] <- 0
dta = Power.curve.Enercon.126[1:17, ]
ggplot(dta) + 
  geom_line(aes(x=Wind.ms, y=e126.W.m2)) + 
  geom_line(aes(x=Wind.ms, y=betz.W.m2), color="blue")


bezettingsoppervlakte.windmolen.m2      = grondoppervlakte.windmolen.m2(wiekdiameter)
aantal.windmolens.per.km2               = ((1000^2)/bezettingsoppervlakte.windmolen.m2)

# gebruikt in tekst, cabauw 135
opp.enercon.126.km2 = (127*separatieafstand)^2 / 1e6


# --- IMPORTEREN KNMI DATA OP 10 MIN RESOLUTIE ---------------------------------

# GitHub staat bestand groter dan 100 MB kennelijk niet toe, de winddata is daarom
# hier in tweeën geknipt.
xwr1       = fread(file="KNMI_cabauw_10min_wind_1.csv")
windcabauw = xts(xwr1[, 2:6], as.POSIXct(xwr1$index))
xwr2       = fread(file="KNMI_cabauw_10min_wind_2.csv")
windcabauw = rbind(windcabauw, xts(xwr2[, 2:6], as.POSIXct(xwr2$index)))
xzr        = fread(file="KNMI_cesar_10min_zon_straling.csv") # KNMI_cabauw_10min_zon_straling.csv
zoncabauw  = xts(xzr[,2], as.POSIXct(xzr$index))
# merge beide datasets, vul lege plekken op, en verwijder overblijvende NA's
KNMI_wind_zon_10min = na.omit(na.locf(merge(zoncabauw, windcabauw)))

# breng de nauwkeurigheid van de windsnelheid terug naar 2 cijfers achter de 
# komma, zodat er minder unieke waarden zijn, zodat het rekenwerk wat versnelt. 
# We gaan hier toch niet voor hoge precisie.
KNMI_wind_zon_10min[, 2:6] = round(KNMI_wind_zon_10min[, 2:6], 1)

# straling converteren naar meer bruikbare eenheden: kWh/km2, zelfde eenheid als voor wind
# Straling is nu in W/m2 per 10min, naar khw/m2 = s*6/1000, dan maal een miljoen voor km2
KNMI_wind_zon_10min$straling.Wm2 = KNMI_wind_zon_10min$short.wave.downward.radiation.W.m2

# --- data uit breiden met de opbrengsten (vermogens) voor wind 
# LET OP: we nemen hier de windsnelheid op 140m hoogte van Cabauw, de E-126 heeft
# een hub-hoogte van 135m, daarop is de data-serie afgestemd.
# -- eerst Betz
KNMI_wind_zon_10min$Betz.Wm2 = Betz.in.W.m2(KNMI_wind_zon_10min$wind140m, separatieafstand=separatieafstand, efficientie=efficientie)
# -- dan de praktijk: Enercon-126
unieke.waarden = sort(unique(KNMI_wind_zon_10min$wind140m))
# maak een kolom waar de enercon powercurve-waarden in staan, zodat we later via separatieafstand en 
# efficientie snel opnieuw kunnen berekenen.
fp <- function(w) { interpolate.E126.power.kW(w) }
uw.P.E126.W = 1000 * sapply(unieke.waarden, FUN=fp) 
KNMI_wind_zon_10min$Vermogen.Enercon126.W = NA
for (u in 1:length(unieke.waarden)) {
  KNMI_wind_zon_10min$Vermogen.Enercon126.W[KNMI_wind_zon_10min$wind140m==unieke.waarden[u]] <- uw.P.E126.W[u] 
}
# nu we de power-waarden voor de enercon hebben bij een bepaalde windsneldheid, doen we
# nu de stap die separatieafstand en efficientie meenneemt
enercon.bezet.opp.m2 = (separatieafstand*126)^2   # sd^2
KNMI_wind_zon_10min$Enercon126.Wm2 = efficientie * (KNMI_wind_zon_10min$Vermogen.Enercon126.W / enercon.bezet.opp.m2)  # Watten van de Enercon gedeeld door opp waar-i op staat, maal de efficientie van 't veld

# snellere invulling W/m2 enercon (duurt nog steeds een paar minuten):
# fe <- function(w) { enercon.126.in.W.m2(w, separatieafstand=separatieafstand, efficientie=efficientie) }
# u.2.e126.W.m2 = sapply(unieke.waarden, FUN=fe)
# KNMI_wind_zon_10min$Enercon126.Wm2 = NA
# for (u in 1:length(unieke.waarden)) {
#   KNMI_wind_zon_10min$Enercon126.Wm2[KNMI_wind_zon_10min$wind140m==unieke.waarden[u]] <- u.2.e126.W.m2[u] 
# }

# reality check: wat zijn de gemiddelde opbrengsten per uur voor Betz en Enercon?
# (liggen nu op de 2 a 3 W/m2, dat is aannemelijk) 
vermogen.Betz.10min = mean(KNMI_wind_zon_10min$Betz.Wm2, na.rm=TRUE)
vermogen.E126.10min = mean(KNMI_wind_zon_10min$Enercon126.Wm2, na.rm=TRUE)

# reality check: gem. windsnelheid op 135m zou rond 7,7 m/s moeten zijn. Dat is 7.8, in deze dataset, netjes dus
avg.wind.135m = mean(KNMI_wind_zon_10min$wind140m)

# nu de opbrengsten per 10min berekenen in kWh per km2
# LET OP: omdat dit nu 10 min data is en niet uurdata, moet er opgelet worden op de kilowattUUR.
# we gaan van w/m2 naar kwh/km2, dus maal miljoen voor vierkante meters, gedeeld door 1000 voor kilowatt
# en gedeeld door 6 om 10min te verrekenen naar uur (in 10 min breng je maar een
# zesde van de hoeveelheid kWh op die je in een uur zou doen).
KNMI_wind_zon_10min$opbrengst.Betz.kWh.km2       = (KNMI_wind_zon_10min$Betz.Wm2/6 * 1e6) / 1000  
KNMI_wind_zon_10min$opbrengst.Enercon126.kWh.km2 = (KNMI_wind_zon_10min$Enercon126.Wm2/6 * 1e6) / 1000  
KNMI_wind_zon_10min$opbrengst.zonnecel.kWh.km2   = (rendement*KNMI_wind_zon_10min$short.wave.downward.radiation.W.m2/6 * 1e6) /1000
# reality check op de uitkomsten. Zon zit op rond de 1000 kWh/m2 per jaar
s = sum(KNMI_wind_zon_10min$short.wave.downward.radiation.W.m2["2008"]/6/1000) # in de 1000, mooi


# --- WINDFLUCTUATIES ----------------------------------------------------------

# Selecteer de juiste opbrengstdata voor de gegevens periode en gegeven soort 
# (theoretische wind of windmolen in de praktijk). De manier waarop we dat hier
# doen vormt een verbetering op de vorige versie. In de vorige versie weer eerst
# de windsnelheid gemiddeld en dan het vermogen bepaald. Maar nu IS de data al
# uitgebeid met uurdata voor opbrengst, en deze opbrengsten kunnen nu eenvoudig
# gesommeerd woerden. Zo klopt de opbrengst OOK op grotere tijdschalen, bijv per 
# maand. Iedere schaal maakt dus gebruik van de uurdata in feite en er is geen
# verschil meer tussen de optelling op maandbasis en die op uurbasis voor een 
# bepaalde periode.

Bereken.Opbrengst.Wind2 <- function(PERIODICITEIT="monthly", periode="2006::2015", soort=c("Betz","Enercon-126")) {
  
  # --- creer gem wind met een bepaalde periodiciteit, bijv. maandelijks
  if (PERIODICITEIT=="monthly") wind = apply.monthly(KNMI_wind_zon_10min[periode, "wind140m"], FUN=mean)
  if (PERIODICITEIT=="weekly")  wind = apply.weekly(KNMI_wind_zon_10min[periode, "wind140m"], FUN=mean)
  if (PERIODICITEIT=="daily")   wind = apply.daily(KNMI_wind_zon_10min[periode, "wind140m"], FUN=mean)
  if (PERIODICITEIT=="10min")   wind = KNMI_wind_zon_10min[periode, "wind140m"]
  colnames(wind) = "wind140m"
  
  # --- creer opbrenst met een bepaalde periodiciteit, bijv. maandelijks
  if (PERIODICITEIT=="monthly" & soort=="Betz")        opbr = apply.monthly(KNMI_wind_zon_10min[periode, "opbrengst.Betz.kWh.km2"], FUN=sum)
  if (PERIODICITEIT=="monthly" & soort=="Enercon-126") opbr = apply.monthly(KNMI_wind_zon_10min[periode, "opbrengst.Enercon126.kWh.km2"], FUN=sum)
  if (PERIODICITEIT=="weekly"  & soort=="Betz")        opbr = apply.weekly(KNMI_wind_zon_10min[periode, "opbrengst.Betz.kWh.km2"], FUN=sum)
  if (PERIODICITEIT=="weekly"  & soort=="Enercon-126") opbr = apply.weekly(KNMI_wind_zon_10min[periode, "opbrengst.Enercon126.kWh.km2"], FUN=sum)
  if (PERIODICITEIT=="daily"   & soort=="Betz")        opbr = apply.daily(KNMI_wind_zon_10min[periode, "opbrengst.Betz.kWh.km2"], FUN=sum)
  if (PERIODICITEIT=="daily"   & soort=="Enercon-126") opbr = apply.daily(KNMI_wind_zon_10min[periode, "opbrengst.Enercon126.kWh.km2"], FUN=sum)
  if (PERIODICITEIT=="10min"   & soort=="Betz")        opbr = KNMI_wind_zon_10min[periode, "opbrengst.Betz.kWh.km2"]
  if (PERIODICITEIT=="10min"   & soort=="Enercon-126") opbr = KNMI_wind_zon_10min[periode, "opbrengst.Enercon126.kWh.km2"]
  colnames(opbr) = "opbrengst.kWh.km2"
  
  wind = merge(wind, opbr)
  
  # Bereken het periodieke productie. Let op, moet genormaliseerd
  # worden tov het Nederlandse jaargebruik. We stellen in de hele berekening
  # dat de gemiddelde opbrengst gelijk moet zijn aan het totale NL verbuik. 
  perio = periodiciteit.per.jaar(wind) 
  gemiddelde.opbrengst = mean(wind$opbrengst.kWh.km2) # OK, het opbrengstgemiddelde is nu afgesteld op ons gebruik. Nu kunnen we over tekorten gaan praten.
  jaaropbrengst = gemiddelde.opbrengst*perio
  # bereken dan de opbrengst voor ieder datapunt t.o.v. de gemiddelde jaarproductie
  wind$productie.pct.jr = 100 * (wind$opbrengst.kWh.km2 / jaaropbrengst)
  
  # Overschot: overschotten en tekorten, genormaliseerd tov het gemiddelde (oude visualisatie)
  wind$overschot.pct.jr = wind$productie.pct.jr - (100/perio)
  
  return(wind)
}


# ---- hulpfuncties tekorten

# grafiek normeert de opbrengst tov jaarverbruik, niet maandverbruik zoals eerder (dus jaar=100%, niet maand=100%)
grafiek.tekort <- function(windjaar, title="Fluctuaties", subtitle="tov constant verbruik", toontekst=TRUE, toonbuffer=TRUE, rounding=0) {
  perio = periodiciteit.per.jaar(windjaar)
  p <- ggplot(windjaar, aes(x=Index,y=productie.pct.jr)) +
    theme_maas() +
    geom_bar(stat="identity", aes(fill=productie.pct.jr>(100/perio))) + 
    geom_hline(yintercept=100/perio, linetype="dashed", color="tomato2") +
    scale_fill_manual(values = c('tomato2', 'green') ) +
    labs(title=paste0(title), # ,gsub("::","-",jaar) 
         subtitle = subtitle) +
    theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_text(size = 9)) +   
    ylab("Opbrengst (%)")
  
  if (toontekst) { p <- p + geom_text(aes(x=Index, y=productie.pct.jr/1.5, label = round(productie.pct.jr, rounding)), angle=90) }
  if (toonbuffer) {
    p <- p + 
      geom_line(aes(x=Index, y=buffer), color="tomato2") +
      geom_point(aes(x=Index, y=buffer), color="tomato2") 
  }
  
  return(p)
}


# maak een plot van een (meer)jaarse grafiek, waarbij het cumulatieve overschot
# wordt ingetekend en de benodigde (cumulatieve) buffergrootte   
grafiek.tekort.en.buffer <- function(windjaar, title="Fluctuaties", subtitle="tov constant verbruik", toonbuffer=TRUE) {
    p <- ggplot(windjaar, aes(x=Index,y=overschot)) +
      theme_maas() +
      geom_bar(stat="identity", aes(fill=overschot>0)) + 
      # geom_line(aes(x=Index, y=lopend_overschot), color="green") +
      # geom_point(aes(x=Index, y=lopend_overschot), color="green") +
      geom_text(aes(x = Index, 
                    y = overschot/2, label = round(overschot)), angle=0) +
      scale_fill_manual(values = c('tomato2', 'green') ) +
      labs(title=paste0(title), # ,gsub("::","-",jaar) 
           subtitle = subtitle,
           caption  = "Data: KNMI Data Platform")+
      theme(legend.position = "none",
            axis.title.x = element_blank(), axis.title.y = element_text(size = 9)) +   
      ylab("Tekort of overschot (%)")
    
    if (toonbuffer) {
      p <- p + 
        geom_line(aes(x=Index, y=buffer), color="tomato2") +
        geom_point(aes(x=Index, y=buffer), color="tomato2") 
    }
    
    return(p)
}

# Deze grafiek moet het volgende gaan weergeven: voor iedere beschouwde periode
# berekenen we eerst de minimale benodigde accu- of reservecapaciteit die nodig is 
# om de tekorten op te vangen. Dan loop je de periode door en bepaald hoe "vol"
# de accu op ieder moment nog zou zijn. Dat geef je weer, als een accu op een smartphone.
grafiek.buffercapaciteit <- function(windjaar, title="Laadtoestand denkbeeldige accu", subtitle="", caption="", toonbuffer=TRUE) {
  # perio = periodiciteit.per.jaar(windjaar)
  reservecapaciteit = max(windjaar$benodigde_buffer)
  p <- ggplot(windjaar) +
    theme_maas() +
    scale_fill_manual(values = c(bkgrnd, 'green') ) +
    geom_bar(stat="identity", aes(x=Index, y=reservecapaciteit, fill=bkgrnd)) +
    geom_bar(stat="identity", aes(x=Index,y=(reservecapaciteit-benodigde_buffer), fill='green')) + 
    labs(title=title, subtitle=subtitle, caption=caption) +
    theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_text(size = 9)) +   
    ylab("Laadtoestand (% van jaarverbruik)")

  return(p)
}
grafiek.buffercapaciteit2 <- function(windjaar, title="Laadtoestand denkbeeldige accu") {
  # perio = periodiciteit.per.jaar(windjaar)
  reservecapaciteit = max(windjaar$benodigde_buffer)
  p <- ggplot(windjaar) +
    theme_maas() +
    scale_fill_manual(values = c(bkgrnd, kleur.ontrokken) ) +
    geom_bar(stat="identity", aes(x=Index, y=reservecapaciteit, fill=bkgrnd)) +
    geom_bar(stat="identity", aes(x=Index,y=(reservecapaciteit-benodigde_buffer), fill=kleur.ontrokken)) + 
    labs(title=title) +
    theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_text(size = 9)) +   
    ylab("Laadtoestand (%)")
  
  return(p)
}

# hoeveel buffer had je moeten aanhouden om de tekorten te kunnen 
# bijbenen? Vind het grootste aaneengesloten tekort, in de gehele periode. 
# HEEL LANGZAME CODE, OUD, MAAR HEEFT HET VOORDEEL DAT T START MET VOLLE ACCU.
cumulatief.tekort <- function(overschotpercentages, VERBOSE=FALSE) {
  perio = periodiciteit.per.jaar(overschotpercentages)
  running_tekort = 0 # deze is voor de functie
  cum_tekort     = 0
  benodigde_buffer  = NULL # deze voor de rapportage
  EERSTE_TEKORT_BEREIKT = FALSE
  for (m in seq_along(overschotpercentages)) {
    # begin tekorten te tellen als het eerste tekort zich aandiend. In die eerste
    # maand had in elk geval het huidige maandtekort in de buffer moeten zitten.
    if (!EERSTE_TEKORT_BEREIKT & cel(overschotpercentages[m]) < 0) {
      EERSTE_TEKORT_BEREIKT = TRUE
      running_tekort = cel(overschotpercentages[m])
      if (running_tekort<cum_tekort) cum_tekort = running_tekort
      if (VERBOSE) tell("1e tekort bereikt: ", running_tekort, " ", index(overschotpercentages)[m]) 
    } else
      if (EERSTE_TEKORT_BEREIKT) {
        running_tekort = running_tekort + cel(overschotpercentages[m])
        if (running_tekort < cum_tekort) cum_tekort = running_tekort
        if (VERBOSE) tell("lopend tekort: ", running_tekort, " ", index(overschotpercentages)[m]) 
      }
    
    # als het tekort omslaat in een overschot, dan gaan we op zoek naar het
    # volgen de tekort. Misschien gaat die wel dieper zijn dan cum_tekort (de max)
    if (running_tekort>0 & EERSTE_TEKORT_BEREIKT==TRUE) {
      EERSTE_TEKORT_BEREIKT=FALSE
      if (VERBOSE) tell("Reset: ", running_tekort, " ", index(overschotpercentages)[m]) 
    }
    
    # track & trace
    benodigde_buffer = rbind(benodigde_buffer, xts(running_tekort, index(overschotpercentages)[m]))
  }
  return(list(max_tekort=cum_tekort, benodigde_buffer=benodigde_buffer))
}
# Snelle oplossing voor het cumulatieve tekort. Accu heeft z'n volle ladingstoestand
# ergens gedurende de 'rit', afhankelijk van hoe de wind staat :)
cumulatief.tekort2 <- function(overschotpercentages, VERBOSE=FALSE) {
  cs = cumsum(overschotpercentages)
  cs = cs + abs(min(cs))
  cs = cs - max(cs)
  return(list(max_tekort=min(cs), benodigde_buffer=cs))
}
  
uitbreiden.dataserie.met.tekorten <- function(dataserie, benodigde_buffer) {
  dataserie$benodigde_buffer = benodigde_buffer
  dataserie$benodigde_buffer[dataserie$benodigde_buffer>0] <- 0
  dataserie$benodigde_buffer = -dataserie$benodigde_buffer
  dataserie$lopend_overschot = cumsum(dataserie$overschot)
  return(dataserie)
}

if (LOCALRUN) {
  
# meetperiode 2006-2015
wind20062015 = Bereken.Opbrengst.Wind2(PERIODICITEIT="monthly", periode="2006::2015", soort="Betz")
ct           = cumulatief.tekort2(wind20062015$overschot.pct.jr)
wind20062015 = uitbreiden.dataserie.met.tekorten(wind20062015, ct$benodigde_buffer)
wind20062015$buffer = cummax(wind20062015$benodigde_buffer)
benodigde.buffergrootte.20062015.pct.jr = max(max(wind20062015$buffer), max(wind20062015$lopend_overschot))
index(wind20062015) = as.Date(format(index(wind20062015), "%Y-%m-15"))
plot.tekort.wind.20062015 =  grafiek.tekort(wind20062015, 
                                        title    = paste0("Figuur 3. Opbrengstvariatie (Betz)"),
                                        subtitle = paste0("Opbrengst als percentage van jaaropbrengst"),
                                        toonbuffer = FALSE)
gbuffer.20062015 = grafiek.buffercapaciteit(wind20062015)
grid.arrange(plot.tekort.wind.20062015, gbuffer.20062015, ncol = 1, heights = c(2, 1))

# 2006-2015 Enercon MONTHLY
wind20062015E = Bereken.Opbrengst.Wind2(PERIODICITEIT="monthly", periode="2006::2015", soort="Enercon-126")
ct            = cumulatief.tekort2(wind20062015E$overschot.pct.jr)
wind20062015E = uitbreiden.dataserie.met.tekorten(wind20062015E, ct$benodigde_buffer)
wind20062015E$buffer = cummax(wind20062015E$benodigde_buffer)
benodigde.buffergrootte.20062015E.pct.jr = max(max(wind20062015E$buffer), max(wind20062015E$lopend_overschot))
index(wind20062015E) = as.Date(format(index(wind20062015E), "%Y-%m-15"))
plot.tekort.wind.20062015E =  grafiek.tekort(wind20062015E, 
                                            title    = paste0("Figuur 7. Simulatie van tien jaar windenergie met opslag"),
                                            subtitle = paste0("Scenario waarin alle energie in Nederland wordt opgewekt via wind (Enercon-126)"),
                                            toonbuffer = FALSE, toontekst=FALSE)
gbuffer.20062015E = grafiek.buffercapaciteit(wind20062015E)
grid.arrange(plot.tekort.wind.20062015E, gbuffer.20062015E, ncol = 1, heights = c(1.5, 1))


# 2007 met Betz
wind2007 = Bereken.Opbrengst.Wind2(PERIODICITEIT="monthly", periode="2007", soort="Betz")
ct       = cumulatief.tekort(wind2007$overschot.pct.jr)
wind2007 = uitbreiden.dataserie.met.tekorten(wind2007, ct$benodigde_buffer)
wind2007$buffer = cummax(wind2007$benodigde_buffer)
benodigde.buffergrootte.2007.pct.jr = max(max(wind2007$buffer), max(wind2007$lopend_overschot))
index(wind2007) = as.Date(format(index(wind2007), "%Y-%m-15"))
plot.tekort.wind.2007 =  grafiek.tekort(wind2007, 
                                        title    = paste0("Figuur 3. Opbrengstvariatie (Betz)"),
                                        subtitle = paste0("Opbrengst als percentage van het jaarverbuik (buffer: ", round(benodigde.buffergrootte.2007.pct.jr),"%)"),
                                        toonbuffer = FALSE)
gbuffer.2007 = grafiek.buffercapaciteit(wind2007)
grid.arrange(plot.tekort.wind.2007, gbuffer.2007, ncol = 1, heights = c(2, 1))

# 2007 met Enercon
wind2007E = Bereken.Opbrengst.Wind2(PERIODICITEIT="monthly", periode="2007", soort="Enercon-126")
ct        = cumulatief.tekort(wind2007E$overschot.pct.jr)
wind2007E = uitbreiden.dataserie.met.tekorten(wind2007E, ct$benodigde_buffer)
wind2007E$buffer = cummax(wind2007E$benodigde_buffer)
benodigde.buffergrootte.2007E.pct.jr = max(max(wind2007E$buffer), max(wind2007E$lopend_overschot))
#index(wind2007E) = as.Date(format(index(wind2007E), "%Y-%m-15"))
plot.tekort.wind.2007E =  grafiek.tekort(wind2007E, 
                                         title    = paste0("Figuur 3. Opbrengstvariatie (Enercon-126)"),
                                         subtitle = paste0("Opbrengst als percentage van het jaarverbuik (buffer: ", round(benodigde.buffergrootte.2007E.pct.jr),"%)"),
                                         toontekst= TRUE,
                                         toonbuffer = FALSE)
gbuffer.2007E = grafiek.buffercapaciteit(wind2007E)
grid.arrange(plot.tekort.wind.2007E, gbuffer.2007E, ncol = 1, heights = c(2, 1))

}

# ------------------------------------------------------------------------------

#One of the factors that affect the long-term planning for solar energy is the 
#local climatology. The available solar energy at a given site is quantified by 
#the downward solar (shortwave) radiation at the surface. 
#KNMI: long_name = "short_wave downward (global) irradiance";
#:bsrn_short_name = "SWD";

# --- ZONNE-ENERGIE OUD ------------------------------------------------------------
# Deze berekening is verouderd. We hebben nu instralingsdata van het
# KNMI, dus het gemiddelde over 2004-2013 kan gemakkelijk worden berekend.
# Ter referentie dus: andere data, simpele berekening: reality check
instraling.NL.2019.kWh.Jaar.m2 = 1100 
efficientie.zonnecel = 0.2
opbrengst.zonneenergie.m2 = instraling.NL.2019.kWh.Jaar.m2*efficientie.zonnecel
opbrengst.zonneenergie.km2 = opbrengst.zonneenergie.m2*1000*1000
benodigd.oppervlak.zonnecellen.km2 = Energieverbruik.NL.kWh/opbrengst.zonneenergie.km2

# Gemiddelde procentuele zoninstraling per maand periode 1981-2010
#https://www.zonnepaneel-info.nl/nl/opbrengst2.html

Gemiddelde.procentuele.zoninstraling.per.maand.periode.1981.2010 <- 
  data.frame(maand=factor(c("jan","feb","mrt","apr","mei","jun","jul","aug","sep","okt","nov","dec"),
                          levels=c("jan","feb","mrt","apr","mei","jun","jul","aug","sep","okt","nov","dec")), 
             instraling=c(2, 3.5, 7, 11.5, 15, 15, 15.4, 12.7, 8.4, 5, 2.2, 1.8), stringsAsFactors = TRUE)

gem.maandelijkse.straling = mean(Gemiddelde.procentuele.zoninstraling.per.maand.periode.1981.2010$instraling)

Gemiddelde.procentuele.zoninstraling.per.maand.periode.1981.2010$overschot = Gemiddelde.procentuele.zoninstraling.per.maand.periode.1981.2010$instraling - gem.maandelijkse.straling

Gemiddelde.procentuele.zoninstraling.per.maand.periode.1981.2010$norm.overschot = 100 *  Gemiddelde.procentuele.zoninstraling.per.maand.periode.1981.2010$overschot/gem.maandelijkse.straling


# --- ZONNE-ENERGIE ----------------------------------------------------------

Bereken.Opbrengst.Zon <- function(PERIODICITEIT="monthly", periode="2006::2015") {
  
  KNMI_wind_zon_10min$straling.Wm2      = KNMI_wind_zon_10min$short.wave.downward.radiation.W.m2
  KNMI_wind_zon_10min$opbrengst.kWh.km2 = (rendement*KNMI_wind_zon_10min$short.wave.downward.radiation.W.m2/6 * 1e6) /1000
  
  # converteer naar periodiek (bijv. maandelijks)
  if (PERIODICITEIT == "monthly") stra = apply.monthly(KNMI_wind_zon_10min[periode, "straling.Wm2"], FUN=mean)
  if (PERIODICITEIT == "weekly")  stra = apply.weekly(KNMI_wind_zon_10min[periode, "straling.Wm2"], FUN=mean)
  if (PERIODICITEIT == "daily")   stra = apply.daily(KNMI_wind_zon_10min[periode, "straling.Wm2"], FUN=mean)
  if (PERIODICITEIT == "10min")   stra = KNMI_wind_zon_10min[periode, "straling.Wm2"]
  colnames(stra) = "straling.Wm2"
  
  # converteer naar periodiek (bijv. maandelijks)
  if (PERIODICITEIT == "monthly") opbr = apply.monthly(KNMI_wind_zon_10min[periode, "opbrengst.zonnecel.kWh.km2"], FUN=sum)
  if (PERIODICITEIT == "weekly")  opbr = apply.weekly(KNMI_wind_zon_10min[periode, "opbrengst.zonnecel.kWh.km2"], FUN=sum)
  if (PERIODICITEIT == "daily")   opbr = apply.daily(KNMI_wind_zon_10min[periode, "opbrengst.zonnecel.kWh.km2"], FUN=sum)
  if (PERIODICITEIT == "10min")   opbr = KNMI_wind_zon_10min[periode, "opbrengst.zonnecel.kWh.km2"]
  colnames(opbr) = "opbrengst.kWh.km2"
  
  zon = merge(stra, opbr)

  # Bereken het periodieke productie. Let op, moet genormaliseerd
  # worden tov het Nederlandse jaargebruik. We stellen in de hele berekening
  # dat de gemiddelde opbrengst gelijk moet zijn aan het totale NL verbuik. 
  perio = periodiciteit.per.jaar(zon) 
  gemiddelde.opbrengst = mean(zon$opbrengst.kWh.km2) # OK, het opbrengstgemiddelde is nu afgesteld op ons gebruik. Nu kunnen we over tekorten gaan praten.
  jaaropbrengst = gemiddelde.opbrengst*perio
  # bereken dan de opbrengst voor ieder datapunt t.o.v. de gemiddelde jaarproductie
  zon$productie.pct.jr = 100 * (zon$opbrengst.kWh.km2 / jaaropbrengst)
  
  # Overschot: overschotten en tekorten, genormaliseerd tov het gemiddelde (oude visualisatie)
  zon$overschot.pct.jr = zon$productie.pct.jr - (100/perio)
  
  return(zon)
}

zon = Bereken.Opbrengst.Zon(PERIODICITEIT="monthly", periode="2006::2015")

# reality-check: de straling over een jaar moet in de buurt van de 1000 kWh/m2 liggen
# let op, waarde is opbrengst zonnecel 20% rendement, dus maal 5!
sum(zon["2007"]) # ja, dat klopt op dit moment voor 2007
jaargemiddelde.string.kWh.m2 = apply.yearly(zon, FUN=sum) # allemaal in de 1000 (x5), mooi. 

# frequentiediagram straling
# fd = Bereken.Opbrengst.Zon(PERIODICITEIT="10min", periode="2006::2015")
# hist(fd$straling.kWh.km2[fd$straling.kWh.km2!=0], breaks=100)
# unique(fd$straling.kWh.km2)

if (LOCALRUN) {

# OK, kijk eerst naar de gehele periode 2004-2013
zon20062015        = zon["2006::2015"]
stopifnot(round(sum(zon20062015$overschot), 1) == 0)
ct                 = cumulatief.tekort2(zon20062015$overschot)
zon20062015        = uitbreiden.dataserie.met.tekorten(zon20062015, ct$benodigde_buffer)
zon20062015$buffer = cummax(zon20062015$benodigde_buffer)
benodigde.buffergrootte.om.tekorten.zon.20062015.te.dekken = max(zon20062015$lopend_overschot) - min(zon20062015$lopend_overschot)
plot.tekort.zon20062015 = grafiek.tekort(zon20062015, 
                                  title    = "Figuur 3.  Maandelijkse fluctuaties zon",
                                  subtitle = "Opbrengst als percentage van de gemiddelde jaaropbrengst",
                                  toonbuffer=FALSE, toontekst=FALSE)
gbufferz.20062015 = grafiek.buffercapaciteit(zon20062015)
grid.arrange(plot.tekort.zon20062015, gbufferz.20062015, ncol = 1, heights = c(2, 1))


# OK, neem dan een jaar tijd, om de lezer een gevoel te geven voor een jaar
zon2007        = Bereken.Opbrengst.Zon(PERIODICITEIT="daily", periode="2008")
sum(zon2007$straling.kWh.km2)
round(sum(zon2007$overschot), 1) == 0 # klein verschil omdat gemiddelde op periode 10 jaar is gebaseerd
ct             = cumulatief.tekort2(zon2007$overschot)
zon2007        = uitbreiden.dataserie.met.tekorten(zon2007, ct$benodigde_buffer)
zon2007$buffer = cummax(zon2007$benodigde_buffer)
max(zon2007$benodigde_buffer)
benodigde.buffergrootte.om.tekorten.zon.2007.te.dekken = max(zon2007$lopend_overschot) - min(zon2007$lopend_overschot)
plot.tekort.zon2007 = grafiek.tekort(zon2007, title = "Figuur 3. Maandelijkse fluctuaties zon",
                                              subtitle = "Opbrengst als percentage van de gemiddelde jaaropbrengst",
                                     toontekst=FALSE, toonbuffer=FALSE)
}

# --- ENERGIEMIX ---------------------------------------------------------------

pct.vd.tijd.op.loadfactor = function(data, loadfactor,perio.per.jaar, perio.aantal.jaren) {
  length(which(data$hulpbron>=(loadfactor*(100 * 1/perio.per.jaar)))) / (perio.per.jaar*perio.aantal.jaren)
}

# Uitbreiden simulatie: 
# - maximale vermogen van opslag (belangrijk voor ontrekking en laden)
# - conversieverliezen voor opslagmethoden (punt van Knud) 

Bereken.Energiemix.Praktijk <- function(
  naam                  = "Energiemix",
  jaarverbruik          = Energieverbruik.NL.kWh, # verbruik waartegen alles wordt afgezet
  zonnecellen.km2       = 4000,          # aantal km2 belegt met zonnecellen
  rendement.zonnecellen = 0.2,           # rendement van de zonnecellen, typisch 10-20%
  windmolens.km2        = 52395,         # aandeel tov totaal jaarlijks energieverbruik NL
  separatieafstand      = 7,             # hoe ver de molens uit elkaar staan in wiekdiameters
  veldefficientie       = 0.6,           # de reductie van efficientie van het veld, zie Meneveau, 0.6 bij 7 diam.
  windtype              = "Enercon-126", # of "Betz". gebruik theorie of de Enercon-126 om opbrengst te bepalen. 
  opslaggrootte.TWh     = 1.6,           # De grootte van de buffer of accu om intermittancy mee op te vangen. 1.6 TWh is mondiale capaciteit hydro.
  opslagefficientie     = 0.9,          # de round trip efficiency elektr terug naar elektr.
  startmetvolleaccu     = FALSE,
  Periode               = "2019"
)
{
  # Opmerking: periodiciteit van de data is altijd 10min, sommeren naar langere tijdseenheid komt na de berekeningen.
  data = KNMI_wind_zon_10min[Periode]
  
  # --- bepaal de opbrengst voor wind ahv de meegegeven variabelen in kWh per km2  &checked
  if (windtype=="Enercon-126") {
    enercon.bezet.opp.m2 = (separatieafstand*126)^2   # sd^2
    data$Enercon126.Wm2  = veldefficientie * (data$Vermogen.Enercon126.W / enercon.bezet.opp.m2)  # Watten van de Enercon gedeeld door opp waar-i op staat, maal de efficientie van 't veld
    # nu de opbrengsten per 10min berekenen in kWh per km2
    data$opbrengst.wind.kWh.km2  = (data$Enercon126.Wm2/6 * 1e6) / 1000 
  }
  if (windtype=="Betz") {
    data$Betz.Wm2 = Betz.in.W.m2(data$wind140m, separatieafstand=separatieafstand, efficientie=efficientie)
    # nu de opbrengsten per 10min berekenen in kWh per km2
    data$opbrengst.wind.kWh.km2  = (data$Betz.Wm2/6 * 1e6) / 1000 
  }
  # --- bepaal de opbrengst voor zon ahv de meegegeven variabelen in kWh per km2 &checked
  data$opbrengst.zon.kWh.km2     = (rendement.zonnecellen * data$short.wave.downward.radiation.W.m2/6 * 1e6) /1000
  
  # --- MIX: Combineer de opbrengsten ---
  perio = periodiciteit.per.jaar(data) # alles gaat genormaliseerd worden naar jaarpercentages
  opslag.pct.jr = 100*(opslaggrootte.TWh/(jaarverbruik/1e9)) # Energieverbruik.NL.in.TWh.per.jaar
 
  # hoeveel produceren wind en zon nu eigenlijk in totaal tov het nationaal verbruik?
  dif = index(data)[nrow(data)]- index(data)[1]
  totaal.zon.pct.jr = (sum(data$opbrengst.zonnecel.kWh.km2)*zonnecellen.km2 / as.numeric(dif/365)) / jaarverbruik
  totaal.wind.pct.jr = (sum(data$opbrengst.wind.kWh.km2)*windmolens.km2 / as.numeric(dif/365)) / jaarverbruik
  
   
  # ---- STEL DE PRODUCTIECIJFERS SAMEN -----------------------------------------------------
  
  mix = data[, c("opbrengst.wind.kWh.km2", "opbrengst.zon.kWh.km2")]
  
  # maak de opbrengst relatief tov het JAARgemiddelde, dus in procenten per bar tov jaargemiddelde (.
  # Voor jaarverbruik nemen we het totaalverbruik NL 2018.
  mix$wind.pct.jr = 100*(data$opbrengst.wind.kWh.km2 * windmolens.km2 / jaarverbruik)  
  # maak de opbrengst relatief tov het JAARgemiddelde. Zelfde voor zon
  mix$zon.pct.jr  = 100*(data$opbrengst.zon.kWh.km2 * zonnecellen.km2 / jaarverbruik) 
 
  # nu gaan we een combinatie maken van zon en wind. Dat zijn de hoofdproducenten.
  # Overschot groter dan gemiddelde vraag gaat de opslag in. Tekort aan opslag 
  # wordt bijgedraaid. 
  mix$combi.pct.jr     = mix$wind.pct.jr + mix$zon.pct.jr
  mix$overschot.pct.jr = mix$combi-(100/perio)
  # tussentijdse check: hoe ver komt de productie van zon en wind tov het jaarverbuik?
  # print(paste("jaaroverschot=", round(sum(mix$overschot.pct.jr)),"%")) 
  
  ct  = cumulatief.tekort2(mix$overschot.pct.jr)
  mix = uitbreiden.dataserie.met.tekorten(mix, ct$benodigde_buffer)
  
  # --- STEL DE VISUALISATIECIJFERS (STAAFDIAGRAM) SAMEN -----------------------
  # Eerst een simpele visualisatie: alle wind, alle zon in geel en groen. Blauw
  # voor bijdraaien, groen voor opslag, lichtgeel/lichtgroen voor onbenut zon,wind 
  
  avg.pct = 100/perio # hoeveel procent heb ik per bar tov een jaar
  
  # als eerste reken het overschot uit dat de accu in ZOU kunnen.
  mix$surplus    = ifelse(mix$combi.pct.jr>avg.pct, mix$combi.pct.jr-avg.pct, 0)
  # neem aan dat de accu leeg is. We kunnen dus direct vullen, tot wanneer deze vol is. Dan lopen we over in bijdraaien en onbenut
  mix$tekort     = ifelse(mix$combi.pct.jr <= avg.pct, avg.pct-mix$combi.pct.jr, 0) 
  mix$combi      = ifelse(mix$combi.pct.jr <= avg.pct, mix$combi.pct.jr, avg.pct) # die is makkelijk
  
  # Fast bounded cumsum, in Cpp. Anders meer dan 3 kwartier met 10 jr
  cppFunction('NumericVector cumsumBounded(NumericVector x, double startvalue, double low, double high) {
    NumericVector res(x.size());
    double acc = startvalue;
    for (int i=0; i < x.size(); ++i) {
      acc += x[i];
      if (acc < low)  acc = low;
      else if (acc > high)  acc = high;
      res[i] = acc;
    }
    return res;
  }')
  
  # --- bereken de laadtoestand van de accu. Daar worden de waarden voor opslag, benut
  # en onbenut van afgeleid. Verreken ook de efficiëntie van de opslag: benader dat 
  # door wat er in de accu stroomt te verrekenen met de efficientie. Eigenlijk heb je
  # aparte opslag en leveringsefficientie, maar hier wordt het ineens berekend.
  # De accu verliest een percentage, dat verrekenen we hier door het overschot
  # te laten afnemen, maar niet het tekort.
  mix$corr.overschot = ifelse(mix$overschot.pct.jr>0, mix$overschot.pct.jr*opslagefficientie, mix$overschot.pct.jr)
  if (!startmetvolleaccu) { startwaarde = 0 } else {startwaarde = opslag.pct.jr}
  mix$laadtoestand = cumsumBounded(mix$corr.overschot, startwaarde, 0, opslag.pct.jr)
  
  # uit opslag gebruikt: alles dat uit de accu stroomt, dus het verschil als de accustand verlaagd
  mix$opslag = diff(mix$laadtoestand)    # bereken de toenames en afnames van de accu
  mix$opslag[is.na(mix$opslag)] <- 0
  mix$opslag[mix$opslag>0] <- 0          # we willen de afnames
  mix$opslag = abs(mix$opslag)           # klapt die om naar positief: uit de accu ontvangen
  # wat niet uit de accu stroomde, komt uit de hulpbron
  mix$hulpbron = ifelse(mix$opslag-mix$tekort<0, abs(mix$opslag-mix$tekort), 0)
  # benut en onbenut
  # benut is alles dat in de accu stroomt, dus het verschil als de accustand verhoogd
  mix$benut = diff(mix$laadtoestand)    # bereken de toenames en afnames van de accu
  mix$benut[is.na(mix$benut)] <- 0
  mix$benut[mix$benut<0] <- 0
  mix$onbenut = mix$surplus - mix$benut
  
  # Voor iedere bar moeten we uitzoeken waar de onbenutte energie zit.
  mix$wind       =  mix$combi * (mix$wind.pct.jr/(mix$wind.pct.jr+mix$zon.pct.jr)) # schaalt combi naar windaandeel, probleem als wind en zon nul is
  mix$wind[is.na(mix$wind)] <- 0 # los 't probleem op
  mix$zon        =  mix$combi * (mix$zon.pct.jr/(mix$wind.pct.jr+mix$zon.pct.jr)) # schaalt combi naar aonaandeel, probleem als wind en zon nul is    
  mix$zon[is.na(mix$zon)] <- 0  # los 't probleem op
  #which(is.na(mix$zon))
  
  # iedere bar bestaat uit wat er uit opslag is gepakt, bijgedraaid, door wind en zon, en dat is precies gelijk aan het gem. percentage
  #chk.gem.opbrengst = rowSums(mix[, c("opslag","hulpbron","wind","zon")])
  
  # ---- bereken metrics ahv de data -------------------------------------------
  
  # verschillende conclusies
  # benodigde.buffergrootte.pct.jr = max(max(windzon5050$data$buffer), max(windzon5050$data$lopend_overschot)) # buffer die je nodig zou hebben zonder bijdraaien
  totale.energiebehoefte         = avg.pct * nrow(mix) # het totale verbruik van de gehele beschouwde periode
  bijgedraaid.pct.van.totaal     = sum(mix$hulpbron[2:nrow(mix)])/totale.energiebehoefte
  via.opslag.pct.van.totaal      = sum(mix$opslag)/totale.energiebehoefte     # A   
  onbenut.pct.totaal             = sum(mix$onbenut)/totale.energiebehoefte    
  benut.pct.totaal               = sum(mix$benut)/totale.energiebehoefte      # B
  wind.pct.totaal                = sum(mix$wind)/totale.energiebehoefte
  zon.pct.totaal                 = sum(mix$zon)/totale.energiebehoefte
  min.cap.hulpbron               = max(mix$hulpbron[2:nrow(mix)]) / (100/perio)
  # hoeveel vermogen moet ik kwijt kunnen bij overschotdagen?
  # neem gemiddelde van de top 20%. (max vermogen meet je vóór efficientieverliezen
  # van de opslag). Uitkomst in % tov continu vermogen gedurende het jaar.
  #set = mix$overschot.pct.jr[mix$overschot.pct.jr>0] # !! oeps, dit lijkt fout (12-11-21). Hier wordt ALLE overschot genomen, regardless of het wel in de opslag paste. Dat is niet goed. In het nieuwe model, dat waterstofconversies meerekend, is dit probleem ondervangen.
  set = diff(mix$laadtoestand)[diff(mix$laadtoestand)>0] / opslagefficientie  # correctie (12-11-2021). Tweede fout: verlies niet meegenomen.
  avg = ifelse(nrow(set)>0, mean(set[order(set, decreasing=TRUE)[1:round(nrow(set)/5)]]), 0)
  avg.max.laadvermogen           = avg / (100/perio)
  max.laadvermogen               = abs(max(set)) / (100/perio) # normaliseer naar jaar, over gehele periode
  max.ontlaadvermogen            = abs(min(diff(mix$laadtoestand), na.rm=TRUE)) / (100/perio)
  
  perio.aantal.jaren = r2(interval(first(index(mix)), last(index(mix))) / years(1))
  
  result = list(data=mix, 
                metrics = data.frame(
                  naam=naam,
                  perio.per.jaar=perio,
                  perio.aantal.jaren=perio.aantal.jaren,
                  windpower=windtype,
                  
                  bijgedraaid.pct.van.totaal = bijgedraaid.pct.van.totaal,
                  via.opslag.pct.van.totaal  = via.opslag.pct.van.totaal,
                  onbenut.pct.totaal         = onbenut.pct.totaal,
                  benut.pct.totaal           = benut.pct.totaal,
                  wind.pct.totaal            = wind.pct.totaal,
                  zon.pct.totaal             = zon.pct.totaal,
                  min.cap.hulpbron           = min.cap.hulpbron,
                  avg.max.laadvermogen       = avg.max.laadvermogen,
                  max.laadvermogen           = max.laadvermogen,
                  max.ontlaadvermogen        = max.ontlaadvermogen,
                  totaal.wind.pct.jr         = totaal.wind.pct.jr,
                  totaal.zon.pct.jr          = totaal.zon.pct.jr,
                  # voor de uitkomsten is het beter met een volle accu te beginnen
                  pct.tijd.hulpbron.op.loadfactor0  = pct.vd.tijd.op.loadfactor(mix, 0.001, perio, perio.aantal.jaren),
                  pct.tijd.hulpbron.op.loadfactor50 = pct.vd.tijd.op.loadfactor(mix, 0.5, perio, perio.aantal.jaren),
                  pct.tijd.hulpbron.op.loadfactor90 = pct.vd.tijd.op.loadfactor(mix, 0.9, perio, perio.aantal.jaren),
                  
                  jaarverbruik.TWh      = jaarverbruik/1e9,
                  windmolens.km2        = windmolens.km2,
                  zonnecellen.km2       = zonnecellen.km2,
                  opp.bezetting.wind    = windmolens.km2/Oppervlakte.Nederland.km2,
                  opp.bezetting.zon     = zonnecellen.km2/Oppervlakte.Nederland.km2,
                  rendement.zonnecellen = rendement.zonnecellen,          
                  separatieafstand      = separatieafstand,             
                  veldefficientie       = veldefficientie,  
                  opslagefficientie     = opslagefficientie,
                  windtype              = windtype, 
                  opslaggrootte.TWh     = opslaggrootte.TWh,
                  opslag.pct.jr         = 100*(opslaggrootte.TWh/(jaarverbruik/1e9)),
                  
                  stringsAsFactors = F)
  )
  
  return(result) 
}

# --- Model voor een energiemix met waterstofopslag (Schema: Model Energiemix 2.png) ---
# Wind en zon wekken per tien minuten samen een bepaalde hoeveelheid energie op. Afnemers van elektriciteit worden eerst bediend, 
# omdat het verbranden van waterstof om elektriciteit te maken inefficiënt  is. Op punt (I) wordt besloten of er genoeg productie is 
# om aan de elektriciteitsvraag te voldoen. Eventueel wordt er electriciteit aangevuld uit de opslag. (Hierbij wordt rekening gehouden 
# met een verlies van 50% bij het verbranden van waterstof om elektriciteit op te wekken.) Indien er onvoldoende in de opslag zit, 
# wordt er elektriciteit bijgedraaid door energiecentrales. Op punt (2) wordt vervolgens gekeken of er na het voldoen van de 
# elektriciteitsvraag nog voldoende energie is overgebleven om ook aan de vraag naar waterstof te voldoen. (Hierbij wordt rekening 
# gehouden met een verlies van 20% in het benodigde elektrolyseproces.) Indien er onvoldoende energie overbleef om aan de vraag naar 
# waterstof te voldoen, wordt er waterstof betrokken uit de opslag. Indien de opslag over onvoldoende waterstof beschikt, dan draaien 
# de energiecentrales bij om waterstof te produceren. (Hierbij wordt weer rekening gehouden met een verlies van 20% voor de productie 
# van waterstof.)

Bereken.Energiemix.H2El <- function(
  naam                   = "Energiemix 2B",
  jaarverbruik           = Energieverbruik.NL.kWh, # het totaalverbruik van Nederland
  waterstofverbruik      = 0.8,                    # verbruik aan waterstof als fractie van het jaarverbruik. Stroomverbruik is 1-waterstofverbruik.
  zonnecellen.km2        = 1500,                   # aantal km2 belegt met zonnecellen
  rendement.zonnecellen  = 0.2,                    # rendement van de zonnecellen, typisch 10-20%
  windmolens.km2         = 23000,                  # aandeel tov totaal jaarlijks energieverbruik NL
  separatieafstand       = 7,                      # hoe ver de molens uit elkaar staan in wiekdiameters
  veldefficientie        = 0.6,                    # de reductie van efficientie van het veld, zie Meneveau, 0.6 bij 7 diam.
  windtype               = "Enercon-126",          # of "Betz". gebruik theorie of de Enercon-126 om opbrengst te bepalen. 
  opslaggrootte.TWh      = 3,                      # De grootte van de opslag in TWh, hier waterstof!!  
  efficientie.H2.naar.El = 0.5,                    # de efficiency van waterstof omzetten in stroom
  efficientie.El.naar.H2 = 0.8,                    # de efficiency van stroom omzetten in waterstof
  startmetvolleaccu      = TRUE,
  Periode                = "2019"
)
{
  # ---- BEPAAL DE OPBRENGST UIT VARIABELE BRONNEN ----
  
  # Opmerking: periodiciteit van de data is altijd 10min, sommeren naar langere tijdseenheid komt na de berekeningen.
  data = KNMI_wind_zon_10min[Periode]
  elektriciteitsverbruik = 1 - waterstofverbruik
  
  # --- bepaal de opbrengst voor wind ahv de meegegeven variabelen in kWh per km2  &checked
  if (windtype=="Enercon-126") {
    enercon.bezet.opp.m2 = (separatieafstand*126)^2   # sd^2
    data$Enercon126.Wm2  = veldefficientie * (data$Vermogen.Enercon126.W / enercon.bezet.opp.m2)  # Watten van de Enercon gedeeld door opp waar-i op staat, maal de efficientie van 't veld
    # nu de opbrengsten per 10min berekenen in kWh per km2
    data$opbrengst.wind.kWh.km2  = (data$Enercon126.Wm2/6 * 1e6) / 1000 
  }
  if (windtype=="Betz") {
    data$Betz.Wm2 = Betz.in.W.m2(data$wind140m, separatieafstand=separatieafstand, efficientie=efficientie)
    # nu de opbrengsten per 10min berekenen in kWh per km2
    data$opbrengst.wind.kWh.km2  = (data$Betz.Wm2/6 * 1e6) / 1000 
  }
  # --- bepaal de opbrengst voor zon ahv de meegegeven variabelen in kWh per km2 &checked
  data$opbrengst.zon.kWh.km2     = (rendement.zonnecellen * data$short.wave.downward.radiation.W.m2/6 * 1e6) /1000
  
  # --- MIX: Combineer de opbrengsten ---
  perio = periodiciteit.per.jaar(data) # alles gaat genormaliseerd worden naar jaarpercentages
  opslag.pct.jr = 100*(opslaggrootte.TWh/(jaarverbruik/1e9)) # Energieverbruik.NL.in.TWh.per.jaar
  
  # hoeveel produceren wind en zon nu eigenlijk in totaal tov het nationaal verbruik?
  dif = index(data)[nrow(data)] - index(data)[1]
  totaal.zon.pct.jr  = (sum(data$opbrengst.zonnecel.kWh.km2)*zonnecellen.km2 / as.numeric(dif/365)) / jaarverbruik
  totaal.wind.pct.jr = (sum(data$opbrengst.wind.kWh.km2)*windmolens.km2 / as.numeric(dif/365)) / jaarverbruik
  
  
  # ---- STEL DE PRODUCTIECIJFERS SAMEN -----------------------------------------------------
  
  mix = data[, c("opbrengst.wind.kWh.km2", "opbrengst.zon.kWh.km2")]
  
  # maak de opbrengst relatief tov het JAARgemiddelde, dus in procenten per periode van 10min t.o.v. het jaargemiddelde.
  mix$wind.pct.jr        = 100*(data$opbrengst.wind.kWh.km2 * windmolens.km2 / jaarverbruik)  
  mix$zon.pct.jr         = 100*(data$opbrengst.zon.kWh.km2 * zonnecellen.km2 / jaarverbruik) 
  mix$opbrengst.tot      = mix$wind.pct.jr + mix$zon.pct.jr                       # totaalopbrengst voor iedere 10 minuten van zon en wind
  #mix = mix[, -c(1:4)]
  
  # Definieer de vraag naar H2 als brandstof en elektriciteit in % van totaal jaarverbruik
  mix$vraag.el           = (100/perio) * elektriciteitsverbruik                  # maak gebruik van een vector, zodat later de vraag kan variëren ipv constant is
  mix$vraag.h2           = (100/perio) * waterstofverbruik                       # Voor nu is stroomvraag en h2-vraag constant
  
  # --- MODELIMPLEMENTATIE ---
  
  # --- Afname elektriciteit en waterstof van variabele bronnen
  
  # Elektriciteit (heeft geen verlies)
  mix$direct.el          = ifelse(mix$opbrengst.tot<mix$vraag.el, mix$opbrengst.tot, mix$vraag.el)
  # waterstof (heeft omzettingsverlies in deze stap)
  opbrengst_restant      = mix$opbrengst.tot-mix$direct.el
  vraag.aan.h2.vanaf.el  = mix$vraag.h2 / efficientie.El.naar.H2  
  mix$direct.h2          = ifelse(opbrengst_restant < vraag.aan.h2.vanaf.el, opbrengst_restant*efficientie.El.naar.H2,     mix$vraag.h2)
  mix$verlies.primair    = ifelse(opbrengst_restant < vraag.aan.h2.vanaf.el, opbrengst_restant*(1-efficientie.El.naar.H2), vraag.aan.h2.vanaf.el*(1-efficientie.El.naar.H2)) 
  
  # --- Surplus ---
  # Als directe afname van stroom en waterstof zijn verrekend - hetgeen variabele bronnen direct konden leveren - dan kan het overschot worden 
  # uitgerekend wat beschikbaar is voor de opslag.
  mix$surplus            = mix$opbrengst.tot - mix$vraag.el - mix$vraag.h2/efficientie.El.naar.H2
  mix$surplus[mix$surplus<0] = 0
  
  # maak een loop die laadtoestand, hulpbron en verliezen tegelijkertijd uitrekent
  # in C++ voor snelheid.
  mix$tekort.el          = mix$vraag.el - mix$direct.el 
  mix$tekort.h2          = mix$vraag.h2 - mix$direct.h2 
  sourceCpp("productiecijfersC.cpp")
  if (!startmetvolleaccu) { startwaarde = 0 } else {startwaarde = opslag.pct.jr}
  ot = prodcijfersC(mix, startwaarde, efficientie.H2.naar.El, efficientie.El.naar.H2, opslag.pct.jr)
  ot = xts(ot, index(mix))
  
  # ot heeft nieuwe data, en een doubleure
  mix = mix[ ,-which(colnames(mix)=="verlies.primair")]  # verwijder de doubleure
  mix = cbind(mix, ot)    # en voeg samen
  
  # # ---- maak een loop die laadtoestand, hulpbron en verliezen tegelijkertijd uitrekent ----
  # # in R ter controle
  # mix$tekort.el          = mix$vraag.el - mix$direct.el
  # mix$tekort.h2          = mix$vraag.h2 - mix$direct.h2
  # hulpbron               = rep(0, nrow(mix))
  # opslag                 = rep(0, nrow(mix))
  # onbenut                = rep(0, nrow(mix))
  # laadtoestand           = rep(0, nrow(mix))
  # verlies.hulpbron       = rep(0, nrow(mix))
  # verlies.opslag         = rep(0, nrow(mix))
  # opgeslagen             = rep(0, nrow(mix))
  # if (!startmetvolleaccu) { startwaarde = 0 } else {startwaarde = opslag.pct.jr}
  # laadtoestand[1] = startwaarde
  # for (i in 1:nrow(mix)) {
  #   opslag[i]   = 0
  #   hulpbron[i] = 0
  # 
  #   # Het tekort aan stroom wordt als eerste zoveel mogelijk uit de opslag gehaald
  #   if (laadtoestand[i] > mix$tekort.el[[i]]/efficientie.H2.naar.El) {
  #     opslag[i]              = mix$tekort.el[[i]]
  #     verlies.opslag[i]      = mix$tekort.el[[i]]/efficientie.H2.naar.El - mix$tekort.el[[i]]
  #     laadtoestand[i]        = laadtoestand[i] - mix$tekort.el[i]/efficientie.H2.naar.El
  #   } else {
  #     opslag[i]              = laadtoestand[i]*efficientie.H2.naar.El
  #     verlies.opslag[i]      = laadtoestand[i]*(1-efficientie.H2.naar.El)
  #     # en de hulpbron moet worden ingezet
  #     hulpbron[i]            = mix$tekort.el[[i]] - opslag[i]
  #     # geen verlies hier!
  #     laadtoestand[i]        = 0
  #   }
  # 
  #   # van wat er over is in de opslag, wordt gepoogd het tekort aan h2-vraag te voldoen
  #   if (laadtoestand[i]>mix$tekort.h2[[i]]) {
  #     opslag[i]              = opslag[i] + mix$tekort.h2[[i]]
  #     # geen verlies!
  #     laadtoestand[i]        = laadtoestand[i] - mix$tekort.h2[[i]]
  #   } else {
  #     opslag[i]              = opslag[i] + laadtoestand[i]
  #     # geen verlies!
  # 
  #     # en de hulpbron moet worden ingezet
  #     hulpbron[i]            = hulpbron[i] + mix$tekort.h2[i] - laadtoestand[i]
  #     verlies.hulpbron[i]    = hulpbron[i]/efficientie.El.naar.H2 - hulpbron[i]
  #     laadtoestand[i]        = 0
  #   }
  # 
  #   # het surplus vult de opslag - voor de volgende ronde
  #   if (i<nrow(mix)) {
  #     overloop = (laadtoestand[[i]] + mix$surplus[[i]] * efficientie.El.naar.H2) - opslag.pct.jr
  #     if (overloop > 0) {
  #       # oeps, opslag loopt over...
  #       laadtoestand[i+1]        = opslag.pct.jr;  # voller kan het niet
  #       mix$verlies.primair[[i]] = mix$verlies.primair[[i]] +
  #         (opslag.pct.jr-laadtoestand[i])/efficientie.El.naar.H2 * (1-efficientie.El.naar.H2)
  #       onbenut[i]               = mix$surplus[[i]] - ((opslag.pct.jr-laadtoestand[i])/efficientie.El.naar.H2)
  #       opgeslagen[i+1]          = opslag.pct.jr-laadtoestand[i]
  #     } else {
  #       # niets aan de hand, geen overloop
  #       laadtoestand[i+1]       = laadtoestand[i] + mix$surplus[[i]] * efficientie.El.naar.H2
  #       mix$verlies.primair[[i]] = mix$verlies.primair[[i]]  + mix$surplus[[i]] * (1-efficientie.El.naar.H2)
  #       onbenut[i]               = 0
  #       opgeslagen[i+1]          = mix$surplus[[i]] * efficientie.El.naar.H2
  #     }
  #   }
  # }
  # mix$hulpbron           = hulpbron
  # mix$opslag             = opslag
  # mix$onbenut            = onbenut
  # mix$laadtoestand       = laadtoestand
  # mix$verlies.opslag     = verlies.opslag
  # mix$verlies.hulpbron   = verlies.hulpbron
  # mix$opgeslagen         = opgeslagen
  # # vergelijk C++ code met R:
  # ot2 = mix[, c("hulpbron","opslag","onbenut","opgeslagen","laadtoestand","verlies.primair","verlies.hulpbron","verlies.opslag")]
  # identical(ot, ot2) # == TRUE
  
  # ---- verdeel de directe opbrenst voor El en H2 in een deel voor wind en een deel voor zon ----
  mix$wind       =  (mix$direct.h2+mix$direct.el) * (mix$wind.pct.jr/(mix$wind.pct.jr+mix$zon.pct.jr)) # schaalt combi naar windaandeel, probleem als wind en zon nul is
  mix$wind[is.na(mix$wind)] <- 0 # los 't probleem op
  mix$zon        =  (mix$direct.h2+mix$direct.el) * (mix$zon.pct.jr/(mix$wind.pct.jr+mix$zon.pct.jr)) # schaalt combi naar windaandeel, probleem als wind en zon nul is
  mix$zon[is.na(mix$zon)] <- 0 # los 't probleem op
  mix$verliezen  = mix$verlies.hulpbron+mix$verlies.opslag+mix$verlies.primair+mix$onbenut 
  
  # ---- CHECKS & BALANCES ----
  # neem als datum 1 dag, bijv 2007-03-15, zodat iedere bar zichtbaar is. Doe een
  # ggplot om te checken of alles correct wordt uitgerekend.
  
  # primaire opbrengst, uit opslag en bijdraaien
  # ggdata = xtsmelt(mix[,c("hulpbron","opslag","direct.h2","direct.el")]) # zon, wind
  # colnames(ggdata) = c("Date","Energieopbrengst","value")
  # ggplot(ggdata) +
  #   geom_bar(stat="identity", colour=NA, size=0, aes(fill=Energieopbrengst, y=value, x=Date)) +  # position="stack",
  #   geom_hline(yintercept=mix$vraag.el, linetype="dashed", color="red") +
  #   geom_hline(yintercept=mix$vraag.el+mix$vraag.h2, linetype="dashed", color="cyan")
  # 
  # # terug naar oude overzicht
  # ggdata2 = xtsmelt(mix[,c("verliezen","opgeslagen","hulpbron","opslag","wind","zon")])
  # colnames(ggdata2) = c("Date","Energieopbrengst","value")
  # ggplot(ggdata2) +
  #   geom_bar(stat="identity", colour=NA, size=0, aes(fill=Energieopbrengst, y=value, x=Date)) +
  #   geom_hline(yintercept=mix$vraag.el, linetype="dashed", color="red") +
  #   geom_hline(yintercept=mix$vraag.el+mix$vraag.h2, linetype="dashed", color="cyan") +
  #   scale_fill_manual(guide=guide_legend(reverse=T), values=c("#E8C4C4","#C6E1BF","#56B4E9","darkgreen","#39B54A","#FFDE17") ) + # mooie bruin: #E69F00 "#D78A7D", gr"#92B592, lichtgroen:E5FFE4
  #   theme(legend.position="top", legend.title = element_blank(), axis.title.x = element_blank())
  # 
  # ggplot(mix) +
  #   theme_maas() +
  #   scale_fill_manual(values = c(bkgrnd, 'darkgreen') ) +
  #   geom_bar(stat="identity", width=1, aes(x=Index, y=opslag.pct.jr, fill=bkgrnd)) +
  #   geom_bar(stat="identity", aes(x=Index, y=laadtoestand, fill='darkgreen'), colour='darkgreen') +
  #   geom_hline(yintercept=opslag.pct.jr, linetype="dashed", color = "darkgreen") +
  #   labs(title="Laadtoestand van de opslag", caption = figuurcaption) +
  #   theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_text(size = 9),
  #         plot.caption = element_text(hjust = 1, lineheight = 1.2)) + #family="serif"
  #   ylab("Laadtoestand (% van jaarverbruik)")
  
  # ---- bereken metrics ahv de data -------------------------------------------
  
  # verschillende conclusies
  # benodigde.buffergrootte.pct.jr = max(max(windzon5050$data$buffer), max(windzon5050$data$lopend_overschot)) # buffer die je nodig zou hebben zonder bijdraaien
  totale.energiebehoefte         = (100/perio) * nrow(mix) # het totale verbruik van de gehele beschouwde periode
  bijgedraaid.pct.van.totaal     = sum(mix$hulpbron[2:nrow(mix)])/totale.energiebehoefte
  via.opslag.pct.van.totaal      = sum(mix$opslag)/totale.energiebehoefte     # A   
  onbenut.pct.totaal             = sum(mix$onbenut)/totale.energiebehoefte
  surplus.pct.totaal             = sum(mix$surplus)/totale.energiebehoefte
  omzettingsverliezen.pct.totaal = (sum(mix$verlies.primair)+sum(mix$verlies.hulpbron)+sum(mix$verlies.opslag))/totale.energiebehoefte
  verliezen.pct.totaal           = sum(mix$verliezen)/totale.energiebehoefte    
  
  opgeslagen.pct.totaal          = sum(mix$opgeslagen)/totale.energiebehoefte      # B
  wind.pct.totaal                = sum(mix$wind)/totale.energiebehoefte
  zon.pct.totaal                 = sum(mix$zon)/totale.energiebehoefte
  min.cap.hulpbron               = max(mix$hulpbron[2:nrow(mix)]) / (100/perio)
  # hoeveel vermogen moet ik kwijt kunnen bij overschotdagen?
  # neem gemiddelde van de top 20%. (max vermogen meet je vóór efficientieverliezen
  # van de opslag). Uitkomst in % tov continu vermogen gedurende het jaar.
  set = mix$opgeslagen[mix$opgeslagen>0]/efficientie.El.naar.H2 # opslag naar h2 heeft verlies
  if (length(set)>0) {
    avg = ifelse(nrow(set)>0, mean(set[order(set, decreasing=TRUE)[1:round(nrow(set)/5)]]), 0)
    avg.max.laadvermogen           = avg / (100/perio)
    max.laadvermogen               = abs(max(set)) / (100/perio) # normaliseer naar jaar, over gehele periode
  } else {
    avg.max.laadvermogen           = 0
    max.laadvermogen               = 0
  }
  # ontlaadvermogen is gedefinieerd als het vermogen dat de afnemer ONTVANGT, niet het vermogen dat nodig is het te produceren
  max.ontlaadvermogen            = abs(max(mix$opslag)) / (100/perio) 
  
  perio.aantal.jaren = r2(interval(first(index(mix)), last(index(mix))) / years(1))
  
  result = list(data=mix, 
                metrics = data.frame(
                  naam=naam,
                  perio.per.jaar=perio,
                  perio.aantal.jaren=perio.aantal.jaren,
                  windpower=windtype,
                  
                  bijgedraaid.pct.van.totaal = bijgedraaid.pct.van.totaal,
                  via.opslag.pct.van.totaal  = via.opslag.pct.van.totaal,
                  onbenut.pct.totaal         = onbenut.pct.totaal,
                  surplus.pct.totaal         = surplus.pct.totaal,
                  omzettingsverliezen.pct.totaal = omzettingsverliezen.pct.totaal,
                  verliezen.pct.totaal       = verliezen.pct.totaal,
                  opgeslagen.pct.totaal      = opgeslagen.pct.totaal,
                  wind.pct.totaal            = wind.pct.totaal,
                  zon.pct.totaal             = zon.pct.totaal,
                  min.cap.hulpbron           = min.cap.hulpbron,
                  avg.max.laadvermogen       = avg.max.laadvermogen,
                  max.laadvermogen           = max.laadvermogen,
                  max.ontlaadvermogen        = max.ontlaadvermogen,
                  totaal.wind.pct.jr         = totaal.wind.pct.jr,
                  totaal.zon.pct.jr          = totaal.zon.pct.jr,
                  # voor de uitkomsten is het beter met een volle accu te beginnen
                  pct.tijd.hulpbron.op.loadfactor0  = pct.vd.tijd.op.loadfactor(mix, 0.001, perio, perio.aantal.jaren),
                  pct.tijd.hulpbron.op.loadfactor50 = pct.vd.tijd.op.loadfactor(mix, 0.5, perio, perio.aantal.jaren),
                  pct.tijd.hulpbron.op.loadfactor90 = pct.vd.tijd.op.loadfactor(mix, 0.9, perio, perio.aantal.jaren),
                  
                  jaarverbruik.TWh      = jaarverbruik/1e9,
                  waterstofverbruik     = waterstofverbruik,
                  windmolens.km2        = windmolens.km2,
                  zonnecellen.km2       = zonnecellen.km2,
                  opp.bezetting.wind    = windmolens.km2/Oppervlakte.Nederland.km2,
                  opp.bezetting.zon     = zonnecellen.km2/Oppervlakte.Nederland.km2,
                  rendement.zonnecellen = rendement.zonnecellen,          
                  separatieafstand      = separatieafstand,             
                  veldefficientie       = veldefficientie,  
                  efficientie.H2.naar.El = efficientie.H2.naar.El,
                  efficientie.El.naar.H2 = efficientie.El.naar.H2,
                  windtype              = windtype, 
                  opslaggrootte.TWh     = opslaggrootte.TWh,
                  opslag.pct.jr         = 100*(opslaggrootte.TWh/(jaarverbruik/1e9)),
                  
                  stringsAsFactors = F)
  )
  
  return(result) 
}

Bereken.Energiemix.Model2 <- function(
  naam                   = "Energiemix M2",
  jaarverbruik           = Energieverbruik.NL.kWh, # het totaalverbruik van Nederland
  waterstofverbruik      = 0.8,                    # verbruik aan waterstof als fractie van het jaarverbruik. Stroomverbruik is 1-waterstofverbruik.
  zonnecellen.km2        = 1500,                   # aantal km2 belegt met zonnecellen
  rendement.zonnecellen  = 0.2,                    # rendement van de zonnecellen, typisch 10-20%
  windmolens.km2         = 23000,                  # aandeel tov totaal jaarlijks energieverbruik NL
  separatieafstand       = 7,                      # hoe ver de molens uit elkaar staan in wiekdiameters
  veldefficientie        = 0.6,                    # de reductie van efficientie van het veld, zie Meneveau, 0.6 bij 7 diam.
  windtype               = "Enercon-126",          # of "Betz". gebruik theorie of de Enercon-126 om opbrengst te bepalen. 
  opslaggrootte.TWh      = 3,                      # De grootte van de opslag in TWh, hier waterstof!!  
  efficientie.H2.naar.El = 0.5,                    # de efficiency van waterstof omzetten in stroom
  efficientie.El.naar.H2 = 0.8,                    # de efficiency van stroom omzetten in waterstof
  startmetvolleaccu      = TRUE,
  Periode                = "2019"
)
{
  # ---- BEPAAL DE OPBRENGST UIT VARIABELE BRONNEN ----
  
  # Opmerking: periodiciteit van de data is altijd 10min, sommeren naar langere tijdseenheid komt na de berekeningen.
  data = KNMI_wind_zon_10min[Periode]
  elektriciteitsverbruik = 1 - waterstofverbruik
  
  # --- bepaal de opbrengst voor wind ahv de meegegeven variabelen in kWh per km2  &checked
  if (windtype=="Enercon-126") {
    enercon.bezet.opp.m2 = (separatieafstand*126)^2   # sd^2
    data$Enercon126.Wm2  = veldefficientie * (data$Vermogen.Enercon126.W / enercon.bezet.opp.m2)  # Watten van de Enercon gedeeld door opp waar-i op staat, maal de efficientie van 't veld
    # nu de opbrengsten per 10min berekenen in kWh per km2
    data$opbrengst.wind.kWh.km2  = (data$Enercon126.Wm2/6 * 1e6) / 1000 
  }
  if (windtype=="Betz") {
    data$Betz.Wm2 = Betz.in.W.m2(data$wind140m, separatieafstand=separatieafstand, efficientie=efficientie)
    # nu de opbrengsten per 10min berekenen in kWh per km2
    data$opbrengst.wind.kWh.km2  = (data$Betz.Wm2/6 * 1e6) / 1000 
  }
  # --- bepaal de opbrengst voor zon ahv de meegegeven variabelen in kWh per km2 &checked
  data$opbrengst.zon.kWh.km2     = (rendement.zonnecellen * data$short.wave.downward.radiation.W.m2/6 * 1e6) /1000
  
  # --- MIX: Combineer de opbrengsten ---
  perio = periodiciteit.per.jaar(data) # alles gaat genormaliseerd worden naar jaarpercentages
  opslag.pct.jr = 100*(opslaggrootte.TWh/(jaarverbruik/1e9)) # Energieverbruik.NL.in.TWh.per.jaar
  
  # hoeveel produceren wind en zon nu eigenlijk in totaal tov het nationaal verbruik?
  dif = index(data)[nrow(data)] - index(data)[1]
  totaal.zon.pct.jr  = (sum(data$opbrengst.zonnecel.kWh.km2)*zonnecellen.km2 / as.numeric(dif/365)) / jaarverbruik
  totaal.wind.pct.jr = (sum(data$opbrengst.wind.kWh.km2)*windmolens.km2 / as.numeric(dif/365)) / jaarverbruik
  
  
  # ---- STEL DE PRODUCTIECIJFERS SAMEN -----------------------------------------------------
  
  mix = data[, c("opbrengst.wind.kWh.km2", "opbrengst.zon.kWh.km2")]
  
  # maak de opbrengst relatief tov het JAARgemiddelde, dus in procenten per periode van 10min t.o.v. het jaargemiddelde.
  mix$wind.pct.jr        = 100*(data$opbrengst.wind.kWh.km2 * windmolens.km2 / jaarverbruik)  
  mix$zon.pct.jr         = 100*(data$opbrengst.zon.kWh.km2 * zonnecellen.km2 / jaarverbruik) 
  mix$opbrengst.tot      = mix$wind.pct.jr + mix$zon.pct.jr                       # totaalopbrengst voor iedere 10 minuten van zon en wind
  #mix = mix[, -c(1:4)]
  
  # Definieer de vraag naar H2 als brandstof en elektriciteit in % van totaal jaarverbruik
  mix$vraag.el           = (100/perio) * elektriciteitsverbruik                  # maak gebruik van een vector, zodat later de vraag kan variëren ipv constant is
  mix$vraag.h2           = (100/perio) * waterstofverbruik                       # Voor nu is stroomvraag en h2-vraag constant
  
  # --- MODELIMPLEMENTATIE ---
  
  # --- Afname elektriciteit en waterstof van variabele bronnen
  
  # Elektriciteit (heeft geen verlies)
  mix$direct.el          = ifelse(mix$opbrengst.tot<mix$vraag.el, mix$opbrengst.tot, mix$vraag.el)
  # waterstof (heeft omzettingsverlies in deze stap)
  opbrengst_restant      = mix$opbrengst.tot-mix$direct.el
  vraag.aan.h2.vanaf.el  = mix$vraag.h2 / efficientie.El.naar.H2  
  mix$direct.h2          = ifelse(opbrengst_restant < vraag.aan.h2.vanaf.el, opbrengst_restant*efficientie.El.naar.H2,     mix$vraag.h2)
  mix$verlies.h2productie = ifelse(opbrengst_restant < vraag.aan.h2.vanaf.el, opbrengst_restant*(1-efficientie.El.naar.H2), vraag.aan.h2.vanaf.el*(1-efficientie.El.naar.H2))
  
  # --- Surplus ---
  # Als directe afname van stroom en waterstof zijn verrekend - hetgeen variabele bronnen direct konden leveren - dan kan het overschot worden 
  # uitgerekend wat beschikbaar is voor de opslag.
  mix$surplus            = mix$opbrengst.tot - mix$vraag.el - mix$vraag.h2/efficientie.El.naar.H2
  mix$surplus[mix$surplus<0] = 0
  
  # maak een loop die laadtoestand, hulpbron en verliezen tegelijkertijd uitrekent
  # in C++ voor snelheid.
  mix$tekort.el          = mix$vraag.el - mix$direct.el 
  mix$tekort.h2          = mix$vraag.h2 - mix$direct.h2 
  sourceCpp("productiecijfers2C.cpp")
  if (!startmetvolleaccu) { startwaarde = 0 } else {startwaarde = opslag.pct.jr}
  ot = prodcijfersC(mix, startwaarde, efficientie.H2.naar.El, efficientie.El.naar.H2, opslag.pct.jr)
  ot = xts(ot, index(mix))
  
  # ot heeft nieuwe data, en een doubleure
  mix = mix[ ,-which(colnames(mix)=="verlies.h2productie")]  # verwijder de doubleure
  mix = cbind(mix, ot)    # en voeg samen
  
  # # ---- maak een loop die laadtoestand, hulpbron en verliezen tegelijkertijd uitrekent ----
  # # in R ter controle
  # mix$tekort.el          = mix$vraag.el - mix$direct.el
  # mix$tekort.h2          = mix$vraag.h2 - mix$direct.h2
  # hulpbron               = rep(0, nrow(mix))
  # opslag                 = rep(0, nrow(mix))
  # onbenut                = rep(0, nrow(mix))
  # laadtoestand           = rep(0, nrow(mix))
  # verlies.hulpbron       = rep(0, nrow(mix))
  # verlies.opslag         = rep(0, nrow(mix))
  # opgeslagen             = rep(0, nrow(mix))
  # if (!startmetvolleaccu) { startwaarde = 0 } else {startwaarde = opslag.pct.jr}
  # laadtoestand[1] = startwaarde
  # for (i in 1:nrow(mix)) {
  #   opslag[i]   = 0
  #   hulpbron[i] = 0
  # 
  #   # Het tekort aan stroom wordt als eerste zoveel mogelijk uit de opslag gehaald
  #   if (laadtoestand[i] > mix$tekort.el[[i]]/efficientie.H2.naar.El) {
  #     opslag[i]              = mix$tekort.el[[i]]
  #     verlies.opslag[i]      = mix$tekort.el[[i]]/efficientie.H2.naar.El - mix$tekort.el[[i]]
  #     laadtoestand[i]        = laadtoestand[i] - mix$tekort.el[i]/efficientie.H2.naar.El
  #   } else {
  #     opslag[i]              = laadtoestand[i]*efficientie.H2.naar.El
  #     verlies.opslag[i]      = laadtoestand[i]*(1-efficientie.H2.naar.El)
  #     # en de hulpbron moet worden ingezet
  #     hulpbron[i]            = mix$tekort.el[[i]] - opslag[i]
  #     # geen verlies hier!
  #     laadtoestand[i]        = 0
  #   }
  # 
  #   # van wat er over is in de opslag, wordt gepoogd het tekort aan h2-vraag te voldoen
  #   if (laadtoestand[i]>mix$tekort.h2[[i]]) {
  #     opslag[i]              = opslag[i] + mix$tekort.h2[[i]]
  #     # geen verlies!
  #     laadtoestand[i]        = laadtoestand[i] - mix$tekort.h2[[i]]
  #   } else {
  #     opslag[i]              = opslag[i] + laadtoestand[i]
  #     # geen verlies!
  # 
  #     # en de hulpbron moet worden ingezet
  #     hulpbron[i]            = hulpbron[i] + mix$tekort.h2[i] - laadtoestand[i]
  #     verlies.hulpbron[i]    = hulpbron[i]/efficientie.El.naar.H2 - hulpbron[i]
  #     laadtoestand[i]        = 0
  #   }
  # 
  #   # het surplus vult de opslag - voor de volgende ronde
  #   if (i<nrow(mix)) {
  #     overloop = (laadtoestand[[i]] + mix$surplus[[i]] * efficientie.El.naar.H2) - opslag.pct.jr
  #     if (overloop > 0) {
  #       # oeps, opslag loopt over...
  #       laadtoestand[i+1]        = opslag.pct.jr;  # voller kan het niet
  #       mix$verlies.primair[[i]] = mix$verlies.primair[[i]] +
  #         (opslag.pct.jr-laadtoestand[i])/efficientie.El.naar.H2 * (1-efficientie.El.naar.H2)
  #       onbenut[i]               = mix$surplus[[i]] - ((opslag.pct.jr-laadtoestand[i])/efficientie.El.naar.H2)
  #       opgeslagen[i+1]          = opslag.pct.jr-laadtoestand[i]
  #     } else {
  #       # niets aan de hand, geen overloop
  #       laadtoestand[i+1]       = laadtoestand[i] + mix$surplus[[i]] * efficientie.El.naar.H2
  #       mix$verlies.primair[[i]] = mix$verlies.primair[[i]]  + mix$surplus[[i]] * (1-efficientie.El.naar.H2)
  #       onbenut[i]               = 0
  #       opgeslagen[i+1]          = mix$surplus[[i]] * efficientie.El.naar.H2
  #     }
  #   }
  # }
  # mix$hulpbron           = hulpbron
  # mix$opslag             = opslag
  # mix$onbenut            = onbenut
  # mix$laadtoestand       = laadtoestand
  # mix$verlies.opslag     = verlies.opslag
  # mix$verlies.hulpbron   = verlies.hulpbron
  # mix$opgeslagen         = opgeslagen
  # # vergelijk C++ code met R:
  # ot2 = mix[, c("hulpbron","opslag","onbenut","opgeslagen","laadtoestand","verlies.primair","verlies.hulpbron","verlies.opslag")]
  # identical(ot, ot2) # == TRUE

  # ---- verdeel de directe opbrenst voor El en H2 in een deel voor wind en een deel voor zon ----
  mix$wind       =  (mix$direct.h2+mix$direct.el) * (mix$wind.pct.jr/(mix$wind.pct.jr+mix$zon.pct.jr)) # schaalt combi naar windaandeel, probleem als wind en zon nul is
  mix$wind[is.na(mix$wind)] <- 0 # los 't probleem op
  mix$zon        =  (mix$direct.h2+mix$direct.el) * (mix$zon.pct.jr/(mix$wind.pct.jr+mix$zon.pct.jr)) # schaalt combi naar windaandeel, probleem als wind en zon nul is
  mix$zon[is.na(mix$zon)] <- 0 # los 't probleem op
  mix$verliezen  = mix$verlies.h2productie+mix$verlies.elproductie+mix$onbenut 

  # ---- CHECKS & BALANCES ----
  # neem als datum 1 dag, bijv 2007-03-15, zodat iedere bar zichtbaar is. Doe een
  # ggplot om te checken of alles correct wordt uitgerekend.
  
  # primaire opbrengst, uit opslag en bijdraaien
  # ggdata = xtsmelt(mix[,c("hulpbron","opslag","direct.h2","direct.el")]) # zon, wind
  # colnames(ggdata) = c("Date","Energieopbrengst","value")
  # ggplot(ggdata) +
  #   geom_bar(stat="identity", colour=NA, size=0, aes(fill=Energieopbrengst, y=value, x=Date)) +  # position="stack",
  #   geom_hline(yintercept=mix$vraag.el, linetype="dashed", color="red") +
  #   geom_hline(yintercept=mix$vraag.el+mix$vraag.h2, linetype="dashed", color="cyan")
  # 
  # # terug naar oude overzicht
  # ggdata2 = xtsmelt(mix[,c("verliezen","opgeslagen","hulpbron","opslag","wind","zon")])
  # colnames(ggdata2) = c("Date","Energieopbrengst","value")
  # ggplot(ggdata2) +
  #   geom_bar(stat="identity", colour=NA, size=0, aes(fill=Energieopbrengst, y=value, x=Date)) +
  #   geom_hline(yintercept=mix$vraag.el, linetype="dashed", color="red") +
  #   geom_hline(yintercept=mix$vraag.el+mix$vraag.h2, linetype="dashed", color="cyan") +
  #   scale_fill_manual(guide=guide_legend(reverse=T), values=c("#E8C4C4","#C6E1BF","#56B4E9","darkgreen","#39B54A","#FFDE17") ) + # mooie bruin: #E69F00 "#D78A7D", gr"#92B592, lichtgroen:E5FFE4
  #   theme(legend.position="top", legend.title = element_blank(), axis.title.x = element_blank())
  # 
  # ggplot(mix) +
  #   theme_maas() +
  #   scale_fill_manual(values = c(bkgrnd, 'darkgreen') ) +
  #   geom_bar(stat="identity", width=1, aes(x=Index, y=opslag.pct.jr, fill=bkgrnd)) +
  #   geom_bar(stat="identity", aes(x=Index, y=laadtoestand, fill='darkgreen'), colour='darkgreen') +
  #   geom_hline(yintercept=opslag.pct.jr, linetype="dashed", color = "darkgreen") +
  #   labs(title="Laadtoestand van de opslag", caption = figuurcaption) +
  #   theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_text(size = 9),
  #         plot.caption = element_text(hjust = 1, lineheight = 1.2)) + #family="serif"
  #   ylab("Laadtoestand (% van jaarverbruik)")
  
  # ---- bereken metrics ahv de data -------------------------------------------
  
  # verschillende conclusies
  # benodigde.buffergrootte.pct.jr = max(max(windzon5050$data$buffer), max(windzon5050$data$lopend_overschot)) # buffer die je nodig zou hebben zonder bijdraaien
  totale.energiebehoefte         = (100/perio) * nrow(mix) # het totale verbruik van de gehele beschouwde periode
  bijgedraaid.pct.van.totaal     = sum(mix$hulpbron[2:nrow(mix)])/totale.energiebehoefte
  via.opslag.pct.van.totaal      = sum(mix$opslag)/totale.energiebehoefte     # A   
  onbenut.pct.totaal             = sum(mix$onbenut)/totale.energiebehoefte
  surplus.pct.totaal             = sum(mix$surplus)/totale.energiebehoefte
  verlies.elproductie.pct.totaal = sum(mix$verlies.elproductie)/totale.energiebehoefte
  verlies.h2productie.pct.totaal = sum(mix$verlies.h2productie)/totale.energiebehoefte
  verliezen.pct.totaal           = sum(mix$verliezen)/totale.energiebehoefte    
  opgeslagen.pct.totaal          = sum(mix$opgeslagen)/totale.energiebehoefte      # B
  wind.pct.totaal                = sum(mix$wind)/totale.energiebehoefte
  zon.pct.totaal                 = sum(mix$zon)/totale.energiebehoefte
  min.cap.hulpbron               = max(mix$hulpbron[2:nrow(mix)]) / (100/perio)
  # hoeveel vermogen moet ik kwijt kunnen bij overschotdagen?
  # neem gemiddelde van de top 20%. (max vermogen meet je vóór efficientieverliezen
  # van de opslag). Uitkomst in % tov continu vermogen gedurende het jaar.
  set = mix$opgeslagen[mix$opgeslagen>0]/efficientie.El.naar.H2 # opslag naar h2 heeft verlies
  if (length(set)>0) {
    avg = ifelse(nrow(set)>0, mean(set[order(set, decreasing=TRUE)[1:round(nrow(set)/5)]]), 0)
    avg.max.laadvermogen           = avg / (100/perio)
    max.laadvermogen               = abs(max(set)) / (100/perio) # normaliseer naar jaar, over gehele periode
  } else {
    avg.max.laadvermogen           = 0
    max.laadvermogen               = 0
  }
  # ontlaadvermogen is gedefinieerd als het vermogen dat de afnemer ONTVANGT, niet het vermogen dat nodig is het te produceren
  max.ontlaadvermogen            = abs(max(mix$opslag)) / (100/perio) 
  
  perio.aantal.jaren = r2(interval(first(index(mix)), last(index(mix))) / years(1))
  
  result = list(data=mix, 
                metrics = data.frame(
                  naam=naam,
                  perio.per.jaar=perio,
                  perio.aantal.jaren=perio.aantal.jaren,
                  periode=Periode,
                  windpower=windtype,
                  
                  bijgedraaid.pct.van.totaal = bijgedraaid.pct.van.totaal,
                  via.opslag.pct.van.totaal  = via.opslag.pct.van.totaal,
                  onbenut.pct.totaal         = onbenut.pct.totaal,
                  surplus.pct.totaal         = surplus.pct.totaal,
                  verlies.elproductie.pct.totaal = verlies.elproductie.pct.totaal,
                  verlies.h2productie.pct.totaal = verlies.h2productie.pct.totaal,
                  verliezen.pct.totaal       = verliezen.pct.totaal,
                  opgeslagen.pct.totaal      = opgeslagen.pct.totaal,
                  wind.pct.totaal            = wind.pct.totaal,
                  zon.pct.totaal             = zon.pct.totaal,
                  min.cap.hulpbron           = min.cap.hulpbron,
                  avg.max.laadvermogen       = avg.max.laadvermogen,
                  max.laadvermogen           = max.laadvermogen,
                  max.ontlaadvermogen        = max.ontlaadvermogen,
                  max.ontlaadvermogen.e      = max.ontlaadvermogen*(1-waterstofverbruik),
                  totaal.wind.pct.jr         = totaal.wind.pct.jr,
                  totaal.zon.pct.jr          = totaal.zon.pct.jr,
                  # voor de uitkomsten is het beter met een volle accu te beginnen
                  pct.tijd.hulpbron.op.loadfactor0  = pct.vd.tijd.op.loadfactor(mix, 0.001, perio, perio.aantal.jaren),
                  pct.tijd.hulpbron.op.loadfactor50 = pct.vd.tijd.op.loadfactor(mix, 0.5, perio, perio.aantal.jaren),
                  pct.tijd.hulpbron.op.loadfactor90 = pct.vd.tijd.op.loadfactor(mix, 0.9, perio, perio.aantal.jaren),
                  
                  hulpcentrales         = r0(min.cap.hulpbron * TWh.jr.naar.GW(jaarverbruik/1e9) / 3), # centrales van 3 GW
                  waterstoffabrieken    = r0(TWh.jr.naar.GW(jaarverbruik/1e9)*avg.max.laadvermogen/0.1),               # elektrolysers van 100 MW
                  waterstofcentrales    = r0(TWh.jr.naar.GW(jaarverbruik/1e9)*max.ontlaadvermogen*(1-waterstofverbruik) / 3), # waterstofverbrandingscentrales a 3 GW
                  
                  jaarverbruik.TWh      = jaarverbruik/1e9,
                  waterstofverbruik     = waterstofverbruik,
                  windmolens.km2        = windmolens.km2,
                  zonnecellen.km2       = zonnecellen.km2,
                  opp.bezetting.wind    = windmolens.km2/Oppervlakte.Nederland.km2,
                  opp.bezetting.zon     = zonnecellen.km2/Oppervlakte.Nederland.km2,
                  rendement.zonnecellen = rendement.zonnecellen,          
                  separatieafstand      = separatieafstand,             
                  veldefficientie       = veldefficientie,  
                  efficientie.H2.naar.El = efficientie.H2.naar.El,
                  efficientie.El.naar.H2 = efficientie.El.naar.H2,
                  windtype              = windtype, 
                  opslaggrootte.TWh     = opslaggrootte.TWh,
                  opslag.pct.jr         = 100*(opslaggrootte.TWh/(jaarverbruik/1e9)),
                  
                  stringsAsFactors = F)
  )
  
  return(result) 
}


Bereken.Energiemix.Model3 <- function(
  naam                   = "Energiemix M2",
  jaarverbruik           = Energieverbruik.NL.kWh, # het totaalverbruik van Nederland
  waterstofverbruik      = 0.8,                    # verbruik aan waterstof als fractie van het jaarverbruik. Stroomverbruik is 1-waterstofverbruik.
  zonnecellen.km2        = 1500,                   # aantal km2 belegt met zonnecellen
  rendement.zonnecellen  = 0.2,                    # rendement van de zonnecellen, typisch 10-20%
  windmolens.km2         = 23000,                  # aandeel tov totaal jaarlijks energieverbruik NL
  separatieafstand       = 7,                      # hoe ver de molens uit elkaar staan in wiekdiameters
  veldefficientie        = 0.6,                    # de reductie van efficientie van het veld, zie Meneveau, 0.6 bij 7 diam.
  windtype               = "Enercon-126",          # of "Betz". gebruik theorie of de Enercon-126 om opbrengst te bepalen. 
  opslaggrootte.TWh      = 3,                      # De grootte van de opslag in TWh, hier waterstof!!  
  eff.R1.W               = 0.8,                    # Eff van productie elektriciteit naar H2. altijd vast op 0.8. Zie "Model - toegepaste efficienties.png"
  eff.R2.O               = 0.8,                    # Eff van productie elektriciteit naar H2. 0.8 in het geval van H2-opslag, 1 bij Li-opslag
  eff.H.W                = 0.8,                    # altijd vast op 0.8
  eff.O.W                = 1,                      # 1 in het geval van H2-opslag, 0.8 bij Li-opslag
  eff.O.E                = 0.5,                    # 0.5 in het geval van H2-opslag, 0.9 bij Li-opslag
  startmetvolleaccu      = TRUE,
  Periode                = "2019"
)
{
  # ---- BEPAAL DE OPBRENGST UIT VARIABELE BRONNEN ----
  
  # Opmerking: periodiciteit van de data is altijd 10min, sommeren naar langere tijdseenheid komt na de berekeningen.
  data = KNMI_wind_zon_10min[Periode]
  elektriciteitsverbruik = 1 - waterstofverbruik
  
  # --- bepaal de opbrengst voor wind ahv de meegegeven variabelen in kWh per km2  &checked
  if (windtype=="Enercon-126") {
    enercon.bezet.opp.m2 = (separatieafstand*126)^2   # sd^2
    data$Enercon126.Wm2  = veldefficientie * (data$Vermogen.Enercon126.W / enercon.bezet.opp.m2)  # Watten van de Enercon gedeeld door opp waar-i op staat, maal de efficientie van 't veld
    # nu de opbrengsten per 10min berekenen in kWh per km2
    data$opbrengst.wind.kWh.km2  = (data$Enercon126.Wm2/6 * 1e6) / 1000 
  }
  if (windtype=="Betz") {
    data$Betz.Wm2 = Betz.in.W.m2(data$wind140m, separatieafstand=separatieafstand, efficientie=efficientie)
    # nu de opbrengsten per 10min berekenen in kWh per km2
    data$opbrengst.wind.kWh.km2  = (data$Betz.Wm2/6 * 1e6) / 1000 
  }
  # --- bepaal de opbrengst voor zon ahv de meegegeven variabelen in kWh per km2 &checked
  data$opbrengst.zon.kWh.km2     = (rendement.zonnecellen * data$short.wave.downward.radiation.W.m2/6 * 1e6) /1000
  
  # --- MIX: Combineer de opbrengsten ---
  perio = periodiciteit.per.jaar(data) # alles gaat genormaliseerd worden naar jaarpercentages
  opslag.pct.jr = 100*(opslaggrootte.TWh/(jaarverbruik/1e9)) # Energieverbruik.NL.in.TWh.per.jaar
  
  # hoeveel produceren wind en zon nu eigenlijk in totaal tov het nationaal verbruik?
  dif = index(data)[nrow(data)] - index(data)[1]
  totaal.zon.pct.jr  = (sum(data$opbrengst.zonnecel.kWh.km2)*zonnecellen.km2 / as.numeric(dif/365)) / jaarverbruik
  totaal.wind.pct.jr = (sum(data$opbrengst.wind.kWh.km2)*windmolens.km2 / as.numeric(dif/365)) / jaarverbruik
  
  
  # ---- STEL DE PRODUCTIECIJFERS SAMEN -----------------------------------------------------
  
  mix = data[, c("opbrengst.wind.kWh.km2", "opbrengst.zon.kWh.km2")]
  
  # maak de opbrengst relatief tov het JAARgemiddelde, dus in procenten per periode van 10min t.o.v. het jaargemiddelde.
  mix$wind.pct.jr        = 100*(data$opbrengst.wind.kWh.km2 * windmolens.km2 / jaarverbruik)  
  mix$zon.pct.jr         = 100*(data$opbrengst.zon.kWh.km2 * zonnecellen.km2 / jaarverbruik) 
  mix$opbrengst.tot      = mix$wind.pct.jr + mix$zon.pct.jr                       # totaalopbrengst voor iedere 10 minuten van zon en wind
  #mix = mix[, -c(1:4)]
  
  # Definieer de vraag naar H2 als brandstof en elektriciteit in % van totaal jaarverbruik
  mix$vraag.el           = (100/perio) * elektriciteitsverbruik                  # maak gebruik van een vector, zodat later de vraag kan variëren ipv constant is
  mix$vraag.h2           = (100/perio) * waterstofverbruik                       # Voor nu is stroomvraag en h2-vraag constant
  
  # --- MODELIMPLEMENTATIE ---
  
  # --- Afname elektriciteit en waterstof van variabele bronnen
  
  # Elektriciteit (heeft geen verlies)
  mix$direct.el          = ifelse(mix$opbrengst.tot<mix$vraag.el, mix$opbrengst.tot, mix$vraag.el)
  # waterstof (heeft omzettingsverlies in deze stap)
  opbrengst_restant      = mix$opbrengst.tot-mix$direct.el
  vraag.aan.h2.vanaf.el  = mix$vraag.h2 / eff.R1.W  
  mix$direct.h2          = ifelse(opbrengst_restant < vraag.aan.h2.vanaf.el, opbrengst_restant*eff.R1.W,     mix$vraag.h2)
  mix$eff.verlies        = ifelse(opbrengst_restant < vraag.aan.h2.vanaf.el, opbrengst_restant*(1-eff.R1.W), vraag.aan.h2.vanaf.el*(1-eff.R1.W))
  
  # --- Surplus ---
  # Als directe afname van stroom en waterstof zijn verrekend - hetgeen variabele bronnen direct konden leveren - dan kan het overschot worden 
  # uitgerekend wat beschikbaar is voor de opslag.
  mix$surplus            = mix$opbrengst.tot - mix$vraag.el - mix$vraag.h2/eff.R1.W
  mix$surplus[mix$surplus<0] = 0
  
  # maak een loop die laadtoestand, hulpbron en verliezen tegelijkertijd uitrekent
  # in C++ voor snelheid.
  mix$tekort.el          = mix$vraag.el - mix$direct.el 
  mix$tekort.h2          = mix$vraag.h2 - mix$direct.h2 
  sourceCpp("productiecijfers3C.cpp")
  if (!startmetvolleaccu) { startwaarde = 0 } else {startwaarde = opslag.pct.jr}
  ot = prodcijfersC(mix, startwaarde, eff.R1.W,eff.R2.O,eff.H.W,eff.O.W,eff.O.E, opslag.pct.jr) 
  ot = xts(ot, index(mix))
  
  # ot heeft nieuwe data, voeg samen met bestaande data
  # ot en mix hebben een doubleure: eff.verlies. ot heeft de getallen meegenomen 
  # van de eerdere berekening en aangepast. Die gaan we dus gebruiken hier.
  mix = mix[, -which(colnames(mix)=="eff.verlies")]
  mix = cbind(mix, ot)    # en voeg samen
  
  # # ---- maak een loop die laadtoestand, hulpbron en verliezen tegelijkertijd uitrekent ----
  # # in R ter controle
  # mix$tekort.el          = mix$vraag.el - mix$direct.el
  # mix$tekort.h2          = mix$vraag.h2 - mix$direct.h2
  # hulpbron               = rep(0, nrow(mix))
  # opslag                 = rep(0, nrow(mix))
  # onbenut                = rep(0, nrow(mix))
  # laadtoestand           = rep(0, nrow(mix))
  # verlies.hulpbron       = rep(0, nrow(mix))
  # verlies.opslag         = rep(0, nrow(mix))
  # opgeslagen             = rep(0, nrow(mix))
  # if (!startmetvolleaccu) { startwaarde = 0 } else {startwaarde = opslag.pct.jr}
  # laadtoestand[1] = startwaarde
  # for (i in 1:nrow(mix)) {
  #   opslag[i]   = 0
  #   hulpbron[i] = 0
  # 
  #   # Het tekort aan stroom wordt als eerste zoveel mogelijk uit de opslag gehaald
  #   if (laadtoestand[i] > mix$tekort.el[[i]]/efficientie.H2.naar.El) {
  #     opslag[i]              = mix$tekort.el[[i]]
  #     verlies.opslag[i]      = mix$tekort.el[[i]]/efficientie.H2.naar.El - mix$tekort.el[[i]]
  #     laadtoestand[i]        = laadtoestand[i] - mix$tekort.el[i]/efficientie.H2.naar.El
  #   } else {
  #     opslag[i]              = laadtoestand[i]*efficientie.H2.naar.El
  #     verlies.opslag[i]      = laadtoestand[i]*(1-efficientie.H2.naar.El)
  #     # en de hulpbron moet worden ingezet
  #     hulpbron[i]            = mix$tekort.el[[i]] - opslag[i]
  #     # geen verlies hier!
  #     laadtoestand[i]        = 0
  #   }
  # 
  #   # van wat er over is in de opslag, wordt gepoogd het tekort aan h2-vraag te voldoen
  #   if (laadtoestand[i]>mix$tekort.h2[[i]]) {
  #     opslag[i]              = opslag[i] + mix$tekort.h2[[i]]
  #     # geen verlies!
  #     laadtoestand[i]        = laadtoestand[i] - mix$tekort.h2[[i]]
  #   } else {
  #     opslag[i]              = opslag[i] + laadtoestand[i]
  #     # geen verlies!
  # 
  #     # en de hulpbron moet worden ingezet
  #     hulpbron[i]            = hulpbron[i] + mix$tekort.h2[i] - laadtoestand[i]
  #     verlies.hulpbron[i]    = hulpbron[i]/efficientie.El.naar.H2 - hulpbron[i]
  #     laadtoestand[i]        = 0
  #   }
  # 
  #   # het surplus vult de opslag - voor de volgende ronde
  #   if (i<nrow(mix)) {
  #     overloop = (laadtoestand[[i]] + mix$surplus[[i]] * efficientie.El.naar.H2) - opslag.pct.jr
  #     if (overloop > 0) {
  #       # oeps, opslag loopt over...
  #       laadtoestand[i+1]        = opslag.pct.jr;  # voller kan het niet
  #       mix$verlies.primair[[i]] = mix$verlies.primair[[i]] +
  #         (opslag.pct.jr-laadtoestand[i])/efficientie.El.naar.H2 * (1-efficientie.El.naar.H2)
  #       onbenut[i]               = mix$surplus[[i]] - ((opslag.pct.jr-laadtoestand[i])/efficientie.El.naar.H2)
  #       opgeslagen[i+1]          = opslag.pct.jr-laadtoestand[i]
  #     } else {
  #       # niets aan de hand, geen overloop
  #       laadtoestand[i+1]       = laadtoestand[i] + mix$surplus[[i]] * efficientie.El.naar.H2
  #       mix$verlies.primair[[i]] = mix$verlies.primair[[i]]  + mix$surplus[[i]] * (1-efficientie.El.naar.H2)
  #       onbenut[i]               = 0
  #       opgeslagen[i+1]          = mix$surplus[[i]] * efficientie.El.naar.H2
  #     }
  #   }
  # }
  # mix$hulpbron           = hulpbron
  # mix$opslag             = opslag
  # mix$onbenut            = onbenut
  # mix$laadtoestand       = laadtoestand
  # mix$verlies.opslag     = verlies.opslag
  # mix$verlies.hulpbron   = verlies.hulpbron
  # mix$opgeslagen         = opgeslagen
  # # vergelijk C++ code met R:
  # ot2 = mix[, c("hulpbron","opslag","onbenut","opgeslagen","laadtoestand","verlies.primair","verlies.hulpbron","verlies.opslag")]
  # identical(ot, ot2) # == TRUE
  
  # ---- verdeel de directe opbrenst voor El en H2 in een deel voor wind en een deel voor zon ----
  mix$wind       =  (mix$direct.h2+mix$direct.el) * (mix$wind.pct.jr/(mix$wind.pct.jr+mix$zon.pct.jr)) # schaalt combi naar windaandeel, probleem als wind en zon nul is
  mix$wind[is.na(mix$wind)] <- 0 # los 't probleem op
  mix$zon        =  (mix$direct.h2+mix$direct.el) * (mix$zon.pct.jr/(mix$wind.pct.jr+mix$zon.pct.jr)) # schaalt combi naar windaandeel, probleem als wind en zon nul is
  mix$zon[is.na(mix$zon)] <- 0 # los 't probleem op
  mix$verliezen  = mix$eff.verlies + mix$onbenut 
  
  # ---- CHECKS & BALANCES ----
  # neem als datum 1 dag, bijv 2007-03-15, zodat iedere bar zichtbaar is. Doe een
  # ggplot om te checken of alles correct wordt uitgerekend.
  
  # primaire opbrengst, uit opslag en bijdraaien
  # ggdata = xtsmelt(mix[,c("hulpbron","opslag","direct.h2","direct.el")]) # zon, wind
  # colnames(ggdata) = c("Date","Energieopbrengst","value")
  # ggplot(ggdata) +
  #   geom_bar(stat="identity", colour=NA, size=0, aes(fill=Energieopbrengst, y=value, x=Date)) +  # position="stack",
  #   geom_hline(yintercept=mix$vraag.el, linetype="dashed", color="red") +
  #   geom_hline(yintercept=mix$vraag.el+mix$vraag.h2, linetype="dashed", color="cyan")
  # 
  # # terug naar oude overzicht
  # ggdata2 = xtsmelt(mix[,c("verliezen","opgeslagen","hulpbron","opslag","wind","zon")])
  # colnames(ggdata2) = c("Date","Energieopbrengst","value")
  # ggplot(ggdata2) +
  #   geom_bar(stat="identity", colour=NA, size=0, aes(fill=Energieopbrengst, y=value, x=Date)) +
  #   geom_hline(yintercept=mix$vraag.el, linetype="dashed", color="red") +
  #   geom_hline(yintercept=mix$vraag.el+mix$vraag.h2, linetype="dashed", color="cyan") +
  #   scale_fill_manual(guide=guide_legend(reverse=T), values=c("#E8C4C4","#C6E1BF","#56B4E9","darkgreen","#39B54A","#FFDE17") ) + # mooie bruin: #E69F00 "#D78A7D", gr"#92B592, lichtgroen:E5FFE4
  #   theme(legend.position="top", legend.title = element_blank(), axis.title.x = element_blank())
  # 
  # ggplot(mix) +
  #   theme_maas() +
  #   scale_fill_manual(values = c(bkgrnd, 'darkgreen') ) +
  #   geom_bar(stat="identity", width=1, aes(x=Index, y=opslag.pct.jr, fill=bkgrnd)) +
  #   geom_bar(stat="identity", aes(x=Index, y=laadtoestand, fill='darkgreen'), colour='darkgreen') +
  #   geom_hline(yintercept=opslag.pct.jr, linetype="dashed", color = "darkgreen") +
  #   labs(title="Laadtoestand van de opslag", caption = figuurcaption) +
  #   theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_text(size = 9),
  #         plot.caption = element_text(hjust = 1, lineheight = 1.2)) + #family="serif"
  #   ylab("Laadtoestand (% van jaarverbruik)")
  
  # ---- bereken metrics ahv de data -------------------------------------------
  
  # verschillende conclusies
  # benodigde.buffergrootte.pct.jr = max(max(windzon5050$data$buffer), max(windzon5050$data$lopend_overschot)) # buffer die je nodig zou hebben zonder bijdraaien
  totale.energiebehoefte         = (100/perio) * nrow(mix) # het totale verbruik van de gehele beschouwde periode
  bijgedraaid.pct.van.totaal     = sum(mix$hulpbron[2:nrow(mix)])/totale.energiebehoefte
  via.opslag.pct.van.totaal      = sum(mix$opslag)/totale.energiebehoefte     # A   
  onbenut.pct.totaal             = sum(mix$onbenut)/totale.energiebehoefte
  surplus.pct.totaal             = sum(mix$surplus)/totale.energiebehoefte
  eff.verlies                    = sum(mix$eff.verlies)/totale.energiebehoefte
  verliezen.pct.totaal           = sum(mix$verliezen)/totale.energiebehoefte    
  opgeslagen.pct.totaal          = sum(mix$opgeslagen)/totale.energiebehoefte      # B
  wind.pct.totaal                = sum(mix$wind)/totale.energiebehoefte
  zon.pct.totaal                 = sum(mix$zon)/totale.energiebehoefte
  min.cap.hulpbron               = max(mix$hulpbron[2:nrow(mix)]) / (100/perio)
  # hoeveel vermogen moet ik kwijt kunnen bij overschotdagen?
  # neem gemiddelde van de top 20%. (max vermogen meet je vóór efficientieverliezen
  # van de opslag). Uitkomst in % tov continu vermogen gedurende het jaar.
  # >> Neem alle 10min perioden, kijk naar opgeslagen energie  
  # >> opgeslagen energie is netto, dus om inputvermogen te krijgen, delen 
  #    we dat eerst door de efficientie van de omzetting van El naar H2
  set = mix$opgeslagen[mix$opgeslagen>0]/eff.R1.W # opslag naar h2 heeft verlies
  if (length(set)>0) {
    avg = ifelse(nrow(set)>0, mean(set[order(set, decreasing=TRUE)[1:round(nrow(set)/5)]]), 0)
    avg.max.laadvermogen           = avg / (100/perio)
    max.laadvermogen               = abs(max(set)) / (100/perio) # normaliseer naar jaar, over gehele periode
  } else {
    avg.max.laadvermogen           = 0
    max.laadvermogen               = 0
  }
  # ontlaadvermogen is gedefinieerd als het vermogen dat de afnemer ONTVANGT, niet het vermogen dat nodig is het te produceren
  max.ontlaadvermogen            = abs(max(mix$opslag)) / (100/perio) 
  
  perio.aantal.jaren = r2(interval(first(index(mix)), last(index(mix))) / years(1))
  
  result = list(data=mix, 
                metrics = data.frame(
                  naam=naam,
                  perio.per.jaar=perio,
                  perio.aantal.jaren=perio.aantal.jaren,
                  periode=Periode,
                  windpower=windtype,
                  
                  bijgedraaid.pct.van.totaal = bijgedraaid.pct.van.totaal,
                  via.opslag.pct.van.totaal  = via.opslag.pct.van.totaal,
                  onbenut.pct.totaal         = onbenut.pct.totaal,
                  surplus.pct.totaal         = surplus.pct.totaal,
                  verliezen.pct.totaal       = verliezen.pct.totaal, # verliezen is onbenut plus eff.verliezen
                  opgeslagen.pct.totaal      = opgeslagen.pct.totaal,
                  wind.pct.totaal            = wind.pct.totaal,
                  zon.pct.totaal             = zon.pct.totaal,
                  min.cap.hulpbron           = min.cap.hulpbron,
                  avg.max.laadvermogen       = avg.max.laadvermogen,
                  max.laadvermogen           = max.laadvermogen,
                  max.ontlaadvermogen.e      = max.ontlaadvermogen*(1-waterstofverbruik),
                  totaal.wind.pct.jr         = totaal.wind.pct.jr,
                  totaal.zon.pct.jr          = totaal.zon.pct.jr,
                  # voor de uitkomsten is het beter met een volle accu te beginnen
                  pct.tijd.hulpbron.op.loadfactor0  = pct.vd.tijd.op.loadfactor(mix, 0.001, perio, perio.aantal.jaren),
                  pct.tijd.hulpbron.op.loadfactor50 = pct.vd.tijd.op.loadfactor(mix, 0.5, perio, perio.aantal.jaren),
                  pct.tijd.hulpbron.op.loadfactor90 = pct.vd.tijd.op.loadfactor(mix, 0.9, perio, perio.aantal.jaren),
                  
                  hulpcentrales         = r0(min.cap.hulpbron * TWh.jr.naar.GW(jaarverbruik/1e9) / 3), # centrales van 3 GW
                  waterstoffabrieken    = r0(TWh.jr.naar.GW(jaarverbruik/1e9)*avg.max.laadvermogen/0.1),               # elektrolysers van 100 MW
                  waterstofcentrales    = r0(TWh.jr.naar.GW(jaarverbruik/1e9)*max.ontlaadvermogen*(1-waterstofverbruik) / 3), # waterstofverbrandingscentrales a 3 GW
                  
                  jaarverbruik.TWh      = jaarverbruik/1e9,
                  waterstofverbruik     = waterstofverbruik,
                  windmolens.km2        = windmolens.km2,
                  zonnecellen.km2       = zonnecellen.km2,
                  opp.bezetting.wind    = windmolens.km2/Oppervlakte.Nederland.km2,
                  opp.bezetting.zon     = zonnecellen.km2/Oppervlakte.Nederland.km2,
                  rendement.zonnecellen = rendement.zonnecellen,          
                  separatieafstand      = separatieafstand,             
                  veldefficientie       = veldefficientie,  
                  eff.verlies           = eff.verlies,
                  windtype              = windtype, 
                  opslaggrootte.TWh     = opslaggrootte.TWh,
                  opslag.pct.jr         = 100*(opslaggrootte.TWh/(jaarverbruik/1e9)),
                  
                  stringsAsFactors = F)
  )
  
  return(result) 
}

Energiemix.Visualisatie <- function(mix, 
                                    visualisatie.tijdschaal = c("10min","daily","monthly"),
                                    figuurtitel="Figuur X. Simulatie van een energiemix met verschillende bronnen",
                                    figuursubtitel="",
                                    figuurcaption="", figheights=c(9, 6)) 
{

   # --- AGGREGEREN GEDETAILLEERDE BEREKENING NAAR GROTERE EENHEDEN VOOR VISUALISERING ---
  visualisatie.tijdschaal = visualisatie.tijdschaal[1]
  if (visualisatie.tijdschaal=="10min") {
    demix = merge(mix$data$onbenut,mix$data$benut,mix$data$hulpbron,mix$data$opslag,mix$data$wind,mix$data$zon)
    demix2 = mix$data$laadtoestand
    printperio = mix$metrics$perio.per.jaar
    print.avg.pct = 100/printperio
  }
  if (visualisatie.tijdschaal=="daily") {
    demix = cbind(apply.daily(mix$data$onbenut, FUN=sum),
                  apply.daily(mix$data$benut, FUN=sum),
                  apply.daily(mix$data$hulpbron, FUN=sum),
                  apply.daily(mix$data$opslag, FUN=sum),
                  apply.daily(mix$data$wind, FUN=sum),
                  apply.daily(mix$data$zon, FUN=sum))
    demix2 = apply.daily(mix$data$laadtoestand, FUN=mean)
    printperio = 365
    print.avg.pct = 100/printperio
  }
  if (visualisatie.tijdschaal=="monthly") {
    demix = cbind(apply.monthly(mix$data$onbenut, FUN=sum),
                  apply.monthly(mix$data$benut, FUN=sum),
                  apply.monthly(mix$data$hulpbron, FUN=sum),
                  apply.monthly(mix$data$opslag, FUN=sum),
                  apply.monthly(mix$data$wind, FUN=sum),
                  apply.monthly(mix$data$zon, FUN=sum))
    demix2 = apply.monthly(mix$data$laadtoestand, FUN=mean)
    printperio = 12
    print.avg.pct = 100/printperio
  }
  
  # zorg dat de uitlijning van de datum op de x-as netjes is
  if (printperio==12)  index(demix) = as.Date(format(index(demix), "%Y-%m-15"))
  if (printperio==365) index(demix) = as.Date(format(index(demix), "%Y-%m-%d"))
  
  # bij maanden hebben we een probleem, want maanden zijn niet allemaal even lang.
  # Alles herschalen zodat we per maand netjes op 8,333% komen. Dat is niet
  # /helemaal/ correct, maar wel inzichtelijk en hoge precisie is voor dit
  # project niet belangrijk.
  #for (i in 2:4) demix[,i] = (demix[,i]/chk.gem.opbrengst) * print.avg.pct
  colnames(demix) = c("onbenut_surplus","benut_surplus","hulpbron","opslag","wind","zon")  
  data = xtsmelt(demix)
  colnames(data) = c("Date","Energieopbrengst","value")
  #data$Energieopbrengst = gsub("//."," ", data$Energieopbrengst)
  #data$Energieopbrengst = factor(data$Energieopbrengst, levels=c("onbenut surplus","benut surplus","via opslag","hulpbron","wind","zon"))
  
  plot.mix <- ggplot(data) + 
    geom_bar(stat="identity", colour=NA, size=0, aes(fill=Energieopbrengst, y=value, x=Date)) +  # position="stack", 
    geom_hline(yintercept=print.avg.pct, linetype="dashed", color="red") +
    theme_maas() + 
    labs(title    = figuurtitel, 
         subtitle = figuursubtitel) +
    ylab("Opbrengst (% van jaarverbruik)") + xlab("") +
    scale_fill_manual(values=c("#E8C4C4","#C6E1BF","#56B4E9","darkgreen","#39B54A","#FFDE17") ) + # mooie bruin: #E69F00 "#D78A7D", gr"#92B592, lichtgroen:E5FFE4
    theme(legend.position="top", legend.title = element_blank(), axis.title.x = element_blank())
  
  plot.opslag <- ggplot(demix2) +
    theme_maas() +
    scale_fill_manual(values = c(bkgrnd, 'darkgreen') ) +
    geom_bar(stat="identity", width=1, aes(x=Index, y=mix$metrics$opslag.pct.jr, fill=bkgrnd)) +
    geom_bar(stat="identity", aes(x=Index, y=laadtoestand, fill='darkgreen'), colour='darkgreen') +
    geom_hline(yintercept=mix$metrics$opslag.pct.jr, linetype="dashed", color = "darkgreen") +
    labs(title="Laadtoestand van de opslag", caption = figuurcaption) +
    theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_text(size = 9),
          plot.caption = element_text(hjust = 1, lineheight = 1.2)) + #family="serif"
    ylab("Laadtoestand (% van jaarverbruik)")
  
  return(grid.arrange(plot.mix, plot.opslag, ncol = 1, heights = figheights))

}

Energiemix.Visualisatie.TWh <- function(mix, 
                                    visualisatie.tijdschaal = c("10min","daily","monthly"),
                                    figuurtitel="Figuur X. Simulatie van een energiemix met verschillende bronnen",
                                    figuursubtitel="",
                                    figuurcaption="Data: KNMI   Platform", figheights=c(9, 6)) 
{
  
  # --- AGGREGEREN GEDETAILLEERDE BEREKENING NAAR GROTERE EENHEDEN VOOR VISUALISERING ---
  visualisatie.tijdschaal = visualisatie.tijdschaal[1]
  if (visualisatie.tijdschaal=="10min") {
    demix = merge(mix$data$onbenut,mix$data$benut,mix$data$hulpbron,mix$data$opslag,mix$data$wind,mix$data$zon)
    demix2 = mix$data$laadtoestand
    printperio = mix$metrics$perio.per.jaar
    print.avg.pct = (100/printperio) * mix$metrics$jaarverbruik.TWh/100
  }
  if (visualisatie.tijdschaal=="daily") {
    demix = cbind(apply.daily(mix$data$onbenut, FUN=sum),
                  apply.daily(mix$data$benut, FUN=sum),
                  apply.daily(mix$data$hulpbron, FUN=sum),
                  apply.daily(mix$data$opslag, FUN=sum),
                  apply.daily(mix$data$wind, FUN=sum),
                  apply.daily(mix$data$zon, FUN=sum))
    demix2 = apply.daily(mix$data$laadtoestand, FUN=mean)
    printperio = 365
    print.avg.pct = (100/printperio) * mix$metrics$jaarverbruik.TWh/100
  }
  if (visualisatie.tijdschaal=="monthly") {
    demix = cbind(apply.monthly(mix$data$onbenut, FUN=sum),
                  apply.monthly(mix$data$benut, FUN=sum),
                  apply.monthly(mix$data$hulpbron, FUN=sum),
                  apply.monthly(mix$data$opslag, FUN=sum),
                  apply.monthly(mix$data$wind, FUN=sum),
                  apply.monthly(mix$data$zon, FUN=sum))
    demix2 = apply.monthly(mix$data$laadtoestand, FUN=mean)
    printperio = 12
    print.avg.pct = (100/printperio) * mix$metrics$jaarverbruik.TWh/100
  }
  
  # zorg dat de uitlijning van de datum op de x-as netjes is
  if (printperio==12)  index(demix) = as.Date(format(index(demix), "%Y-%m-15"))
  if (printperio==365) index(demix) = as.Date(format(index(demix), "%Y-%m-%d"))
  
  # bij maanden hebben we een probleem, want maanden zijn niet allemaal even lang.
  # Alles herschalen zodat we per maand netjes op 8,333% komen. Dat is niet
  # /helemaal/ correct, maar wel inzichtelijk en hoge precisie is voor dit
  # project niet belangrijk.
  # for (i in 2:4) demix[,i] = (demix[,i]/chk.gem.opbrengst) * print.avg.pct
  
  # Ver-TWh het percentage
  demix  = mix$metrics$jaarverbruik.TWh * demix / 100
  demix2 = mix$metrics$jaarverbruik.TWh * demix2 / 100
  colnames(demix) = c("verliezen","opgeslagen","hulpbron","ontrokken","wind","zon")  
  data = xtsmelt(demix)
  colnames(data) = c("Date","Energieopbrengst","value")
  #data$Energieopbrengst = gsub("//."," ", data$Energieopbrengst)
  #data$Energieopbrengst = factor(data$Energieopbrengst, levels=c("onbenut surplus","benut surplus","via opslag","hulpbron","wind","zon"))
  
  plot.mix <- ggplot(data) + 
    geom_bar(stat="identity", colour=NA, size=0, aes(fill=Energieopbrengst, y=value, x=Date)) +  # position="stack", 
    geom_hline(yintercept=print.avg.pct, linetype="dashed", color="red") +
    theme_maas() + 
    labs(title    = figuurtitel, 
         subtitle = figuursubtitel) +
    ylab("Opbrengst (TWh)") + xlab("") +
    scale_fill_manual(guide=guide_legend(reverse=T), values=c("#E8C4C4","#C6E1BF","#56B4E9","darkgreen","#39B54A","#FFDE17") ) + # mooie bruin: #E69F00 "#D78A7D", gr"#92B592, lichtgroen:E5FFE4
    theme(legend.position="top", legend.title = element_blank(), axis.title.x = element_blank())
  
  plot.opslag <- ggplot(demix2) +
    theme_maas() +
    scale_fill_manual(values = c(bkgrnd, 'darkgreen') ) +
    geom_bar(stat="identity", width=1, aes(x=Index, y=mix$metrics$opslaggrootte.TWh, fill=bkgrnd)) +
    geom_bar(stat="identity", aes(x=Index, y=laadtoestand, fill='darkgreen'), colour='darkgreen') +
    geom_hline(yintercept=mix$metrics$opslaggrootte.TWh, linetype="dashed", color = "darkgreen") +
    labs(title="Virtuele laadtoestand", caption = figuurcaption) +
    theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_text(size = 9),
          plot.caption = element_text(hjust = 1, lineheight = 1.2)) + #family="serif"
    ylab("Laadtoestand (TWh)")
  
  return(grid.arrange(plot.mix, plot.opslag, ncol = 1, heights = figheights))
  
}

Energiemix.Visualisatie2.TWh <- function(mix, 
                                         visualisatie.tijdschaal = c("10min","daily","monthly"),
                                         figuurtitel="Figuur X. Simulatie van een energiemix met verschillende bronnen",
                                         figuursubtitel="",
                                         figuurcaption="Data: KNMI Data Platform", figheights=c(9, 6)) 
{
  
  # --- AGGREGEREN GEDETAILLEERDE BEREKENING NAAR GROTERE EENHEDEN VOOR VISUALISERING ---
  visualisatie.tijdschaal = visualisatie.tijdschaal[1]
  if (visualisatie.tijdschaal=="10min") {
    demix = merge(mix$data$onbenut,mix$data$benut,mix$data$hulpbron,mix$data$opslag,mix$data$wind,mix$data$zon)
    demix2 = mix$data$laadtoestand
    printperio = mix$metrics$perio.per.jaar
    print.avg.pct = (100/printperio) * mix$metrics$jaarverbruik.TWh/100
  }
  if (visualisatie.tijdschaal=="daily") {
    demix = cbind(apply.daily(mix$data$onbenut, FUN=sum),
                  apply.daily(mix$data$benut, FUN=sum),
                  apply.daily(mix$data$hulpbron, FUN=sum),
                  apply.daily(mix$data$opslag, FUN=sum),
                  apply.daily(mix$data$wind, FUN=sum),
                  apply.daily(mix$data$zon, FUN=sum))
    demix2 = apply.daily(mix$data$laadtoestand, FUN=mean)
    printperio = 365
    print.avg.pct = (100/printperio) * mix$metrics$jaarverbruik.TWh/100
  }
  if (visualisatie.tijdschaal=="monthly") {
    demix = cbind(apply.monthly(mix$data$onbenut, FUN=sum),
                  apply.monthly(mix$data$benut, FUN=sum),
                  apply.monthly(mix$data$hulpbron, FUN=sum),
                  apply.monthly(mix$data$opslag, FUN=sum),
                  apply.monthly(mix$data$wind, FUN=sum),
                  apply.monthly(mix$data$zon, FUN=sum))
    demix2 = apply.monthly(mix$data$laadtoestand, FUN=mean)
    printperio = 12
    print.avg.pct = (100/printperio) * mix$metrics$jaarverbruik.TWh/100
  }
  
  # zorg dat de uitlijning van de datum op de x-as netjes is
  if (printperio==12)  index(demix) = as.Date(format(index(demix), "%Y-%m-15"))
  if (printperio==365) index(demix) = as.Date(format(index(demix), "%Y-%m-%d"))
  
  # bij maanden hebben we een probleem, want maanden zijn niet allemaal even lang.
  # Alles herschalen zodat we per maand netjes op 8,333% komen. Dat is niet
  # /helemaal/ correct, maar wel inzichtelijk en hoge precisie is voor dit
  # project niet belangrijk.
  # for (i in 2:4) demix[,i] = (demix[,i]/chk.gem.opbrengst) * print.avg.pct
  
  # Ver-TWh het percentage
  demix  = mix$metrics$jaarverbruik.TWh * demix / 100
  demix2 = mix$metrics$jaarverbruik.TWh * demix2 / 100
  colnames(demix) = c("verliezen","opgeslagen","hulpbron","opslag","wind","zon")  
  data = xtsmelt(demix)
  colnames(data) = c("Date","Energieopbrengst","value")
  #data$Energieopbrengst = gsub("\\."," ", data$Energieopbrengst)
  #data$Energieopbrengst = factor(data$Energieopbrengst, levels=c("onbenut surplus","benut surplus","via opslag","hulpbron","wind","zon"))
  
  plot.mix <- ggplot(data) +
    geom_bar(stat="identity", colour=NA, size=0, aes(fill=Energieopbrengst, y=value, x=Date)) +  # position="stack", 
    geom_hline(yintercept=print.avg.pct, linetype="dashed", color="red") +
    theme_maas() %+replace% 
    theme(plot.margin  = margin(0, 7, 2, 7, "mm")) + 
    labs(title = figuurtitel, subtitle = figuursubtitel) +
    ylab("Opbrengst (TWh)") + xlab("") +
    scale_fill_manual(guide=guide_legend(reverse=T), values=c("#E8C4C4","#C6E1BF","#56B4E9","darkgreen","#39B54A","#FFDE17") ) + # mooie bruin: #E69F00 "#D78A7D", gr"#92B592, lichtgroen:E5FFE4
    theme(legend.position="top", legend.title=element_blank(), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) 
  
  plot.opslag <- ggplot(demix2) +
    theme_maas() %+replace% 
    theme(
      plot.title        = element_blank(),
      plot.subtitle     = element_blank(),
      plot.margin       = margin(0, 7, 5, 7, "mm")
    ) +
    scale_fill_manual(values = c(bkgrnd, 'darkgreen') ) +
    geom_bar(stat="identity", width=1, aes(x=Index, y=mix$metrics$opslaggrootte.TWh, fill=bkgrnd)) +
    geom_bar(stat="identity", aes(x=Index, y=laadtoestand, fill='darkgreen'), colour='darkgreen') +
    geom_hline(yintercept=mix$metrics$opslaggrootte.TWh, linetype="dashed", color = "darkgreen") +
    labs(caption = figuurcaption) +
    theme(legend.position="none", axis.title.x = element_blank(), 
          plot.caption = element_text(hjust = 1, lineheight = 1.2)) + #family="serif"
    ylab("Laadtoestand (TWh)")
  
  #return(grid.arrange(plot.mix, plot.opslag, ncol = 1, heights = figheights))
  
  return( ggarrange(plot.mix, plot.opslag, heights = c(2, 0.9), ncol = 1, nrow = 2, align = "v") # +
          #annotation_custom(grid.rect(x=0.007, y=0.935, hjust = 1, vjust=0, gp=gpar(fill='#704904',lwd=0)))
  )
}

Energiemix.Visualisatie3.TWh <- function(mix, 
                                        visualisatie.tijdschaal = c("10min","daily","monthly"),
                                        figuurtitel="Energiemix (Elektrificatie + verbranding H2)",
                                        figuursubtitel="",
                                        figuurcaption="", figheights=c(2, 0.9)) 
{
  
  # --- AGGREGEREN GEDETAILLEERDE BEREKENING NAAR GROTERE EENHEDEN VOOR VISUALISERING ---
  visualisatie.tijdschaal = visualisatie.tijdschaal[1]
  if (visualisatie.tijdschaal=="10min") {
    demix = merge(mix$data$verliezen,mix$data$opgeslagen,mix$data$hulpbron,mix$data$opslag,mix$data$wind,mix$data$zon)
    demix2 = mix$data$laadtoestand
    printperio = mix$metrics$perio.per.jaar
    print.avg.pct = (100/printperio) * mix$metrics$jaarverbruik.TWh/100
  }
  if (visualisatie.tijdschaal=="daily") {
    demix = cbind(apply.daily(mix$data$verliezen, FUN=sum),
                  apply.daily(mix$data$opgeslagen, FUN=sum),
                  apply.daily(mix$data$hulpbron, FUN=sum),
                  apply.daily(mix$data$opslag, FUN=sum),
                  apply.daily(mix$data$wind, FUN=sum),
                  apply.daily(mix$data$zon, FUN=sum))
    demix2 = apply.daily(mix$data$laadtoestand, FUN=mean)
    printperio = 365
    print.avg.pct = (100/printperio) * mix$metrics$jaarverbruik.TWh/100
  }
  if (visualisatie.tijdschaal=="monthly") {
    demix = cbind(apply.monthly(mix$data$verliezen, FUN=sum),
                  apply.monthly(mix$data$opgeslagen, FUN=sum),
                  apply.monthly(mix$data$hulpbron, FUN=sum),
                  apply.monthly(mix$data$opslag, FUN=sum),
                  apply.monthly(mix$data$wind, FUN=sum),
                  apply.monthly(mix$data$zon, FUN=sum))
    demix2 = apply.monthly(mix$data$laadtoestand, FUN=mean)
    printperio = 12
    print.avg.pct = (100/printperio) * mix$metrics$jaarverbruik.TWh/100
  }
  
  # zorg dat de uitlijning van de datum op de x-as netjes is
  if (printperio==12)  index(demix) = as.Date(format(index(demix), "%Y-%m-15"))
  if (printperio==365) index(demix) = as.Date(format(index(demix), "%Y-%m-%d"))
  
  # bij maanden hebben we een probleem, want maanden zijn niet allemaal even lang.
  # Alles herschalen zodat we per maand netjes op 8,333% komen. Dat is niet
  # /helemaal/ correct, maar wel inzichtelijk en hoge precisie is voor dit
  # project niet belangrijk.
  # for (i in 2:4) demix[,i] = (demix[,i]/chk.gem.opbrengst) * print.avg.pct
  
  # Ver-TWh het percentage
  demix  = mix$metrics$jaarverbruik.TWh * demix / 100
  demix2 = mix$metrics$jaarverbruik.TWh * demix2 / 100
  # colnames(demix) = c("verliezen","opgeslagen","hulpbron","opslag","wind","zon")  
  data = xtsmelt(demix)
  colnames(data) = c("Date","Energieopbrengst","value")
  #data$Energieopbrengst = gsub("//."," ", data$Energieopbrengst)
  #data$Energieopbrengst = factor(data$Energieopbrengst, levels=c("onbenut surplus","benut surplus","via opslag","hulpbron","wind","zon"))
  
  if (figuursubtitel=="(beschrijf)") {
    # verzin een beschrijving van de settings
    figuursubtitel = paste0("Oppervlakte: wind ", r0(100*mix$metrics$opp.bezetting.wind), "% NL, zon ",  r0(100*mix$metrics$opp.bezetting.zon), "% NL \n",
           "Verbruik: waterstof ", r0(100*mix$metrics$waterstofverbruik), "%, elektriciteit ", r0(100*(1-mix$metrics$waterstofverbruik)), "%"
    )
  }
  
  plot.mix <- ggplot(data) +
    geom_bar(stat="identity", colour=NA, size=0, aes(fill=Energieopbrengst, y=value, x=Date)) +  # position="stack", 
    geom_hline(yintercept=print.avg.pct, linetype="dashed", color="red") +
    theme_maas() %+replace% 
    theme(plot.margin  = margin(0, 7, 2, 7, "mm")) + 
    ylab("Opbrengst (TWh)") + xlab("") +
    scale_fill_manual(guide=guide_legend(reverse=T), values=c("#E8C4C4","#C6E1BF","#56B4E9","darkgreen","#39B54A","#FFDE17") ) + # mooie bruin: #E69F00 "#D78A7D", gr"#92B592, lichtgroen:E5FFE4
    theme(legend.position="top") 

  if (figuursubtitel != "") {
    plot.mix <- plot.mix  + labs(title = figuurtitel, subtitle = figuursubtitel) 
  } else {
    plot.mix <- plot.mix  + labs(title = figuurtitel) 
  }
  
  plot.opslag <- ggplot(demix2) +
    theme_maas() %+replace% 
    theme(
      plot.title        = element_blank(),
      plot.subtitle     = element_blank(),
      plot.margin       = margin(0, 7, 5, 7, "mm")
    ) +
    scale_fill_manual(values = c(bkgrnd, 'darkgreen') ) +
    geom_bar(stat="identity", width=1, aes(x=Index, y=mix$metrics$opslaggrootte.TWh, fill=bkgrnd)) +
    geom_bar(stat="identity", aes(x=Index, y=laadtoestand, fill='darkgreen'), colour='darkgreen') +
    geom_hline(yintercept=mix$metrics$opslaggrootte.TWh, linetype="dashed", color = "darkgreen") +
    theme(legend.position="none", axis.title.x = element_blank(), plot.caption = element_blank()) + #family="serif"
    ylab("Laadtoestand (TWh)")
  
  #return(grid.arrange(plot.mix, plot.opslag, ncol = 1, heights = figheights))

  if (figheights[2]==0) {
    return(plot.mix)
  } else {
    plot.mix <- plot.mix + theme(legend.title=element_blank(), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) 
    return( ggarrange(plot.mix, plot.opslag, heights = figheights, ncol = 1, nrow = 2, align = "v") # +
            #annotation_custom(grid.rect(x=0.007, y=0.935, hjust = 1, vjust=0, gp=gpar(fill='#704904',lwd=0)))
    )
  }
}

Inventarisvisualisatie <- function(mix) {
   
}


# ----- TABEL MET VERSCHILLENDE ENERGIEMIXEN -----------------------------------

MaakResultatenTabel <- function(mixen) {
  results = NULL
  for (l in 1:length(mixen)) {
    mix = mixen[[l]]
    rw = data.frame(
      naam         = mix$metrics$naam,
      jaarverbruik = mix$metrics$jaarverbruik.TWh,
      
      # ruimtegebruik
      opp.zon        = 100*mix$metrics$opp.bezetting.zon,
      opp.wind       = 100*mix$metrics$opp.bezetting.wind,
      
      # capaciteit van de bron tov de (nationale) energievraag
      # "hoe groot is de investering, het wagenpark dat je hebt staan"
      cap.zon        = 100*mix$metrics$totaal.zon.pct.jr,
      cap.wind       = 100*mix$metrics$totaal.wind.pct.jr,
      cap.hulpbron   = mix$metrics$min.cap.hulpbron * 1000*mix$metrics$jaarverbruik.TWh/(365*24),
      cap.opslag     = mix$metrics$opslag.pct.jr,
      
      # daadwerkelijk geleverde hoeveelheid energie
      zondirect      = 100*mix$metrics$zon.pct.totaal,
      winddirect     = 100*mix$metrics$wind.pct.totaal,
      bijgedraaid    = 100*mix$metrics$bijgedraaid.pct.van.totaal,
      uit.opslag     = 100*mix$metrics$via.opslag.pct.van.totaal,
      
      # verliezen
      onbenut             = 100*mix$metrics$onbenut.pct.totaal,
      omzettingsverliezen = ifelse("opslagefficientie" %in% colnames(mix$metrics), 100*mix$metrics$onbenut.pct.totaal ,100*mix$metrics$omzettingsverliezen.pct.totaal),
      tot.verliezen       = ifelse("opslagefficientie" %in% colnames(mix$metrics), 100*mix$metrics$onbenut.pct.totaal ,100*mix$metrics$verliezen.pct.totaal),
      
      # statistieken mbt hulpbron
      hulptijd.lf0            = 100*mix$metrics$pct.tijd.hulpbron.op.loadfactor0,
      hulptijd.lf90           = 100*mix$metrics$pct.tijd.hulpbron.op.loadfactor90,
      
      # statistieken mbt opslag
      max.laadvermogen        = mix$metrics$max.laadvermogen * 1000*mix$metrics$jaarverbruik.TWh/(365*24),
      avg.max.laadvermogen    = mix$metrics$avg.max.laadvermogen * 1000*mix$metrics$jaarverbruik.TWh/(365*24), # bepaald ook de grootte van de installatie/ aantal fabrieken
      max.ontlaadvermogen     = mix$metrics$max.ontlaadvermogen * 1000*mix$metrics$jaarverbruik.TWh/(365*24),
      
      opslagefficientie       = ifelse("opslagefficientie" %in% colnames(mix$metrics), 100*mix$metrics$opslagefficientie, NA),
      efficientie.El.naar.H2  = ifelse("efficientie.El.naar.H2" %in% colnames(mix$metrics), 100*mix$metrics$efficientie.El.naar.H2, NA),
      efficientie.H2.naar.El  = ifelse("efficientie.El.naar.H2" %in% colnames(mix$metrics), 100*mix$metrics$efficientie.H2.naar.El, NA),
      
      stringsAsFactors= FALSE
    )
    results = rbind(results, rw)
  }
  
  return(results)
}

MaakResultatenTabelModel2 <- function(mixen) {
  results = NULL
  for (l in 1:length(mixen)) {
    mix = mixen[[l]]
    rw = data.frame(
      naam                   = mix$metrics$naam,
      
      # verbruik
      jaarverbruik           = mix$metrics$jaarverbruik.TWh,
      waterstofverbruik      = ifelse("opslagefficientie" %in% colnames(mix$metrics), NA, 100*mix$metrics$waterstofverbruik),
      elektriciteitsverbruik = ifelse("opslagefficientie" %in% colnames(mix$metrics), NA, 100*(1-mix$metrics$waterstofverbruik)),
      
      # ruimtegebruik
      opp.zon        = 100*mix$metrics$opp.bezetting.zon,
      opp.wind       = 100*mix$metrics$opp.bezetting.wind,
      
      # capaciteit van de bron tov de (nationale) energievraag
      # "hoe groot is de investering, het wagenpark dat je hebt staan"
      cap.zon        = 100*mix$metrics$totaal.zon.pct.jr,
      cap.wind       = 100*mix$metrics$totaal.wind.pct.jr,
      cap.hulpbron   = 100*mix$metrics$min.cap.hulpbron,
      cap.opslag     = (mix$metrics$opslag.pct.jr/100)*mix$metrics$jaarverbruik.TWh,
      
      # daadwerkelijk geleverde hoeveelheid energie
      zondirect      = 100*mix$metrics$zon.pct.totaal,
      winddirect     = 100*mix$metrics$wind.pct.totaal,
      bijgedraaid    = 100*mix$metrics$bijgedraaid.pct.van.totaal,
      uit.opslag     = 100*mix$metrics$via.opslag.pct.van.totaal,
      
      # verliezen
      onbenut              = 100*mix$metrics$onbenut.pct.totaal,
      verlies.elproductie  = ifelse("opslagefficientie" %in% colnames(mix$metrics), NA ,100*mix$metrics$verlies.elproductie.pct.totaal),
      verlies.h2productie  = ifelse("opslagefficientie" %in% colnames(mix$metrics), NA ,100*mix$metrics$verlies.h2productie.pct.totaal),
      
      # statistieken mbt hulpbron
      hulptijd.lf0         = 100*mix$metrics$pct.tijd.hulpbron.op.loadfactor0,
      hulptijd.lf90        = 100*mix$metrics$pct.tijd.hulpbron.op.loadfactor90,
      
      # statistieken mbt opslag
      max.laadvermogen     = mix$metrics$max.laadvermogen     * 1000*mix$metrics$jaarverbruik.TWh/(365*24),
      avg.max.laadvermogen = mix$metrics$avg.max.laadvermogen * 1000*mix$metrics$jaarverbruik.TWh/(365*24), # bepaald ook de grootte van de installatie/ aantal fabrieken
      max.ontlaadvermogen  = mix$metrics$max.ontlaadvermogen  * 1000*mix$metrics$jaarverbruik.TWh/(365*24),
      
      opslagefficientie       = ifelse("opslagefficientie" %in% colnames(mix$metrics), 100*mix$metrics$opslagefficientie, NA),
      efficientie.El.naar.H2  = ifelse("efficientie.El.naar.H2" %in% colnames(mix$metrics), 100*mix$metrics$efficientie.El.naar.H2, NA),
      efficientie.H2.naar.El  = ifelse("efficientie.El.naar.H2" %in% colnames(mix$metrics), 100*mix$metrics$efficientie.H2.naar.El, NA),
      
      # inventaris
      hulpcentrales        = mix$metrics$hulpcentrales,
      waterstoffabrieken   = mix$metrics$waterstoffabrieken,
      waterstofcentrales   = mix$metrics$waterstofcentrales,
      
      # tijdsperiode waarover de simulatie werd gedaan
      periode                = mix$metrics$periode,
      
      stringsAsFactors= FALSE
    )
    results = rbind(results, rw)
  }
  
  return(results)
}

MaakResultatenTabelModel3 <- function(mixen) {
  results = NULL
  for (l in 1:length(mixen)) {
    mix = mixen[[l]]
    rw = data.frame(
      naam                   = mix$metrics$naam,
      
      # verbruik
      jaarverbruik           = mix$metrics$jaarverbruik.TWh,
      waterstofverbruik      = 100*mix$metrics$waterstofverbruik,
      elektriciteitsverbruik = 100*(1-mix$metrics$waterstofverbruik),
      
      # ruimtegebruik
      opp.zon        = 100*mix$metrics$opp.bezetting.zon,
      opp.wind       = 100*mix$metrics$opp.bezetting.wind,
      
      # capaciteit van de bron tov de (nationale) energievraag
      # "hoe groot is de investering, het wagenpark dat je hebt staan"
      cap.zon        = 100*mix$metrics$totaal.zon.pct.jr,
      cap.wind       = 100*mix$metrics$totaal.wind.pct.jr,
      cap.hulpbron   = 100*mix$metrics$min.cap.hulpbron,
      cap.opslag     = (mix$metrics$opslag.pct.jr/100)*mix$metrics$jaarverbruik.TWh,
      
      # daadwerkelijk geleverde hoeveelheid energie
      zondirect      = 100*mix$metrics$zon.pct.totaal,
      winddirect     = 100*mix$metrics$wind.pct.totaal,
      bijgedraaid    = 100*mix$metrics$bijgedraaid.pct.van.totaal,
      uit.opslag     = 100*mix$metrics$via.opslag.pct.van.totaal,
      
      # verliezen
      onbenut               = 100*mix$metrics$onbenut.pct.totaal,
      eff.verlies           = 100*mix$metrics$eff.verlies,
       
      # statistieken mbt hulpbron
      hulptijd.lf0          = 100*mix$metrics$pct.tijd.hulpbron.op.loadfactor0,
      hulptijd.lf90         = 100*mix$metrics$pct.tijd.hulpbron.op.loadfactor90,
      
      # statistieken mbt opslag
      max.laadvermogen      = mix$metrics$max.laadvermogen      * 1000*mix$metrics$jaarverbruik.TWh/(365*24),
      avg.max.laadvermogen  = mix$metrics$avg.max.laadvermogen  * 1000*mix$metrics$jaarverbruik.TWh/(365*24), # bepaald ook de grootte van de installatie/ aantal fabrieken
      max.ontlaadvermogen.e = mix$metrics$max.ontlaadvermogen.e * 1000*mix$metrics$jaarverbruik.TWh/(365*24),
      
      # inventaris
      hulpcentrales        = mix$metrics$hulpcentrales,
      waterstoffabrieken   = mix$metrics$waterstoffabrieken,
      waterstofcentrales   = mix$metrics$waterstofcentrales,
      
      # tijdsperiode waarover de simulatie werd gedaan
      periode              = mix$metrics$periode,
      
      stringsAsFactors= FALSE
    )
    results = rbind(results, rw)
  }
  
  return(results)
}

MaakInventarisLijst <- function(results) {
  # vertaling resultaten naar inventaris
  # Doel is begrijpelijke lijst te krijgen van de benodigde hardware
  TWh.jr.naar.GW <- function(x) { 1000 * x/(365*24)}
  inventaris = results[,c("naam","jaarverbruik","opp.zon","opp.wind"), drop=FALSE]
  inventaris[, c("jaarverbruik","opp.zon","opp.wind")] = r0(inventaris[, c("jaarverbruik","opp.zon","opp.wind")])
  inventaris$hulpcentrales = r0((results$cap.hulpbron/100)*TWh.jr.naar.GW(results$jaarverbruik) / 2.9)  # centrales van 2.9 GW
  inventaris$opslag        = r1(results$cap.opslag)  # opslag in TWh
  inventaris$h2centrales   = r0(results$avg.max.laadvermogen / 0.1)  # centrales van 0.1 GW (100 MW)
  inventaris$Li.voorraad   = r0(100*results$cap.opslag/1.1922)   # r1(results$cap.opslag / ((82*8.7e6)/1e9))  # hoeveel maal tov cap totaal geelektrificeerd wagenpark NL (8,7 milj auto's)
  # inventaris[4,c(6:7)] <- "-"
  # inventaris[c(1,3),7] <- "-"
  # inventaris[c(1,2,4,8),8] <- "-"
  # inventaris[c(7,9),7] <- "-"
  # inventaris[9,5] <- "0"
  
  # formatting for print
  inventaris$opp.zon  = paste0(inventaris$opp.zon, " %")
  inventaris$opp.wind = paste0(inventaris$opp.wind, " %")
  
  return(inventaris)
}

MaakUitkomstenTabel <- function(metrics) {
  tabel = data.frame( 
    # capaciteit, directe levering aan gebruikers, levering aan opslag, inventaris, max geleverd vermogen, max ontvangen vermogen
    zon = c( paste0(r0(100*metrics$totaal.zon.pct.jr),"%"), 
             paste0("(",r0(metrics$totaal.zon.pct.jr*metrics$jaarverbruik.TWh), " TWh)"),
             paste0(r0(100*metrics$zon.pct.totaal),"%"), 
             paste0("(",r0(metrics$zon.pct.totaal*metrics$jaarverbruik.TWh), " TWh)"),
             "-", "-",
             paste0(r0(metrics$zonnecellen.km2)," km2"), "zonnecellen", paste0("(",r0(100*metrics$opp.bezetting.zon), "% van NL)"),
             "-",
             "-"
    ),
    wind = c(  paste0(r0(100*metrics$totaal.wind.pct.jr),"%"),
               paste0("(",r0(metrics$totaal.wind.pct.jr*metrics$jaarverbruik.TWh), " TWh)"),
               paste0(r0(100*metrics$wind.pct.totaal),"%"),
               paste0("(",r0(metrics$wind.pct.totaal*metrics$jaarverbruik.TWh), " TWh)"),
               "-", "-",
               paste0(r0(metrics$windmolens.km2)," km2"), "windmolens", paste0("(",r0(100*metrics$opp.bezetting.wind), "% van NL)"),
               "-",
               "-"
    ),
    opslag = c( paste0(r1(100*metrics$opslaggrootte.TWh/metrics$jaarverbruik.TWh),"%"),
                paste0("(",r1(metrics$opslaggrootte.TWh), " TWh)"),
                paste0(r0(100*metrics$via.opslag.pct.van.totaal),"%"), 
                paste0("(",r0(metrics$via.opslag.pct.van.totaal*metrics$jaarverbruik.TWh), " TWh)"),
                paste0(r0(100*metrics$surplus.pct.totaal),"%"),
                paste0("(",r0(metrics$surplus.pct.totaal*metrics$jaarverbruik.TWh), " TWh)"), 
                paste0(r0(metrics$avg.max.laadvermogen*(1000*metrics$jaarverbruik.TWh/24/365)/0.1), " water-"), "stoffabrieken", "van 100 MW", 
                paste0(r0(metrics$max.ontlaadvermogen*(1000*metrics$jaarverbruik.TWh/24/365))," GW"),
                paste0(r0(metrics$avg.max.laadvermogen*(1000*metrics$jaarverbruik.TWh/24/365)), " GW")
    ),
    hulpbron = c( paste0(r0(100*metrics$min.cap.hulpbron),"%"), 
                  paste0("(",r0(metrics$min.cap.hulpbron*metrics$jaarverbruik.TWh), " TWh)"),
                  paste0(r0(100*metrics$bijgedraaid.pct.van.totaal),"%"), 
                  paste0("(",r0(metrics$bijgedraaid.pct.van.totaal*metrics$jaarverbruik.TWh), " TWh)"),
                  "-", "-", 
                  paste0(r0(metrics$min.cap.hulpbron*(1000*metrics$jaarverbruik.TWh/24/365)/2.9), " centrales"), "van 2,9 GW", " ",
                  paste0(r0(metrics$min.cap.hulpbron*(1000*metrics$jaarverbruik.TWh/24/365))," GW"),
                  "-"
    )
  )
  #names(tabel) <- c("//color{zon}{//bullet} zon", "wind", "opslag", "hulpbron")
  row.names(tabel) = c("capaciteit", " ", "levering aan gebruikers", "    ", "levering aan opslag","  ", "inventaris",  "   ", "      ",
                       "max geleverd vermogen", "max ontvangen vermogen")
  return(tabel)
}

MaakProductieLijst <- function(results) {
  productie = results[, c(1,11,12,14,13,20,22,9,18,19)]
  productie$cap.hulpbron = (productie$cap.hulpbron/100) * TWh.jr.naar.GW(results$jaarverbruik)  
  productie[, 2:ncol(productie)] = r0(productie[, 2:ncol(productie)])
  for (i in c(2:5,9,10)) productie[, i] =  paste0(productie[, i], " %")
  return(productie)
}

MaakTekortTemporeelLijst <- function(mixen) {
  TekortTemporeel = NULL
  for (m in 1:length(mixen)) {
    mix = mixen[[m]]
    tekort = mix$data$hulpbron
    gem.opbrengst.10min = 1/(365*24*6)
    tekorten.groter.dan.10p = which(tekort/gem.opbrengst.10min > 10)
    if (length(tekorten.groter.dan.10p)>0) {
      cons.periods = diff(tekorten.groter.dan.10p)
      run.len.enc = rle(cons.periods)
      tekorten.langer.dan.een.uur          = length(which(run.len.enc$lengths > 6))
      tekorten.langer.dan.een.halve.dag    = length(which(run.len.enc$lengths > (144/2)))
      langste.tekort.in.uren               = r0(max(run.len.enc$lengths) / 6)
    } else {
      tekorten.langer.dan.een.uur          = 0
      tekorten.langer.dan.een.halve.dag    = 0
      langste.tekort.in.uren               = 0
    }
    tekorten.groter.90p = which(tekort/gem.opbrengst.10min > 90)
    if (length(tekorten.groter.90p)>0) {
      tekorten.groter.90p.vermogen.in.uren = r0(length(tekorten.groter.90p)/6)
    } else {
      tekorten.groter.90p.vermogen.in.uren = 0
    }
    rw = data.frame(naam=mix$metrics$naam, tekorten.langer.dan.een.uur=tekorten.langer.dan.een.uur, tekorten.langer.dan.een.halve.dag=tekorten.langer.dan.een.halve.dag,
                    langste.tekort.in.uren=langste.tekort.in.uren, tekorten.groter.90p.vermogen.in.uren)
    TekortTemporeel = rbind(TekortTemporeel, rw)
    
  }
  return(TekortTemporeel)
}

MaakVerkorteUitkomsten <- function(results, weergave="alles") {
  # Verkorte tabel voor de compacte weergave van niet meer dan een paar scenario's
  #  results = Resultatentabel.Energiemixen[11, ]
  
  results.mem = results
  
  # vanaf col 16 anders: 17 vervalt, 23,24,25 vervalt
  
  # nice format
  results[, c(2:9, 11:12, 14:16, 19:21)] <- r0(results[, c(2:9, 11:12, 14:16, 19:21)])
  results[, c(10,13,17:18)]              <- r1(results[, c(10,13,17:18)])
  results[,21] <- results[,21] * (1-(results[,3]/100))
  colnames(results)[21] <- "mx.ontlaadvermogen.e"
  
  for (i in 1:nrow(results)) results[i, c(3:4,7:9,11:18)] <- paste0(results[i,c(3:4,7:9,11:18)], " %")
  for (i in 1:nrow(results)) results[i, c(5:6)]         <- paste0(results[i,c(5:6)], " % NL")
  for (i in 1:nrow(results)) results[i, c(2,10)]        <- paste0(results[i,c(2,10)], " TWh")
  for (i in 1:nrow(results)) results[i, c(19:21)]       <- paste0(results[i,c(19:21)], " GW")
  for (i in 1:nrow(results)) results[i, 25]             <- gsub("::", " - ", results[i,25])
  
  rijnamen = colnames(results)
  rijnamen = c(" ", "Jaarverbruik","Aan waterstof","Aan elektriciteit","Oppervlakte zon","Oppervlakte wind",              
               "Zon","Wind","Hulpbron","Opslag","Zon","Wind" ,           
               "Hulpbron","Opslag","Onbenut zon & wind","Omzettingsverliezen",
               # el eff, h2 eff
               "Totale draaitijd",          
               "Draaitijd op > 90% verm.","Maximaal","Gemiddelde top 20%","Max. ontladen elektr.",
               # "opslagefficientie","efficientie.El.naar.H2", "efficientie.H2.naar.El",
               "Hulpcentrales (3 GW)","Waterstoffabrieken (100 MW)","Waterstofcentrales (3 GW)","Periode") 
  
  results = rbind(rijnamen, results) 
  
  # selecteer resultaten
  if (weergave=="alles") {
    trans = results[, c(1:4, 5:6, 7:10, 11:18, 19:21, 22:24, 25)]
  } else {
    trans = results[, c(1:4, 5:6, 7:10, 11:18, 19:21, 25)]
  }
  # geef goede namen in de eerste row mee (wordt later 1e kolom), colnames verprutst de namen altijd  
  trans = as.data.frame(t(trans))
  
  return(trans)
}

# WIND EN ZON ZIJN GEIJKT OP 50% CAPACITEIT IN DE BESCHOUWDE PERIODE
# 2001-2019:
zonkm2.0119  = 1500 / 0.4650777 * 0.5
windkm2.0119 = 22400 / 0.5104667 * 0.5
# 2019:
zonkm2    = 1559   
windkm2   = 21530  
# 2015::2019:
zonkm2.1519    = 1559/(2*0.494866)
windkm2.1519   = 21530/(2*0.489526)

# ----- Energiemixen ter test ----
energiemixenbuffer = list(
  # twee 'gelijke scenario's' ter vergelijk
  zw5050he0100   = Bereken.Energiemix.Model2(naam="zw5050he0100",     jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=0, zonnecellen.km2=zonkm2, rendement.zonnecellen=0.2, windmolens.km2=windkm2, separatieafstand=7, veldefficientie=0.6, windtype="Enercon-126", opslaggrootte.TWh=10, efficientie.H2.naar.El=0.5,efficientie.El.naar.H2=0.8,startmetvolleaccu=FALSE, Periode="2019"),
  zw5050e100_oud = Bereken.Energiemix.Praktijk(naam="zw5050e100_oud", jaarverbruik=Energieverbruik.NL.kWh,                      zonnecellen.km2=zonkm2, rendement.zonnecellen=0.2, windmolens.km2=windkm2, separatieafstand=7, veldefficientie=0.6, windtype="Enercon-126", opslaggrootte.TWh=10, opslagefficientie=0.4,                                startmetvolleaccu=FALSE, Periode="2019"),

  # scenario's nieuw model (met waterstofverbruik)
  geenopslag   = Bereken.Energiemix.Model2(naam="zon wind, geen opslag", jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=0,        zonnecellen.km2=zonkm2, rendement.zonnecellen=0.2, windmolens.km2=windkm2,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",opslaggrootte.TWh=0,efficientie.H2.naar.El=0.5,efficientie.El.naar.H2=0.8,startmetvolleaccu=FALSE, Periode="2019"),
  overcap      = Bereken.Energiemix.Model2(naam="overcapaciteit zon wind, geen opslag", jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=0, zonnecellen.km2=2*zonkm2, rendement.zonnecellen=0.2, windmolens.km2=2*windkm2,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",opslaggrootte.TWh=0,efficientie.H2.naar.El=0.5,efficientie.El.naar.H2=0.8,startmetvolleaccu=FALSE, Periode="2019"),
  kernenergie  = Bereken.Energiemix.Model2(naam="kernenergie",           jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=0,        zonnecellen.km2=0, rendement.zonnecellen=0.2, windmolens.km2=0,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",opslaggrootte.TWh=0,efficientie.H2.naar.El=0.5,efficientie.El.naar.H2=0.8,startmetvolleaccu=FALSE, Periode="2019"),
  teslas       = Bereken.Energiemix.Model2(naam="zon wind en elektrisch wagenpark",jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=0,zonnecellen.km2=zonkm2, rendement.zonnecellen=0.2, windmolens.km2=windkm2,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",opslaggrootte.TWh=0.7,efficientie.H2.naar.El=0.9,efficientie.El.naar.H2=1,startmetvolleaccu=FALSE, Periode="2019"),
  teslas2x     = Bereken.Energiemix.Model2(naam="zon wind en 2x wagenpark",jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=0,        zonnecellen.km2=zonkm2, rendement.zonnecellen=0.2, windmolens.km2=windkm2,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",opslaggrootte.TWh=1.4,efficientie.H2.naar.El=0.9,efficientie.El.naar.H2=1,startmetvolleaccu=FALSE, Periode="2019"),
  overkill     = Bereken.Energiemix.Model2(naam="2x zon en wind, 2x wagenpark",jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=0,    zonnecellen.km2=zonkm2*2, rendement.zonnecellen=0.2, windmolens.km2=windkm2*2,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",opslaggrootte.TWh=1.4,efficientie.H2.naar.El=0.9,efficientie.El.naar.H2=1,startmetvolleaccu=FALSE, Periode="2019"),
  besparen     = Bereken.Energiemix.Model2(naam="Besparen",              jaarverbruik=Energieverbruik.NL.kWh*0.5, waterstofverbruik=0.2,  zonnecellen.km2=zonkm2, rendement.zonnecellen=0.2, windmolens.km2=windkm2,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",opslaggrootte.TWh=0.7,efficientie.H2.naar.El=0.9,efficientie.El.naar.H2=1,startmetvolleaccu=FALSE, Periode="2019"),
  besparen_oud = Bereken.Energiemix.Praktijk(naam="Besparen oud",        jaarverbruik=Energieverbruik.NL.kWh*0.5,                         zonnecellen.km2=zonkm2, rendement.zonnecellen=0.2, windmolens.km2=windkm2,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",opslaggrootte.TWh=0.7,opslagefficientie=0.9,                              startmetvolleaccu=FALSE, Periode="2019"),
  besparen.ov  = Bereken.Energiemix.Model2(naam="Besparen overcapa.",    jaarverbruik=Energieverbruik.NL.kWh*0.5, waterstofverbruik=0.2,  zonnecellen.km2=4*zonkm2, rendement.zonnecellen=0.2, windmolens.km2=2*windkm2,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",opslaggrootte.TWh=0.7,efficientie.H2.naar.El=0.9,efficientie.El.naar.H2=1,startmetvolleaccu=FALSE, Periode="2019"),
  besparen.lang    = Bereken.Energiemix.Model2(naam="Besparen lang", jaarverbruik=Energieverbruik.NL.kWh*0.5, waterstofverbruik=0.2, zonnecellen.km2=zonkm2.0119, rendement.zonnecellen=0.2, windmolens.km2=windkm2.0119,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",opslaggrootte.TWh=0.7,efficientie.H2.naar.El=0.9,efficientie.El.naar.H2=1,startmetvolleaccu=FALSE, Periode="2001::2019"),
  besparen.ov.lang = Bereken.Energiemix.Model2(naam="Besparen overc. lang", jaarverbruik=Energieverbruik.NL.kWh*0.5, waterstofverbruik=0.2, zonnecellen.km2=4*zonkm2.0119, rendement.zonnecellen=0.2, windmolens.km2=2*windkm2.0119,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",opslaggrootte.TWh=0.7,efficientie.H2.naar.El=0.9,efficientie.El.naar.H2=1,startmetvolleaccu=FALSE, Periode="2001::2019"),
  
  # zon.mincap: probeer het zonder hulpbron door een zo klein mogelijke h2-opslag te gebruiken.
  # Kieseen opslag van 20 TWh, klein wellicht voor h2, je 35000 km2 zonnecellen moet neerleggen. Neem CBS kaart NL indeling
  zon.mincap   = Bereken.Energiemix.Model2(naam="zon, min. opslag",          jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=0.2,  zonnecellen.km2=35000, rendement.zonnecellen=0.2, windmolens.km2=0,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",opslaggrootte.TWh=20,efficientie.H2.naar.El=0.5,efficientie.El.naar.H2=0.8,startmetvolleaccu=FALSE, Periode="2015::2019"),
  # zon minopp: probeer het zonder hulpbron door opslaggrootte maximaal te maken. 
  # Kies 350 TWh, dat is de helft van het NL verbruik. Fors. Dan nog 6000 km2 nodig. 
  zon.minopp   = Bereken.Energiemix.Model2(naam="zon, min. oppervlak",       jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=0.2,  zonnecellen.km2=6000, rendement.zonnecellen=0.2, windmolens.km2=0,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",opslaggrootte.TWh=350,efficientie.H2.naar.El=0.5,efficientie.El.naar.H2=0.8,startmetvolleaccu=FALSE, Periode="2015::2019"),
  # wind mincap n minopp als bij zon. Gebruik dit voor de vraag: is windeenergie op land zinvol?
  wind.mincap  = Bereken.Energiemix.Model2(naam="wind min. opslag",          jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=0.2,  zonnecellen.km2=0, rendement.zonnecellen=0.2, windmolens.km2=1.8*2*windkm2.1519,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",opslaggrootte.TWh=20,efficientie.H2.naar.El=0.5,efficientie.El.naar.H2=0.8,startmetvolleaccu=FALSE, Periode="2015::2019"),
  wind.minopp  = Bereken.Energiemix.Model2(naam="wind min. oppervlak",       jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=0.2,  zonnecellen.km2=0, rendement.zonnecellen=0.2, windmolens.km2=1.5*2*windkm2.1519,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",opslaggrootte.TWh=350,efficientie.H2.naar.El=0.5,efficientie.El.naar.H2=0.8,startmetvolleaccu=FALSE, Periode="2015::2019"),

  # pure waterstofmaatschappij versus volledig ge-elektrificeerde maatschappij (MINDER DUIDELIJK)
  elektr.accu         = Bereken.Energiemix.Model2(naam="El accu", jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=0, zonnecellen.km2=zonkm2.1519, rendement.zonnecellen=0.2, windmolens.km2=windkm2.1519,opslaggrootte.TWh=0.7,efficientie.H2.naar.El=0.9,efficientie.El.naar.H2=1,startmetvolleaccu=FALSE, Periode="2015::2019"),
  elektr.h2opslag     = Bereken.Energiemix.Model2(naam="El H2",   jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=0, zonnecellen.km2=zonkm2.1519, rendement.zonnecellen=0.2, windmolens.km2=windkm2.1519,opslaggrootte.TWh=50,efficientie.H2.naar.El=0.5,efficientie.El.naar.H2=0.8,startmetvolleaccu=FALSE, Periode="2015::2019"),
  waterm.h2opslag     = Bereken.Energiemix.Model2(naam="H2 H2",   jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=1, zonnecellen.km2=zonkm2.1519, rendement.zonnecellen=0.2, windmolens.km2=windkm2.1519,opslaggrootte.TWh=50,efficientie.H2.naar.El=0.5,efficientie.El.naar.H2=0.8,startmetvolleaccu=FALSE, Periode="2015::2019"),
  waterm.h2opslag.O.B = Bereken.Energiemix.Model2(naam="H2 H2 OB",jaarverbruik=Energieverbruik.NL.kWh*0.75, waterstofverbruik=1, zonnecellen.km2=1.5*zonkm2.1519, rendement.zonnecellen=0.2, windmolens.km2=windkm2.1519,opslaggrootte.TWh=50,efficientie.H2.naar.El=0.5,efficientie.El.naar.H2=0.8,startmetvolleaccu=FALSE, Periode="2015::2019"),

  # 'Kunnen we toe met 15% kernenergie, oftewel met 7 van de 29 centrales?'
  # Variant met 50% NL wind en 8% NL zon en 150 TWh opslag heeft GEEN kernenergie nodig. Nadeel: 50% van NL vol met windturbines. Kan dat minder?
  minkern1 = Bereken.Energiemix.Model2(naam="min. kernenergie 1", jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=0.5, zonnecellen.km2=2*zonkm2.1519, rendement.zonnecellen=0.2, windmolens.km2=windkm2.1519,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",opslaggrootte.TWh=150, efficientie.H2.naar.El=0.5,efficientie.El.naar.H2=0.8,startmetvolleaccu=TRUE, Periode="2015::2019"),
  # Doe meer zon, 25% NL aan wind, en meer opslag, en dan lukt het ook. Nadeel nu is de max. laadstroom: die is erg hoog. 
  # Subconclusie: meer zon installeren helpt enorm i.c.m. hoge opslagcapaciteit.  
  minkern2 = Bereken.Energiemix.Model2(naam="min. kernenergie 2", jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=0.5, zonnecellen.km2=2.1*zonkm2.1519, rendement.zonnecellen=0.2, windmolens.km2=0.5*windkm2.1519,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",opslaggrootte.TWh=250, efficientie.H2.naar.El=0.5,efficientie.El.naar.H2=0.8,startmetvolleaccu=TRUE, Periode="2015::2019"),
  # Maar als ik nu 15% kernenergie wil? -> Dat kan niet, het is altijd 100%, of niets. Tussen de 7,6% en 7,9% NL zon zit een faseovergang van geen 
  # kerncentrales naar het volle aantal. 
  minkern3 = Bereken.Energiemix.Model2(naam="min. kernenergie 3", jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=0.5,  zonnecellen.km2=2.02*zonkm2.1519, rendement.zonnecellen=0.2, windmolens.km2=0.5*windkm2.1519,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",opslaggrootte.TWh=250, efficientie.H2.naar.El=0.5,efficientie.El.naar.H2=0.8,startmetvolleaccu=FALSE, Periode="2015::2019"),
  
  # pure waterstofmaatschappij versus volledig ge-elektrificeerde maatschappij
  # GROOT verschil hier tussen beide limietgevallen!!!
  # OVER NADENKEN... moet dit mee? pro?
  waterelek1 = Bereken.Energiemix.Model2(naam="waterelek1",jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=0.5,  zonnecellen.km2=2.02*zonkm2.1519, rendement.zonnecellen=0.2, windmolens.km2=0.5*windkm2.1519,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",opslaggrootte.TWh=250, efficientie.H2.naar.El=0.5,efficientie.El.naar.H2=0.8,startmetvolleaccu=TRUE, Periode="2015::2019"),
  waterelek2 = Bereken.Energiemix.Model2(naam="waterelek2",jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=1,  zonnecellen.km2=2.02*zonkm2.1519, rendement.zonnecellen=0.2, windmolens.km2=0.5*windkm2.1519,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",opslaggrootte.TWh=250,efficientie.H2.naar.El=0.5,efficientie.El.naar.H2=0.8,startmetvolleaccu=TRUE, Periode="2015::2019"),
  waterelek3 = Bereken.Energiemix.Model2(naam="waterelek3", jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=0,  zonnecellen.km2=2.02*zonkm2.1519, rendement.zonnecellen=0.2, windmolens.km2=0.5*windkm2.1519,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",opslaggrootte.TWh=250, efficientie.H2.naar.El=0.5,efficientie.El.naar.H2=0.8,startmetvolleaccu=TRUE, Periode="2015::2019")
)

# ----- Energiemixen worden gebruikt in de tekst ----
energiemixen = list(
  # Zon en wind zonder opslag
  geenopslag   = Bereken.Energiemix.Model3(naam="S1. zon wind, geen opslag", jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=0,        zonnecellen.km2=zonkm2, rendement.zonnecellen=0.2, windmolens.km2=windkm2,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",opslaggrootte.TWh=0,startmetvolleaccu=FALSE, Periode="2019"),
  overcap      = Bereken.Energiemix.Model3(naam="S2. overcapaciteit zon wind, geen opslag", jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=0, zonnecellen.km2=2*zonkm2, rendement.zonnecellen=0.2, windmolens.km2=2*windkm2,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",opslaggrootte.TWh=0,startmetvolleaccu=FALSE, Periode="2019"),
  
  # Elektrisch wagenpark als energiebufffer  
  teslas       = Bereken.Energiemix.Model3(naam="S3. zon wind en elektrisch wagenpark",jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=0,zonnecellen.km2=zonkm2, rendement.zonnecellen=0.2, windmolens.km2=windkm2,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",opslaggrootte.TWh=0.7, eff.O.W=0.8,eff.O.E=0.9, startmetvolleaccu=FALSE, Periode="2019"),
  teslas2x     = Bereken.Energiemix.Model3(naam="S4. zon wind en 2x wagenpark",jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=0,        zonnecellen.km2=zonkm2, rendement.zonnecellen=0.2, windmolens.km2=windkm2,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",opslaggrootte.TWh=1.4, eff.O.W=0.8,eff.O.E=0.9, startmetvolleaccu=FALSE, Periode="2019"),
  overkill     = Bereken.Energiemix.Model3(naam="S5. 2x zon en wind, wagenpark",jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=0,    zonnecellen.km2=zonkm2*2, rendement.zonnecellen=0.2, windmolens.km2=windkm2*2,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",opslaggrootte.TWh=0.7, eff.O.W=0.8,eff.O.E=0.9, startmetvolleaccu=FALSE, Periode="2019"),
  
  # Hoe effectief is windenergie op land?
  effwind      = Bereken.Energiemix.Model3(naam="S6. Alleen windenergie", jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=0, zonnecellen.km2=0, rendement.zonnecellen=0.2, windmolens.km2=2*windkm2,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",opslaggrootte.TWh=0, startmetvolleaccu=FALSE, Periode="2019"),
  effzon       = Bereken.Energiemix.Model3(naam="S7. Alleen zonne-energie", jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=0,zonnecellen.km2=2*zonkm2, rendement.zonnecellen=0.2, windmolens.km2=0,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",opslaggrootte.TWh=0, startmetvolleaccu=FALSE, Periode="2019"), 
  # effcombi     = Bereken.Energiemix.Model3(naam="S8. Combinatie wind en zon", jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=0, zonnecellen.km2=zonkm2, rendement.zonnecellen=0.2, windmolens.km2=windkm2,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",opslaggrootte.TWh=0, startmetvolleaccu=FALSE, Periode="2019"),
  
  # Kan schone energie zonder kernenergie?
  h2opslag     = Bereken.Energiemix.Model3(naam="S8. Zon, wind en waterstof", jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=0.25, zonnecellen.km2=zonkm2, rendement.zonnecellen=0.2, windmolens.km2=windkm2,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",opslaggrootte.TWh=50, startmetvolleaccu=FALSE, Periode="2019"),
  h2opslag2    = Bereken.Energiemix.Model3(naam="S9. Veel zon en wind met waterstof", jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=0.25, zonnecellen.km2=2*zonkm2, rendement.zonnecellen=0.2, windmolens.km2=windkm2,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",opslaggrootte.TWh=150, startmetvolleaccu=TRUE, Periode="2019"),
  h2opslag2l   = Bereken.Energiemix.Model3(naam="S10. Waterstof lange termijn", jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=0.25, zonnecellen.km2=2*zonkm2.0119, rendement.zonnecellen=0.2, windmolens.km2=windkm2.0119,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",opslaggrootte.TWh=150, startmetvolleaccu=TRUE, Periode="2001::2019"),
  
  # Kernenergie
  kernenergie  = Bereken.Energiemix.Model3(naam="S11. kernenergie", jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=0.25, zonnecellen.km2=0, rendement.zonnecellen=0.2, windmolens.km2=0, separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",opslaggrootte.TWh=0,startmetvolleaccu=FALSE, Periode="2019"),
  
  # Wind, zon en kernenergie
  # Kan beperkte inzet van kernenergie de flucuaties van zon en wind opvangen?
  # Dit scenario staat eigenlijk gelijk aan een klein beetje energie uit het buitenland halen in de zin dat het economisch een nono is.
  beperktkern1 = Bereken.Energiemix.Model3(naam="S12. Basisscenario beperkt kernenergie", jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=0.25, 
                                           zonnecellen.km2=zonkm2.1519, rendement.zonnecellen=0.2,windmolens.km2=windkm2.1519, separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",
                                           opslaggrootte.TWh=0, startmetvolleaccu=TRUE, Periode="2015::2019"),
  beperktkern2 = Bereken.Energiemix.Model3(naam="S13. Extra wind en zon", jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=0.25, 
                                           zonnecellen.km2=0.25*Oppervlakte.Nederland.km2, rendement.zonnecellen=0.2, windmolens.km2=(0.4*1.35*2 + 0.5) * Oppervlakte.Nederland.km2, separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",
                                           opslaggrootte.TWh=0, startmetvolleaccu=TRUE, Periode="2015::2019"),
  beperktkern3 = Bereken.Energiemix.Model3(naam="S14. Extra wind, zon en beperkte opslag", jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=0.25, 
                                           zonnecellen.km2=0.25*Oppervlakte.Nederland.km2, rendement.zonnecellen=0.2, windmolens.km2=(0.4*1.35*2 + 0.5) * Oppervlakte.Nederland.km2, separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",
                                           opslaggrootte.TWh=0.7, eff.O.W=0.8,eff.O.E=0.9, startmetvolleaccu=TRUE, Periode="2015::2019"),
  
  # Alleen zon?
  zon          = Bereken.Energiemix.Model3(naam="S15. Alleen zon", jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=0.25,  zonnecellen.km2=2*zonkm2.1519, rendement.zonnecellen=0.2, windmolens.km2=0,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",opslaggrootte.TWh=0,startmetvolleaccu=FALSE, Periode="2015::2019"),
  
  # zon minopp: probeer het zonder hulpbron door opslaggrootte maximaal te maken. 
  # Kies 350 TWh, dat is de helft van het NL verbruik. Fors. Dan nog 6000 km2 nodig. 
  zon.minopp   = Bereken.Energiemix.Model3(naam="S16. Zon, ruimte geminimaliseerd", jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=0.25,  
                                           zonnecellen.km2=6000, rendement.zonnecellen=0.2, windmolens.km2=0,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",
                                           opslaggrootte.TWh=350,startmetvolleaccu=TRUE, Periode="2015::2019"),
  # zon.mincap: probeer het zonder hulpbron door een zo klein mogelijke h2-opslag te gebruiken.
  # Kieseen opslag van 20 TWh, klein wellicht voor h2, je 35000 km2 zonnecellen moet neerleggen. Neem CBS kaart NL indeling
  zon.mincap   = Bereken.Energiemix.Model3(naam="S17. Zon, opslag geminimaliseerd", jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=0.25,  
                                           zonnecellen.km2=Oppervlakte.Nederland.km2*0.85, rendement.zonnecellen=0.2, windmolens.km2=0,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",
                                           opslaggrootte.TWh=30,startmetvolleaccu=TRUE, Periode="2015::2019"),
  # Besparen
  besparen.opslag = Bereken.Energiemix.Model3(naam="S18. Besparen: minder opslag", jaarverbruik=0.5*Energieverbruik.NL.kWh, waterstofverbruik=0.25, 
                                              zonnecellen.km2=2*zonkm2.0119, rendement.zonnecellen=0.2, 
                                              windmolens.km2 =windkm2.0119,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",
                                              opslaggrootte.TWh=15, startmetvolleaccu=TRUE, Periode="2001::2019"),
  besparen.opper = Bereken.Energiemix.Model3(naam="S19. Besparen: minder ruimtegebruik", jaarverbruik=0.5*Energieverbruik.NL.kWh, waterstofverbruik=0.25, 
                                             zonnecellen.km2=zonkm2.0119, rendement.zonnecellen=0.2, 
                                             windmolens.km2 =0.4*windkm2.0119,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",
                                             opslaggrootte.TWh=150, startmetvolleaccu=TRUE, Periode="2001::2019"),
  besparen.25p   = Bereken.Energiemix.Model3(naam="S20. Besparen: 25% minder", jaarverbruik=0.75*Energieverbruik.NL.kWh, waterstofverbruik=0.25, 
                                             zonnecellen.km2=2*zonkm2.0119, rendement.zonnecellen=0.2, 
                                             windmolens.km2 =windkm2.0119,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",
                                             opslaggrootte.TWh=60, startmetvolleaccu=TRUE, Periode="2001::2019")
)

# --- Resultatentabel wordt gebruikt in de tekst
Resultatentabel.Energiemixen = MaakResultatenTabelModel3(energiemixen)
Resultatentabel.Energiemixen[, 2:(ncol(Resultatentabel.Energiemixen)-3)] <- round(Resultatentabel.Energiemixen[, 2:(ncol(Resultatentabel.Energiemixen)-3)], 3)
fwrite(Resultatentabel.Energiemixen, "Resultatentabel_Energiemixen.csv")

# besparen.25p   = Bereken.Energiemix.Model2(naam="S21. Besparen: 25% minder", jaarverbruik=0.75*Energieverbruik.NL.kWh, waterstofverbruik=0.25, 
#                                            zonnecellen.km2=2*zonkm2.0119, rendement.zonnecellen=0.2, 
#                                            windmolens.km2 =windkm2.0119,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",
#                                            opslaggrootte.TWh=60, efficientie.H2.naar.El=0.5,efficientie.El.naar.H2=0.8,startmetvolleaccu=TRUE, Periode="2001::2019")
# Energiemix.Visualisatie3.TWh(besparen.25p, visualisatie.tijdschaal = "daily", figuurtitel="Scenario", figuursubtitel="")
# b = MaakVerkorteUitkomsten(MaakResultatenTabelModel2(list(besparen.25p)))

# results = Resultatentabel.Energiemixen[2:3, ]
# b = MaakVerkorteUitkomsten(Resultatentabel.Energiemixen[3,])

# MaakInventarisLijst(Resultatentabel.Energiemixen)
# MaakTekortTemporeelLijst(energiemixen)


# ----- Bereken de lengte van tekorten, gebaseerd op diepte -----

if (FALSE) {
  minkern1 = Bereken.Energiemix.Model2(naam="min. kernenergie 1", jaarverbruik=Energieverbruik.NL.kWh, waterstofverbruik=0.5, 
                                       zonnecellen.km2=zonkm2.1519, rendement.zonnecellen=0.2, 
                                       windmolens.km2=windkm2.1519,separatieafstand=7,veldefficientie=0.6,windtype="Enercon-126",opslaggrootte.TWh=0, efficientie.H2.naar.El=0.5,efficientie.El.naar.H2=0.8,startmetvolleaccu=TRUE, Periode="2019:01")
  
  # Wat is 'veel bijdraaien op veel vermogen'? Een minimum percentage van vol vermogen is niet helemaal
  # bevredigend, want stel dat er 10 bars achter elkaar met veel vermogen (zeg minimaal op 80%) wordt
  # bijgedraaid, en er zit 1 bar tussen waar er op 60% wordt bijgedraaid, deel je dan de periode in
  # tweeën, of negeer je de tijdelijke 60% en tel je ze alle 10? Het laatste lijkt de lading meer te dekken.
  # Probeer het volgende algoritme: een periode 'bijdraaien op hoog vermogen' met lengte n is 
  # in minstens 0.8n van de gevallen boven de 80% vermogen, en heeft niet meer dan 0.1n opeenvolgende bars
  # waar het vermogen minder is dan 80%

  fors.bijdraaien.lengte = c()
  counter = 0; counting = FALSE; threshold = .8; teweiniglen = 0
  for (i in 1:nrow(energiemixen$beperktkern3$data$hulpbron)) {
    perc = energiemixen$beperktkern3$data$hulpbron[i]*energiemixen$beperktkern3$metrics$perio.per.jaar/100
    if (perc >= .8 & !counting) { counter=1; counting = TRUE; teweiniglen=0} 
    if (perc >= .8 & counting)  { counter=counter+1; teweiniglen=0} 
    if (perc < .8  & counting)  { 
      if (teweiniglen/counter > 0.1) { 
        fors.bijdraaien.lengte = c(fors.bijdraaien.lengte, counter); 
        counting=FALSE; counter=0
      } else {teweiniglen = teweiniglen + 1}
    }
  }
  hist(fors.bijdraaien.lengte[fors.bijdraaien.lengte>5]/6)
  
}

