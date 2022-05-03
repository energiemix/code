# Code 'De energietransitie doorgerekend'

De repository 'code' bevat alle code en tekst om het 'boek' te genereren. Het bevat alle berekeningen, inclusief de data, die in de tekst worden gedaan. Alle grafieken en tabellen zijn hiermee te reproduceren en dus te controleren. 

## Opzet in Github

Er zijn drie verschillende repositories op github.com/energiemix:
1. code: alle berekeningen ter controle (deze repository)
2. energiemix.github.io: de online tekstversie, inclusief PDF-download
3. tekst: de laatste versie van de tekst in PDF-vorm

Maak bijv. een hoofdmap 'energiemix' en kloon de repositories daar onder

## Structurering code

De code is geschreven in R (via RStudio) met optimalsaties in C++. De gebruikte data komt vooramelijk van het KNMI en het CBS en is bijgesloten. Github staat bestanden groter dan 100 MB niet toe; de winddata van de KNMI is daarom in tweeën geknipt.

Kort stappenplan om het boek te genereren uit code:
1. kloon 'code' naar een lokale schrijf
2. Installeer R en Rstudio
3. Open het project in R studio
4. In RStudio, open bestand 'Index.Rmd', daar staan hints voor compilatie
5a. Om de PDF te genereren, tik 'bookdown::render_book("index.Rmd", "bookdown::pdf_book")' in de console. 
    De PDF verschijnt in de map 'docs'.
5b. Om de online versie te genereren, tik 'bookdown::render_book()' in de console. De HTML verschijnt in 'energiemix.github.io'.    

De code voor het rekenwerk staat zoveel mogelijk in 'Energietransitie8.R'. Voor het napluizen en controleren van de code en het genereren van grafieken is het afdoende om alleen dit bestand te openen. Vervolgens zul je echt de code inmoeten. Op dit moment is deze nog niet maximaal becommentarieert, maar daar staat tegenover dat R goed leesbaar is.

Wat suggesties. Begin onderaan het 'Energietransitie8.R' bij de code die een grafiek produceert. Doe vervolgens een drill-down van deze code om te zien wat er allemaal wordt aangeroepen.

Voor de scenario's, zoek op 'energiemixenbuffer'.
De belangrijkste routine is waarschijnlijk 'Bereken.Energiemix.Model3()'. een belangrijk onderdeel daarin is de optimalisatie in C, te vinden in 'sourceCpp("productiecijfers3C.cpp")'. 

Het berekenen van de energieopbrengst van windmolenparken en zonneparken is redelijk eenvoudig, en is te vinden bovenaan de code. Zo'n beetje alles gebeurt in sectie '# --- IMPORTEREN KNMI DATA OP 10 MIN RESOLUTIE ---------------------------------'. Dit zou het eerste zijn om te controleren, want alles is gebaseerd op deze opbrengstberekeningen. Je hebt daarna de opbrengsten per 10 minuten voor een vierkante kilometer molens en zonnecellen in midden-Nederland, dat als gemiddelde wordt genomen.

Vervolgens zou het goed zijn om te kijken of al die verschillende tijdsvakken van 10 minuten  wel goed worden geaggregeerd in alle grafieken en tabellen. Dat gebeurt in 'Bereken.Energiemix.Model3()', onder '# ---- STEL DE PRODUCTIECIJFERS SAMEN -----------------------------------------------------'. Voor zover de code complex is, is het in dit stuk. Er worden hier behoorlijk wat ballen tegelijk in de lucht gehouden. De crux is dat er voor een  aantal eenheden (belangrijkste: de stand van de 'accu' of energiebuffer die wordt gebruikt om tekorten op te vangen) een 'rolling' berekening wordt gedaan, die alleen met een loop is op te lossen. Dat duurt in R eeuwig. Wat niet in R geoptimaliseerd kon worden, in verplaatst  naar C++ (bestand 'productiecijfers3C.cpp'). De code is niet moeilijk, maar, zoals al gezegd, er hangen veel ballen in de lucht.

Mijn suggestie voor een aanpak om dit te controleren zou als volgt zijn. Voer de totale code uit door op 'source' te drukken in RStudio. Installeer eventuele packages die ontbreken. Zorg dat de code compileert. R is een interpreter, je kunt door de code heen 'steppen'. Alle variabelen zijn per stap te inspecteren. Dit is bijv. te doen voor de code in sectie '# ---- STEL DE PRODUCTIECIJFERS SAMEN -----------------------------------------------------', op de optimalisatie in C++ na, en die is wel erg belangrijk. Hier is wellicht wat meer doorzettingsvermogen vereist.

## Tot slot

Voor diegene die gaat proberen de code te controleren (wellicht omdat hij/zij, net als ik, op zoek is naar een betrouwbare berekening om conclusies op te kunnen baseren), allereerst dank! Ik houd me voor suggesties en verbeteringen aanbevolen. Tevens kunnen er natuurlijk vragen over de code gesteld worden. Meel me op 'mR1 apenstaartje optiGRRmaas punt coAm', waarbij je alle hoofdletters verwijderd. 

Pascal Maas
mei 2022


 