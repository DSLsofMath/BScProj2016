# Halvtidsredovisning


## Att tänka på

** Varje byte mellan person måste nämnas med namn: “Nu ska Joakim prata om…”

Innehållet i presentationen skall fokusera på  projektets mål, projektplaneringen samt nuvarande status för projektet och som avslutning lite om egna erfarenheter.

### Projektets mål:
Att använda programmering för att underlätta förståelsen av matematiska texter som behövs till TSS och Reglerteknik tillsammans med alla de begrepp som studenterna kämpar med inom kurserna.
En bild som visar hur många som klarar kursen/blir underkända.

### Projektplanering:
Vi la upp en fin plan relativt snabbt. Examinatorer skulle intervjuas för att vi skulle få bättre koll på vad de tyckte studenterna hade svårt att förstå. Studenterna skall även intervjuas och få testa vår produkt om de vill.
Presentera de olika delmomenten?

### Status för projektet:
Våra texter håller en lätt ton för att undvika känslan av att man läser en torr, tråkig kursbok.
En kombination av texter och övningar i att programmera.
Visa upp något om vi har blivit klara?

### Egna erfarenheter:
Att hjälpa andra förstå en komplicerad matematisk text innebär att vi själva först måste förstå texten.

## Presentationen

### Manus:

J: Hej, Mitt namn är Joakim.

F: Och mitt namn är Filip

J: Vi ska prata om DSLsofMath, vilket står för “Domain Specific Languages of Mathematics” eller “Matematikens domänspecifika språk. Detta blev ett projekt då det under många år har varit alldeles för många på “Datateknik” som har blivit underkända på kurserna Transformer, Signaler och System även känt som TSS, samt Reglerteknik. Mellan 2010 och 2016 så är det endast 51 % som har blivit godkända i TSS respektive 53 % i Reglerteknik. Det här ser ju inte direkt bra ut, men när vi kollade upp mer om dessa resultaten så finns även det ett mörkertal med dessa siffror eftersom vid till exempel Reglertekniks ordinarie tentamen 2014 så var det 48 utav de 122 registrerade studenterna, vilket innebär 39 % som inte skrev tentan över huvud taget och därför inte är medräknade. I helhet var det därför endast 31 % av de som var registrerade på kursen som blev godkända på den tentan. (Dela upp publiken)

Det är där vårt projekt kommer in! När vi fick projektet så fick vi stort svängrum om hur vi ville lösa det här problemet. Vi kommer att skriva en tutorial, alltså en slags guide, som man kan arbeta med ifall man tänker läsa kurserna, eller har läst dem förut men ska ge sig på en omtenta eller något åt det hållet. Tanken med tutorialen är även att den är skriven på ett lättsamt sätt för att göra det lättare att ta åt sig informationen och komma ihåg den bättre, för en datateknolog. Det kommer även att finnas uppgifter som man kommer ska lösa genom att programmera, vilket datastudenter oftast känner sig mer bekväma med. En inspiration som vi har haft när vi utarbetat planen för arbetet är Learnyouahaskell, en hemsida (och bok) med underhållande men även lärorika texter som datateknologer brukar använda sig av för att lära sig Haskell när de börjar på Chalmers. 

Prata lite kort om learnyouahaskell exemplet.

För att ta reda på var de stora svårigheterna med att ta till sig TSSen och Reglertekniken ligger, så har vi utfört intervjuer med examinatorer från de båda kurserna för att få veta vad de tror är det svåra för datateknologerna. Vi har även utformat och skickat ut en enkät till Data-tvåan (registrerade 2014), samt de som registrerades på Data år 2013 och år 2012 för att få veta hur de bedömde sina förkunskaper inför de här kurserna och ifall de hade läst kursen kunde de även svara på hur de upplevde att de behärskade olika områden och delmoment från kurserna på så vis kunde vi se var studenterna själva kände att de var oförberedda eller hade svårigheter med inlärningen. 

Tutorialen är tänkt att bli en PDF som först går igenom delmomenten, som exempelvis Fourierrepresentationer, Fouriertransform och mycket annat man träffar på under kursernas gång och sedan avslutas varje kapitel med några uppgifter som ska hjälpa till att få folk att känna sig mer bekväma med att arbeta med ämnet, men det ska samtidigt inte vara för många så att man känner sig överväldigad och inte gör dom över huvud taget. En utmaning blir att hålla det intressant och lättförståeligt så att man faktiskt vill sitta och arbeta med det samtidigt som man känner att man lär sig utav det. Tanken är ju, som vi nämnde tidigare, att det ska vara ett komplement till de andra kurserna, inte ersätta dom helt.

J: Nu ska Filip prata mer om projektet i sig. 

F: Tack, Joakim.

Vårt projekt har uppstått som en följd av  det pedagogiska projektet på Chalmers, DSLsofMath vilket är ett projekt som Patrik Jansson, en tidigare Programansvarig på datateknik, satte igång. Det resulterade även i kursen DSLsofMath som tre av medlemmarna i vår kandidatgrupp läser vid sidan av kandidaten just nu, delvis så de får en chans att lära sig hur man utvecklar ett DSL men även så man kan hålla koll på vad som utvecklas i den så vi inte utvecklar samma sak som de gör i kursen. Vårt projekt ska fungera som ett komplement till både den här kursen och de andra två, så man vill ju undvika att utveckla saker de redan utvecklat. Den kursen har som mål att lära ut hur man skriver DSLs inom lite mer grundläggande områden än vad vi ska göra med vårt projekt. Eftersom det är första gången kursen går så har vi även chans att forma hur den ser ut genom att ge feedback under kursens gång. 

Vi har ju bara kort nämnt att vårt projekt heter Domain specific languages of mathematics så för er som inte vet vad Domain Specific Languages, eller DSLs, så är det ett programmeringsspråk som är specialbyggt för att lösa ett problem eller lämpa sig väldigt väl för en viss typ av problem, snarare än ett generellt programmeringsspråk som Java eller C som man kan använda för vilka problem som helst.. Man kan skriva DSLs på två olika sätt. nämligen som helt egna språk eller genom att bädda in dem i ett redan existerande språk. Vi har valt det sistnämnda och alltså byggt vidare på ett fullt fungerande programmeringsspråk för att göra det ännu bättre på vissa speciella områden.

Programmeringspråket det är tänkt man ska arbeta i när man gör uppgifterna från tutorialen är Haskell. Detta för att det är smidigt att arbeta med då haskell är ett funktionellt programmeringspråk som ligger nära matematiken till att börja med, men även för att datastudenter har arbetat med det tidigare och förhoppningsvis känner igen sig så man slipper att lära sig även språket utöver matematiken.

Lite mer konkret vad vi har lyckats göra med vårt projekt hittills, så har vi gjort det mesta av det förarbete som krävs för att vi ordentligt ska kunna komma igång och skriva vår tutorial. Vi har läst på inom TSSen och Reglertekniken, vi har studerat olika sätt att komponera pedagogiska texter och eftersom vi läst kursen har vi även nu studerat hur man faktiskt bygger upp domänspecifika språk med Haskell. Så det arbete som ligger framför oss nu är att fortsätta jobba med / producera den tutorial som vi pratat om idag, med hjälp av allt förarbete vi gjort och med fortsatt feedback från de som testar vår produkt under arbetets gång. 




### Visuellt till redovisningen:

- Illustrera statistik, först mörkat sedan omörkat. Effektfullt.
- Illustrera statisktik med publik! och allsång!
- Visa upp learnyouahaskell
