Fillarilaskennat
================

Ideana on polkupyörien konelaskentapisteiden datan yhdistäminen ilmatieteenlaitoksen säähistoriadataan - kuinka paljon säätila oikeasti vaikuttaa pyöräilymääriin ja on trendeissä tapahtunut muutoksia vuosien kuluessa?

Kansiot:

* _R/_  Datan siivoukseen ja analyysiin liittyvät R-scriptit. *a script that produce the ' pyoralaksennat_table_20120317.csv' (note the typo!).*
* _data_ Kansiossa on vuoden 2011 konelaskennat alkuperäisinä, sekä ZIP -paketissa PP_04_10.zip konelaskennat vuosilta 2004-2010.
* _output_ Alkuperäisdatasta scriptillä tuotetut siivotut datasetit



## Ilmatieteenlaitoksen säähavaintodata

https://ilmatieteenlaitos.fi/avoin-data


## Helsingin koneellisetn pyörälaskentojen data

Helsingissä on nykyisin (vuonna 2012) 12 konelaskentapistettä.  Viidessä pisteessä lasketaan ympäri vuoden ja muissa noin huhtikuusta syyskuun loppuun.  Laskennat tehdään siis vuorokauden ympäri.  Tulokset ovat 15 minuutin välein, mutta tulokset otetaan tunneittain. Laskennat tehdään Marksmanilla. Tiedot käydään imaisemassa kerran kuukaudessa. Marksmanin tuloksista ne siirretään Exceliin, johon ne otetaan vuorokausilukuina. Pisteet, joissa lasketaan, kuvaavat suurimmaksi osaksi työmatkaliikennettä, osa vapaa-ajan liikennettä ja osa asiointiliikennettä.
 
Kesäkuussa tehdään arkisin klo 7- 19 käsilaskentoja. Niitä tekevät koululaiset 15- 17-vuotiaat kesätyönään. Tällöin lasketaan vakiopisteiden lisäksi suunnittelijoiden toiveet. Joka kolmas vuosi lasketaan niemen ja kantakaupungin rajan ylittävät pyöräilijät. Niemen raja lasketaan, joka vuosi, mutta ei kaikissa pisteissä. Pyöräilijöiden kypärän käyttö lasketaan aina samoissa paikoissa vuosittain. Myös konelaskentapisteissä lasketaan käsin, jolloin voi tarkistaa konepisteen luotettavuuden.  Muutama laskee todella hyvin, muutama huonosti.

Käsilaskennoissa otetaan laskenta-aika klo 7-19, vuorokausi (kerroin konelaskentapisteistä) ja huippuvuorokausi. Huippuvuorokausiluvulla poistetaan sään vaikutusta. Kerroin saadaan Eläintarhan konelaskentapisteen touko-elokuun viiden korkeimman arkivuorokauden keskiarvona.
 
Käsilaskentojen yhteydessä tehdään suunnittelijoiden toivomia jalankulkulaskentoja.
 
Pyöräilystä tehdään vuosittain muistio, joka on kaupunkisuunnitteluviraston nettisivuille. Lisäksi siellä kartat pyörä- ja jalankulkulaskennoista. http://www.hel.fi/hki/ksv/fi/Liikennesuunnittelu/Liikennetutkimus

Datassa käytetyt paikkamerkinnät:

102=Kuusisaarensilta
103=Kulosaarensilta
107=Eläintarhanlahti
108=Hesperian puisto
112=Veräjämäki
115=Eteläesplanadi
116=Kantelettarentie
1171=Lauttasaaren silta, etelä
1172= Lauttasaaren silta, Pohjoinen
118=Kehä I, Vantaajoki
120=Nordenskiöldinpolku
