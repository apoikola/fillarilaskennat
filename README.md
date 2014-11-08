Fillarilaskennat
================


## TODO: Siivoa datan esikäsittely-, mallinnus- ja visualisointikoodit ja dokumentoi paremmin.

Ideana on polkupyörien konelaskentapisteiden datan yhdistäminen ilmatieteenlaitoksen säähistoriadataan - kuinka paljon säätila oikeasti vaikuttaa pyöräilymääriin ja on trendeissä tapahtunut muutoksia vuosien kuluessa?

Kansiot:

* _[R/](https://github.com/apoikola/fillarilaskennat/tree/master/R)_  Datan siivoukseen ja analyysiin liittyvät R-scriptit.
* _[data/](https://github.com/apoikola/fillarilaskennat/tree/master/data)_ Kansiossa on vuosien 2004-2011 polkupyörien konelaskentadatat alkuperäisinä (saatu Helsingin kaupunkisuunnitteluvirastosta), [Konelaskentapisteiden koordinaatit](https://github.com/apoikola/fillarilaskennat/blob/master/data/Laskentapisteet%202011%20-%20Laskentapisteet%202011%20FIX.csv), sekä [Kaisaniemen sääaseman havainnot 01/2004 - 08/2014](https://github.com/apoikola/fillarilaskennat/blob/master/data/kaisaniemi_saa_200401-201408.csv)
* _[output/](https://github.com/apoikola/fillarilaskennat/tree/master/output)_ Alkuperäisestä konelaskentadatasta scriptillä tuotetut siivotut datasetit (esim. _[kuu_20140927.csv](https://github.com/apoikola/fillarilaskennat/blob/master/output/%20kuu_20140927.csv)_)



## Ilmatieteenlaitoksen säähavaintodata

Data haettu [FMI:n rajapinnasta](https://ilmatieteenlaitos.fi/avoin-data)

rrday=sade, tday=keskilämpötila, snow=lumen paksuus(?), tmin=minimilämpötila, tmax=maksimilämpötila


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
