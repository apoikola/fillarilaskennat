Fillarilaskennat
================

This repository contains data and code for processing, modeling and visualizing bike count data in Helsinki. Most of the material is in Finnish. An English summary of the modeling efforts is coming...

## Data and source code summary

Data

* Original (messy) data in folder `data_orig`
* Cleaned data (from R scripts) in folder `data_clean`
* Run the Python script `parse.py` to obtain a better version of the cleaned data.
* 

Important scripts (in the `source` folder)

* R scripts for cleaning the data: `liikenne_20120313.R` and `liikenne_20140927.R`
* Python script for cleaning the data: `parse.py`
* R script for cleaning and preparing the data for modeling: `prepare_data.R`
* R script for running the GAM model: `run_model.R`
* R script for plotting the model output: `plot_model.R`


Note! The folders were renamed at some point, so all scripts may not work anymore!


## Fillarilaskennat artikkeli

Fillarilaskennoista kirjoitettiin artikkeli, joka lähetettiin [Apps4Finland 2014 -kilpailun](http://apps4finland.fi/) hahmota-sarjaan. 

* Artikkeli julkaistu [Kaupunkifillari-blogissa](http://www.kaupunkifillari.fi/blog/2015/03/04/pyoraily-on-arkista-touhua/)
* Artikkeli [PDF:nä](https://github.com/apoikola/fillarilaskennat/raw/master/Fillarilaskennatartikkeli.pdf)
* Artikkelin [kirjoitusversio](http://bit.ly/fillarilaskennat_artikkeli) nähtävillä Google Docsissa
* Apps4Finland [kilpailutyö](https://trello.com/c/vV0K5hmr/165-pyoraily-on-arkista-touhua-datajournalistisin-keinoin-tarkasteltu-pyorailymaarien-trendeja-helsingissa-10-vuoden-ajalta) 
* Työtä varten saimme rahoitusta [Datademosta](http://datademo.fi/tiedote_20102014/)

Artikkelin taustalla olevan tilastollisen mallin koodit löytyvät tästä reposta. Artikkelissa käytetyt kuvat on tuotettu skriptiillä `source/plot_model_a4f2014.R`. Mallin tarkempi kuvaus työn alla.


## Taustaa

Ideana on polkupyörien konelaskentapisteiden datan yhdistäminen ilmatieteenlaitoksen säähistoriadataan - kuinka paljon säätila oikeasti vaikuttaa pyöräilymääriin ja on trendeissä tapahtunut muutoksia vuosien kuluessa?

Kansiot:

* _[source/](https://github.com/apoikola/fillarilaskennat/tree/master/source)_  Datan siivoukseen ja analyysiin liittyvät R-scriptit.
* _[data_orig/](https://github.com/apoikola/fillarilaskennat/tree/master/data_orig)_ Kansiossa on vuosien 2004-2011 polkupyörien konelaskentadatat alkuperäisinä (saatu Helsingin kaupunkisuunnitteluvirastosta), [Konelaskentapisteiden koordinaatit](https://github.com/apoikola/fillarilaskennat/blob/master/data_orig/Laskentapisteet%202011%20-%20Laskentapisteet%202011%20FIX.csv), sekä [Kaisaniemen sääaseman havainnot 01/2004 - 08/2014](https://github.com/apoikola/fillarilaskennat/blob/master/data_orig/kaisaniemi_saa_200401-201408.csv)
* _[data_clean/](https://github.com/apoikola/fillarilaskennat/tree/master/data_clean)_ Alkuperäisestä konelaskentadatasta scriptillä tuotetut siivotut datasetit (esim. _[kuu_20140927.csv](https://github.com/apoikola/fillarilaskennat/blob/master/output/%20kuu_20140927.csv)_)



## Ilmatieteenlaitoksen säähavaintodata

Data haettu [FMI:n rajapinnasta](https://ilmatieteenlaitos.fi/avoin-data)

rrday=sade, tday=keskilämpötila, snow=lumen paksuus(?), tmin=minimilämpötila, tmax=maksimilämpötila


## Helsingin koneellisten pyörälaskentojen data

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
