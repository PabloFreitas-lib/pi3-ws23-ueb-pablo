Informationssicherheit, 5. Übung
================================

* * * * *

Aufgabe 1, 4 Punkte, Gruppe
---------------------------

Fkdip, Lqp qamo wig Oibo igofyqjkiffijo. Waf cap ziprkojlyq tilg
xpvffif Dpvmjir ekip Ikyq. Cijyqif Zvpxiqig lfo irdeiqjigfcipo, kr mil
ilgir kgmitaggoig Zipeaqpig nkgaiyqfo ake wli Apo wip Zipfyqjkiffijkgx
nk tvrrig? Cli lfo wip Gari wip qlip zipcigwioig Apo zvg
Zipfyqjkiffijkgx?  Clifv jaiffo fli flyq fv ilgeayq igofyqjkiffijg?
Ekip wlifi Apo wip Zipfyqjkiffijkgx ilxgio flyq ilg Zipeaqpig mifvgwipf
xko. Cijyqif? Ake cijyqi Cilfi qamo Lqp wig Tjapoibo qipakfxiekgwig,
waf qilffo, cli filw Lqp zvpxixagxig?  Wpkyto mlooi wig xifaroig
kgzipfyqjkiffijoig Oibo lg Ikpip Jvifkgx am kgw zipcigwio wank wli
Yvwi-Krximkgx zvg Raptwvcg.

Qikoi ilgxifionoi Zipfyqjkiffijkgxfzipeaqpig flgw rilfo glyqo fv
ilgeayq nk igofyqjkiffijg. Ilgi clyqolxi Pvjji fdlijo wamil wli Jaigxi
wif Fyqjkiffijf. Cijyqi? Cli jagx fvjjoi wip Fyqjkiffij mil wip
Zipcigwkgx zvg AIF gvprajipcilfi filg? Giggo rlgwifoigf ilgi
Hkijji. Akf cijyqig Xpkigwig ckipwi rag lg AIF wvyq jaigxipi
Fyqjkiffij zipcigwig?

Zipcigwio vdigffj fdiiw rlo ilgir flggzvjjig Ajxvploqrkf kr nk
mipiyqgig, cli jagxi Ikip Piyqgip ioca mpakyqig ckipwi, kr AIF rlo wip
agxiximigig Fyqjkiffijjaigxi nk tgaytig. Ximo lg Ikpip Jvfkgx wig zvg
Ikyq zipcigwioig Akepke kgw wiffig Ipximglf ag kgw iptjaipo wig
Piyqigcix nk Ikpir Ipximglf.

**Dokumentiert Eure Vorgehensweise und nennt mindestens eine Quelle.**

* * * * *

Aufgabe 2, 4 Punkte, Gruppe
----------------------------

Bei einem System zur Rechnungsverwaltung der Firma "Secure Billing"
werden die ausstehenden Beträge in Latex-Dokumenten gespeichert, die
zur Darstellung zu PDF-Dateien kompiliert werden. Hier ein Ausschnitt
aus einer solchen Datei:

```
...
42,20Euro
11,20Euro%vom 1.1.2021
4000Euro
...
```

Zur Sicherung der Integrität der Daten werden in einer separaten
Datenbank für jeden Eintrag (Zeile ohne Zeilenende) aus dem
Tex-Dokument jeweils ein einzelner Hash gespeichert.  Dabei kommt
folgende, nicht standardisierte
Hashfunktion zum Einsatz:

```
char * hash (char * data){
	char * hashed_data = calloc(16, sizeof(char));
	int i=0;
	while (i<32){
		hashed_data[i%15] = 'A' + (hashed_data[i%15] + 12*i + (*data)) % ('Z' - 'A' + 1);
		data++; i++;
	}
	return (hashed_data);
}
```

Die Einträge sind bis zu 32 Zeichen lang, beim Einlesen werden nicht belegte Zeichen durch 0-Bytes (`\0`) auf 32 Byte aufgefüllt.

Ihr möchtet nun einen Rechnungsbetrag ändern. Ihr habt auch eine
Sicherheitslücke auf dem Server der "Secure Billing" gefunden, über
die Ihr das Latex-Dokument manipulieren könnt.  Ihr habt aber keinen
Zugriff auf die gespeicherten Hashes, müsst die Sicherung durch Hashwerte also überwinden, indem Ihr den Eintrag in dem Latex-Dokument geschickt manipuliert.

Wie könnt Ihr dennoch den Eintrag **5000Euro** zu einem Eintrag, der
als **50Euro** dargestellt wird, verändern, ohne dass es Veränderungen
im PDF gibt, durch die die Betreiber des Systems Verdacht schöpfen
könnten? Welchen Effekt hat die Manipulation von Zeichen an
verschiedenen Stellen des Eingabewertes auf den Hashwert?  Welche
Stellen des Hashwertes müsst Ihr ändern?  Warum? Welche Eigenschaft
der vorgegebenen Funktion hat die Lösung für Euch stark vereinfacht?
Achtet darauf, dass Euer manipulierter Eintrag aus Zeichen besteht,
die in einem Latex-Dokument darstellbar sind.

Beschreibt Euer Vorgehen und gebt den manipulierten Eintrag an! Beschreibt eine Gegenmaßnahme, um Euren Angriff zu verhindern.

**Hinweis**: In Latex leitet das Prozent-Zeichen ("%") einen Kommentar bis zum Zeilenende ein.


* * * * *

Abgabe
------


bis 2023-11-30 23:59 UTC, digital in Stud.IP als Markdown-Datei mit dem
Dateinamen `isec23_ueb05_grpYY.(md|zip|tgz)` (das `YY` mit Eurer Gruppennummer ersetzen).
Dabei bitte in der Datei die Nummer Eurer Gruppe in Stud.IP sowie alle
an der Abgabe beteiligten Gruppenmitglieder nennen.

Bitte steckt die Energie ins Denken und Schreiben, nicht in eine
wunderschöne Formatierung — lesbar darf es allerdings sein. Die
Lösungswege sollten ggf. nachvollziehbar sein.

Wenn Ihr Euch irgendwelcher Quellen aus dem Netz bedient (Anleitungen,
Howtos, etc), gebt diese bitte als URI mit an.

*Carsten Bormann, Karsten Sohr, Stefanie Gerdes, Jan-Frederik
Rieckers, Finn Ewers, Celina Röll ·
<isec@tzi.org>*, WS 2023/24
