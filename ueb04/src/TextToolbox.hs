module TextToolbox where

import Data.Char
import Data.List


-- 4.1

countChars :: String -> Int
countChars = undefined


countCharsWOSpaces :: String -> Int
countCharsWOSpaces = undefined


countWords :: String -> Int
countWords = undefined


averageWordLength :: String -> Float
averageWordLength = undefined


freqOfChars :: String -> [(Char, Int)]
freqOfChars = undefined


-- 4.2

formatText :: String -> Int -> [String]
formatText t l = undefined


addLineNrs :: [String] -> [String]
addLineNrs = undefined


searchString :: String -> [String] -> [String]
searchString = undefined


nicelyPrint :: [String] -> String
nicelyPrint = undefined


-- sample texts

text_dt = "Da ihr gewiss schon die Abenteuer von Tom Sawyer gelesen habt, so brauche ich mich euch nicht vorzustellen. Jenes Buch hat ein gewisser Mark Twain geschrieben und was drinsteht ist wahr – wenigstens meistenteils. Hie und da hat er etwas dazugedichtet, aber das tut nichts. Ich kenne niemand, der nicht gelegentlich einmal ein bisschen luegen taete, ausgenommen etwa Tante Polly oder die Witwe Douglas oder Mary. Toms Tante Polly und seine Schwester Mary und die Witwe Douglas kommen alle in dem Buche vom Tom Sawyer vor, das wie gesagt, mit wenigen Ausnahmen eine wahre Geschichte ist. – Am Ende von dieser Geschichte wird erzaehlt, wie Tom und ich das Geld fanden, das die Raeuber in der Hoehle verborgen hatten, wodurch wir nachher sehr reich wurden. Jeder von uns bekam sechstausend Dollars, lauter Gold. Es war ein grossartiger Anblick, als wir das Geld auf einem Haufen liegen sahen. Kreisrichter Thatcher bewahrte meinen Teil auf und legte ihn auf Zinsen an, die jeden Tag einen Dollar fuer mich ausmachen. Ich weiss wahrhaftig nicht, was ich mit dem vielen Geld anfangen soll. Die Witwe Douglas nahm mich als Sohn an und will versuchen, mich zu sievilisieren wie sie sagt. Das schmeckt mir aber schlecht, kann ich euch sagen, das Leben wird mir furchtbar sauer in dem Hause mit der abscheulichen Regelmaessigkeit, wo immer um dieselbe Zeit gegessen und geschlafen werden soll, einen Tag wie den andern. Einmal bin ich auch schon durchgebrannt, bin in meine alten Lumpen gekrochen, und – hast du nicht gesehen, war ich draussen im Wald und in der Freiheit. Tom Sawyer aber, mein alter Freund Tom, spuerte mich wieder auf, versprach, er wolle eine Raeuberbande gruenden und ich solle Mitglied werden, wenn ich noch einmal zu der Witwe zurueckkehre und mich weiter ›sievilisieren‹ lasse. Da tat ich's denn. Die Witwe vergoss Traenen, als ich mich wieder einstellte, nannte mich ein armes, verirrtes Schaf und sonst noch allerlei, womit sie aber nichts Schlimmes meinte. Sie steckte mich wieder in die neuen Kleider, in denen es mir immer ganz eng und schwuel wird. ueberhaupt ging's nun vorwaerts im alten Trab. Wenn die Witwe die Glocke laeutete, musste man zum Essen kommen. Sass man dann gluecklich am Tisch, so konnte man nicht flott drauflos an die Arbeit gehen, Gott bewahre, da musste man abwarten bis die Witwe den Kopf zwischen die Schultern gezogen und ein bisschen was vor sich hingemurmelt hatte. Damit wollte sie aber nichts ueber die Speisen sagen, o nein, die waren ganz gut soweit, nur missfiel mir, dass alles besonders gekocht war und nicht Fleisch, Gemuese und Suppe alles durcheinander. Eigentlich mag ich das viel lieber, da kriegt man so einen tuechtigen Mund voll Bruehe dabei und die hilft alles glatt hinunterspuelen. Na, das ist Geschmacksache! Nach dem Essen zog sie dann ein Buch heraus und las mir von Moses in den Schilfern vor und ich brannte drauf, alles von dem armen kleinen Kerl zu hoeren. Da, mit einemmal sagte sie, der sei schon eine ganze Weile tot. Na, da war ich aber boese und wollte nichts weiter wissen – was gehen mich tote und begrabene Leute an? Die interessieren mich nicht mehr! – Dann haett' ich gern einmal wieder geraucht und fragte die Witwe, ob ich's duerfe. Da kam ich aber gut an! Sie sagte, das gehoere sich nicht fuer mich und sei ueberhaupt »eine gemeine und unsaubere Gewohnheit«, an die ich nicht mehr denken duerfe. So sind nun die Menschen! Sprechen ueber etwas, das sie gar nicht verstehen! Quaelt mich die Frau mit dem Moses, der sie weiter gar nichts angeht, der nicht einmal verwandt mit ihr war und mit dem jetzt nichts mehr anzufangen ist, und verbietet mir das Rauchen, das doch gewiss gar nicht so uebel ist. Na, und dabei schnupft sie, aber das ist natuerlich ganz was andres und kein Fehler, weil sie's eben selbst tut."

text_en = "You don’t know about me without you have read a book by the name of The Adventures of Tom Sawyer; but that ain’t no matter. That book was made by Mr. Mark Twain, and he told the truth, mainly. There was things which he stretched, but mainly he told the truth. That is nothing. I never seen anybody but lied one time or another, without it was Aunt Polly, or the widow, or maybe Mary. Aunt Polly—Tom’s Aunt Polly, she is—and Mary, and the Widow Douglas is all told about in that book, which is mostly a true book, with some stretchers, as I said before. Now the way that the book winds up is this: Tom and me found the money that the robbers hid in the cave, and it made us rich. We got six thousand dollars apiece—all gold. It was an awful sight of money when it was piled up. Well, Judge Thatcher he took it and put it out at interest, and it fetched us a dollar a day apiece all the year round—more than a body could tell what to do with. The Widow Douglas she took me for her son, and allowed she would sivilize me; but it was rough living in the house all the time, considering how dismal regular and decent the widow was in all her ways; and so when I couldn’t stand it no longer I lit out. I got into my old rags and my sugar-hogshead again, and was free and satisfied. But Tom Sawyer he hunted me up and said he was going to start a band of robbers, and I might join if I would go back to the widow and be respectable. So I went back. The widow she cried over me, and called me a poor lost lamb, and she called me a lot of other names, too, but she never meant no harm by it. She put me in them new clothes again, and I couldn’t do nothing but sweat and sweat, and feel all cramped up. Well, then, the old thing commenced again. The widow rung a bell for supper, and you had to come to time. When you got to the table you couldn’t go right to eating, but you had to wait for the widow to tuck down her head and grumble a little over the victuals, though there warn’t really anything the matter with them,—that is, nothing only everything was cooked by itself. In a barrel of odds and ends it is different; things get mixed up, and the juice kind of swaps around, and the things go better. After supper she got out her book and learned me about Moses and the Bulrushers, and I was in a sweat to find out all about him; but by-and-by she let it out that Moses had been dead a considerable long time; so then I didn’t care no more about him, because I don’t take no stock in dead people. Pretty soon I wanted to smoke, and asked the widow to let me. But she wouldn’t. She said it was a mean practice and wasn’t clean, and I must try to not do it any more. That is just the way with some people. They get down on a thing when they don’t know nothing about it. Here she was a-bothering about Moses, which was no kin to her, and no use to anybody, being gone, you see, yet finding a power of fault with me for doing a thing that had some good in it. And she took snuff, too; of course that was all right, because she done it herself."