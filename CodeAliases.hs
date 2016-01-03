{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, GADTs, 
    NoMonomorphismRestriction,
    OverloadedStrings, PatternSynonyms, QuasiQuotes,
    ScopedTypeVariables, TemplateHaskell, TypeOperators,
    ViewPatterns #-}
module CodeAliases(codealiases) where

import Data.Text (Text)
import qualified Data.Map as Map

codealiases = Map.fromList
    [("ALL","samtliga tumörer")
    , ("140","läpp")
    , ("1400","överläpp")
    , ("1401","underläpp")
    , ("1408","läpp, flera lokalisationer")
    , ("1409","läpp, ospecificerat ställe")
    , ("141","tunga")
    , ("142","spottkörtel")
    , ("143","munbotten")
    , ("144","munnen, annan eller ospecificerad del")
    , ("145","mellansvalget")
    , ("146","näs-svalgrummet")
    , ("147","svalget bakom struphuvudet")
    , ("148","svalget, ospecificerat ställe")
    , ("150","matstrupe")
    , ("1500","matstrupe specificerad del")
    , ("1508","matstrupe, flera lokalisationer")
    , ("1509","matstrupe, ospecifierat")
    , ("151","magsäck")
    , ("1510","magsäck övriga delar")
    , ("1511","övre magmunnen")
    , ("1518","magsäck, flera lokalisationer")
    , ("1519","magsäck, ospecifierat")
    , ("152","tunntarm")
    , ("1520","tolvfingertarmen")
    , ("1527","tunntarm övriga delar")
    , ("1528","tunntarm, flera lokalisationer")
    , ("1529","tunntarm, ospecificerat ställe")
    , ("153","tjocktarm")
    , ("1530","tjocktarm (caecum eller ascendens)")
    , ("1531","tjocktarm (transversum eller flexur)")
    , ("1532","tjocktarm (descendens)")
    , ("1533","tjocktarm (sigmoideum)")
    , ("1534","blindtarm")
    , ("1538","tjocktarm, flera lokalisationer")
    , ("1539","tjocktarm, ospecificerat ställe")
    , ("154","ändtarm och anus")
    , ("1540","ändtarm")
    , ("1541","anus")
    , ("1548","ändtarm och anus, flera lokalisationer")
    , ("155","lever och gallvägar")
    , ("1550","levercancer, primär")
    , ("1551","gallblåsa")
    , ("1552","gallvägar utanför levern")
    , ("1553","gallvägsmynningen")
    , ("1558","gallvägar, flera lokalisationer")
    , ("1559","gallvägar, ospecificerat ställe")
    , ("156","levercancer, ospecificerat")
    , ("157","bukspottkörtel")
    , ("158","bukhinna")
    , ("160","mellanöra, näshåla och bihålor")
    , ("1600","näsans insida och hålrum")
    , ("1601","örontrumpeten")
    , ("1602","överkäkshålan (sinus maxillaris)")
    , ("1607","bihåla övriga delar")
    , ("1608","mellanöra, näshåla och bihålor, flera lokalisationer")
    , ("1609","mellanöra, näshåla och bihålor, ospecificerat ställe")
    , ("161","struphuvud och stämband")
    , ("162","lunga, luftstrupe och bronker")
    , ("1620","luftstrupe")
    , ("1621","lungcancer, primär inkl bronker")
    , ("1622","lungsäck (pleura)")
    , ("163","lungcancer, ospecificerad")
    , ("164","bindvävsrum mellan lungorna (mediastinum)")
    , ("170","bröst")
    , ("171","livmoderhals (cervix uteri)")
    , ("172","livmoderkropp (corpus uteri)")
    , ("173","moderkaka (placenta)")
    , ("174","livmoder, ospecificerat ställe")
    , ("175","äggstock, äggledare och breda livmoderbanden")
    , ("1750","äggstock (ovarium)")
    , ("1751","äggledare och breda livmoderbanden")
    , ("1758","äggledare och breda livmoderbanden, flera lokalisationer")
    , ("1759","äggledare och breda livmoderbanden, ospecificerat ställe")
    , ("176","kvinnligt könsorgan")
    , ("1760","vulva")
    , ("1761","vagina")
    , ("1767","kvinnligt könsorgan, annan specificerad del")
    , ("1768","kvinnligt könsorgan, flera lokalisationer")
    , ("1769","kvinnligt könsorgan, ospecificerat ställe")
    , ("177","prostata (blåshalskörtel)")
    , ("178","testikel")
    , ("179","manligt könsorgan")
    , ("1790","penis")
    , ("1791","pungen (scrotum)")
    , ("1797","manligt könsorgan, annan specificerad del")
    , ("1798","manligt könsorgan, flera lokalisationer")
    , ("1799","manligt könsorgan, ospecificerat ställe")
    , ("180","njure och njurbäcken")
    , ("1801","njurbäcken")
    , ("1809","njure, ospecificerat ställe inkl njurvävnaden")
    , ("181","urinvägar utom njure")
    , ("1810","urinblåsa")
    , ("1811","urinledare")
    , ("1812","urinröret")
    , ("1816","urachus")
    , ("1817","urinvägar, annan specificerad del")
    , ("1818","urinvägar, flera lokalisationer")
    , ("1819","urinvägar, ospecificerat ställe")
    , ("190","malignt melanom i huden")
    , ("1901","melanom på ögonlock, i ögonvinkel")
    , ("1902","melanom på öra, i yttre hörselgång")
    , ("1903","melanom, annan specificerad del i ansiktet")
    , ("1904","melanom i hårbotten och på halsen")
    , ("1905","melanom på bålen och pungen")
    , ("1906","melanom på övre extremitet och axill")
    , ("1907","melanom på nedre extremitet och ljumske")
    , ("1908","melanom i huden, flera lokalisationer")
    , ("1909","melanom i huden, ospecificerat ställe")
    , ("191","tumör i huden, ej malignt melanom")
    , ("1911","hudtumör på ögonlock, i ögonvinkel")
    , ("1912","hudtumör på öra, i yttre hörselgång")
    , ("1913","hudtumör, annan specificerad del i ansiktet")
    , ("1914","hudtumör i hårbotten och på halsen")
    , ("1915","hudtumör på bålen och pungen")
    , ("1916","hudtumör på övre extremitet och axill")
    , ("1917","hudtumör på nedre extremitet och ljumske")
    , ("1918","tumör i huden, flera lokalisationer")
    , ("1919","tumör i huden, ospecificerat ställe")
    , ("192","öga, ögonhåla och tårkörtel")
    , ("1920","öga")
    , ("1921","synnerv")
    , ("1922","ögonhåla")
    , ("1923","ögats bindehinna")
    , ("1927","öga, annan specificerad lokalisation")
    , ("1928","öga, flera lokalisationer")
    , ("1929","öga, ospecificerat ställe")
    , ("193","hjärna och övriga nervsystemet")
    , ("1930","hjärna, hjärnnerver och hjärnhinna")
    , ("1931","ryggmärg, ryggmärgshinna")
    , ("1933","perifera nerver")
    , ("1938","perifera nerver, flera lokalisationer")
    , ("1939","perifera nerver, ospecificerat ställe")
    , ("194","sköldkörtel")
    , ("195","endokrin körtel")
    , ("1950","binjure")
    , ("1951","bisköldkörtel")
    , ("1952","brässen (thymus)")
    , ("1953","hypofys")
    , ("1955","endokrin körtelvävnad i bukspottkörtel")
    , ("1957","annan specificerad endokrin körtelvävnad")
    , ("1958","endokrin körtelvävnad, flera lokalisationer")
    , ("1959","endokrin körtelvävnad, ospecificerad körtel eller lokalisation")
    , ("196","skelettet")
    , ("1960","skalle, ansiktes ben och överkäksben")
    , ("1961","underkäksben")
    , ("1962","ryggrad")
    , ("1963","revben, bröstben och nyckelben")
    , ("1964","överarmsben och skulderblad")
    , ("1965","handlovsben, mellanhandsben och fingerben")
    , ("1966","höftben, korsben och svansben")
    , ("1967","nedre extremitetens långa ben")
    , ("1968","nedre extremitetens korta ben")
    , ("1969","skelett, flera lokalisationer eller ospecificerat ställe")
    , ("197","bindväv och annan mjuk vävnad")
    , ("1970","huvud inkl ansikte och hals")
    , ("1971","bål och rygg")
    , ("1972","övre extremitet och skuldra")
    , ("1973","nedre extremitet, höft och säte")
    , ("1974","retroperitoneala rummet")
    , ("1975","hjärta")
    , ("1977","bindväv, annan mjuk vävnad, annan specificerad lokalisation")
    , ("1978","bindväv och annan mjuk vävnad, flera lokalisationer")
    , ("1979","bindväv och annan mjuk vävnad, ospecificerat ställe")
    , ("199","ospecificerad lokalisation")
    , ("1991","ospecificerat ställe i huvud, ansikte och hals")
    , ("1992","ospecificerat ställe i bröstet (thorax)")
    , ("1993","ospecificerat ställe i buken och matsmältningskanalen")
    , ("1994","ospecificerat ställe i lilla bäckenet")
    , ("1995","ospecificerat ställe i extremiteterna")
    , ("1999", "ospecificerat ställe")
    , ("200","lymfatisk och blodbildande vävnad")
    , ("2001","malignt lymfom av non-Hodgkintyp, lymfocytärt")
    , ("2002","malignt lymfom utan närmare specifikation")
    , ("2003","malignt lymfom av non-Hodgkintyp, Waldenströms sjukdom")
    , ("201","Hodgkins sjukdom")
    , ("202","tumör i lymfatisk vävnad")
    , ("2021","tumör i lymfatisk vävnad, ospecificerad")
    , ("2022","tumör i lymfatisk vävnad, mycosis fungoides")
    , ("2024","hårcellsleukemi")
    , ("203","multipelt myelom, plasmocytom")
    , ("204","lymfatisk leukemi")
    , ("2040","akut lymfatisk leukemi")
    , ("2041","kronisk lymfatisk leukemi")
    , ("2049","lymfatisk leukemi, ospecificerad")
    , ("205","myeloisk leukemi")
    , ("2050","akut myeloisk leukemi")
    , ("2051","kronisk myeloisk leukemi")
    , ("2059","myeloisk leukemi, ospecificerad")
    , ("206","monocytleukemi")
    , ("2060","akut monocytleukemi")
    , ("2061","kronisk monocyleukemi")
    , ("2069","monocytleukemi, ospecificerad")
    , ("207","annan leukemi och ospecificerad")
    , ("2070","akut leukemi, ospecificerad")
    , ("2071","kronisk leukemi, ospecificerad")
    , ("2072","akut erytrocytleukemi")
    , ("2073","megakaryocytleukemi")
    , ("2079","leukemi, ospecificerad")
    , ("208","polycytemia vera")
    , ("209","myelofibros")]
