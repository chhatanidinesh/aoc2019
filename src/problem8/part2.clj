(ns problem8.part2
  (:require [clojure.string :as cs]))

(defn solve
  []
  (let [input "222222222222022212222222222220222221222212022202221222212020212222222222222222222222222222222201222022222222122222212222222202112022220222222112212222222222222222022212222222222222222221222212022202220222212121222222222222222222222222222222222201222222222222022222202222222212212122220222222222222222222222222222122202222222222221222222222202222212222222202222222222222222222222222222222222222222222122222222022222202222222212202222120222222002222222222222222222122212222222222221222222222202022222220222202220222222222222222222222222222222222212222222222222222222222222222212212121221222222222222222222222222222222212202222222220222221222212220212222222222021202222222222222222222222222222222212222122222222022222202222222212222120022222222222222222222222222222222212202222222222222222222212222202222222222220212222222222222222222222222222222201222122222222222222222222222212102121122222222102202222222222222222222222222222222222222221222222021212221222212121222222222222222222222222222222222212222022222222222222212222222212222122221222222222202222222022222220222222212222222221222221222212222212222222222022212222222222222222222222222222222222222222222222122222202222222202002020121222222212212222222122222221022212202222222220222220222202121202221222222222222222222222222222222222222222222221222122222222222022222222222212102220121222222122202222222022222222122212212222222221222222222212222202222222222121212222222222222222222222222222222221222022222222022122222222222202022020222222222112212222222222222221122202212222222222222222222212021212222222222022222222222222222222222222222222222212222022222222222022222222222222212222222222222212202222222022222221022212212222222222222220222202121212222222212120212222222222222222222222222222222222222122222222022122212222222222122120220222222212222222222122222221122202222222222222222220222222122202221222202022202222222222222222222222222220222220222022222222122222222222222212002022222222222222222222212022222221022212222222222222222222222212221222220222202022222222222222222222222222222220222221222122222222122222222222222202212120120222222222212222202222222220122212212222222220222221222202121212222222212222202222222222222222222222222222222212222122222222220222222222222222222020221222222122212222202022222221022222202222222220222220222212121212221222202221222222222222222222222222222220222221222122222222221222202222222212122220020222222202212222212022222220022222222222222220122222222202121202222222202220202222222222222222202222222222222211222222222222022022212222222222122222122222222022212222202022222220022222212122222221222220222202122202222222202020212222222222222222222222222220222212222022222222022222022222222012202021122222222222222222222022222222022222212122222222122221222202221212220222222222222222222222222222212222222221222200222222222222220122102222222102222021220222222202202222202022222221222222222122222220222220222212122202221222222220222222222222222222212222222222222221022222222222121222222222222222002220122222222102222222202022222220122202222022222221122222222212122210222222222022202222222222222222222222222220222202222222222222120122112222222102122120221222222122222222222122222222022212202222222220122221222212021222221222222100212222222222222222212222222221222211222122222222122122012222222202122220220222222122222222222122222220122202202222222220222222222222220211221222222111222222222222222222212222222221222220222022222222122222012222222202222222121222222002212222212122222222022222222222222221122221222212220221221222222220202222222222222222222222222221222211122222222222121112212222222202222222122222222022202222202122222220222222202122222220222220222222020212022222222112202222222222222222202222222220222212122022222222220102112222222222102122121222222212202222212022222222222212222122222222122222222202122220221222202002202222222222222222212222222221222210022122222222121002212222222002012020120222222102202222222222222221222222212122222221222220222222020212021222222000212222222222222222202222222221222212122222222222022212102222222202012220020222222112202222212022222220222212222222222220122222222222222201022222212010212222222222222222202222222222222212222022222222022012122222222222212121120222022122202222212222222221122212222022222221022221222222021222121222212222222222222222222222222222222221222210222122222222222102102222222212122120020222222122202222202220222221222222222122222222122221222202021212220222222101202222222222222222212222222222222201122222222222020002212222222022112020022222122012222222212122222220122222202022222222122220222202220202120222222120222222222222222222202222222222222222022222222222121212212222222112202221121222122222202222222120222221222222212222222222022220222222121211222222212110222222222222202222212222222221222211222122222222121012222222222202122021122222022002212222212021222221022202202022222222222220222212220201122222212020202222222222222222202222222220222220222222222222220012212222222122112121220222222002202222222221222220022212212012222221022222222212222221120122222211202222222222222222222222222220222201022222222202222002002222222102112022222222122122222222202220222220222222202012222220122221222202122220221122222000222222222222212222222222222221222210222122222202222022012222222112222022120222222102202222212122222221122212222022222222222221222222220212121022212211222202222222202222212222222220222220222022222212122212022222222002202020020222022012222222202120222222022212212202222221022221222222221202222122212120212222222222212222212222222221222221122212222212120122002222222002002121120222122102222222212221222221222222222112222222222222222202021200121022212002202202222222212222212222222222222212222212222212121212122222222102002220221222122022222222212120222220222222222102222220122220222212121211221222212101222222222222202222212222222221222200022222222202120112012222222002202122221222122222202222222220222220122212202201222220022220222222220210122222212101222222222222202222222222222222222202022122222222122112112222222022022222222222122002202222212220222220022212222002222222022220222212122221220222202001212212222222212222202222222220222202122122222202020122212222222102100120020222122222222222202222222221022202202101222222022222222202121220022122212012202212222222212222202222222221222201022022222222022212202222222022100021120222222202222222212220222220122212222120222221222220222222120200220222212222212202222222212222202222222222222210022122222222120212122222222012120122222222122202222222202220222220222202212212222221122220222212121200222022212110012202222222222222222222222220222221122022222202220122112222222022001120121222122102222222222120222220122212202111222221222221222222221201120122222100212212222222202222212222222222222221122002222212121022122222222012022122021222122222202222212222222220122212202120222222122220222202222222121122202011202202122222212222212222222221222210122122222212122112222222222122021221020222222222222222212121222220022222202021222221222222222212122211221122202111102222222222212222222222222222222221122202222212221202222222222012120020020222122212222222212122222220022212221102222221222221222222020211222222202021212202122222202222202222222222222220002012222202121200002222222122020122122222222222202222212220222220022202200022222221122220222222020202222222222122212212022222202222202222222221222210112222222202221001022222222112221022221222222012212222222120222221222212221022222220222220222222120211020022222221022212022222212222222222222221222200112002222202122112102022222202020020221222022012222222222020222222122202202220222221122221002202020202221122222022022222022222212222202222222220222211112202222202020222022022222212101020221222022002212222222122220221022212210101222222222221012212122201022022202102022212022222222202222222222222222210022122222212222211002022222202120022222222222102222222222220222220022212210021222220022222102222122200220122022121112202022222102202222222222120222220202122222222020222222222222212012120121222122122222222212102222221222202210101222221122220212222221212022222122000112222222222112202202222222120222200202222222212221122202022222022210120022222022002222222202202220221022222202201222221022220202222122212022022212110122212122222102212222222222122222221202012222212122211012222222012001222022222222002222222202102221220222222210200222221122221122222221201222022202212122211122222202202202222222021222202002212222212121221122122222112112220221222222012212222222021221222102202200022222220022220112202221200221022122220012221122222112222222222222121222202212212222222020122122122222122011221121222122101202222202121222220221202220022222220122222212222222210122022222002102222222222112202212222222021222201012022222202121200112122222212200221021222022110202222212221220221202222210001222221222220012222121212220222102210222220022222112202202222222021222201022212222202122012222022222202100022220222022101202200202202222220222202201010222222222221002212022210020122022202112211022222100212222222222222222220102012222212221200012222222112011022002222022000202002212011221220212202212221222220222222002202221210122022122002022220122212022202212022222022222220202012222202022012212122222012011021022222022011202100202012221220021202212021222221122221102202022221021222012022202200122202222212222222222020222210202112222212022001122102222222222121020222022002210011222201222220002212211220222222122222012212120211022122012210212210122202220222212122222221222221022000222202122101202222222112200221021222122121211000212112221222101222222220222222022220012212121202022122202212102210222222202202202122222020222221122112220222020010002222222102021221010222222220202020212112222220122212220102222221222222202202021202122022112112222202122212222212202222222021222220012101221212220220202102222012010121121222222200211112202011221222102222222112222221122221112202020210220022112201002200222222020202202222222221222202122011222220222212012002222122012021221222222212200210212210221222001212212002222222122220122202022211020122202022112201022202121222212122222220222210102102220212022020012222222212022021202222222220211101222101220220011222210212222222002221122212222221122122122101102222022202100222212222222222222221122211221211022202112222222012021022121222222002210012102021222220011212212101222221212221102202222200220022122211202202222202222202212222222020222222002221220221121020012202222212101121001222122020211010202112222221010222211020222222112220012202222201221122012012212200222202000222222222222222222220112110220200021120022102222002211022000222122212202010202200222221120212221211222221102222012202122201120222022122022210122222021202222122222020222212012011221221220000012112222222112222122222122121220222122122221222110222211211222222112221002212120200021122112000212220122222200202202122222020222200122122220202120010002102222022010220001222022101210210202211222220012222210022222221022222122212020211020222102101212221022222110212222222222121222202212002221210020021202022222122210020010202122222201101022012220221201202201210222222012221112212120200020112222001202211022222020222212022220020220202122110222202022222012222222102110220101222222101010012002020220220001222211101222222022222222222120102122012112211222212222202210202212122221120221220002212220222121201112202222202111220200212022011220011222021221221100202221111212222122221202222221111022002112001102200222212101212222122222220220201112201222202122120122102222002120020200202222220122001112001220220122211201120212222022220212212222001222222202211112202212212120202212122202122222211122121222220222010212122222212111221201222222210012022102002221220101220220200212220222220212212120100020002022120222220002222102202202222202220220201102122222212120222112012222202100121011202022001102011102202221220210220220011222221222220002212120100121122012122122210202202111202212222210022221222222011220211121202022122222122111021020202222100011121112201221021100210112012222222012221002212220000221012212000112202002212112212202022200022220202212120221212122221102122222202110122220222022121021020222002220222212221012101202220102220012202122012122212222122202211012212110212212220201120220200022210221212120101022212222102011222101222022112010020012210222220012110021010202222102221222222002201022222022121102212112202100202222221212222222221102100221201022111102110222002002022221202122112122122012112220021012120100201222220112222202212221002221122222010122222102222211212201021201022221222122201220220022202022010222102210120220202222102101201102022220220200102000112222220122222212222111221120202102122202222012222211222122021220122220202022121221202121120200201222002001222211212222100020101022021221020002102101010202222112222022202200100220102112021112222002212210222120221201222222200122111222211120111112012222212021121022222022110210222202010221120100212220222202221122222002222102102022102112111222222202212020212100120212121222211022001222200221012010122222012122122010222022101222221022111212221010010022200222222212220222222112022221212112101012222202212011212112020200220221212122102220221220120002202222212221102000212122200121220212221212122001211202001212220102221212202010222221222112211122202212212220212211122200121220212102210221211222211011001222102111121201222122102100120111000210020111112210011212220022221002202012000121202202001202221002222212222101120222220222220122202220211020112202021222202021212221221221121001121022012212122101102110011202220122222102212122022222202212020102200202202011212101222211021222210002002221221221022211120220012101022200202222010211202101020221020201100110011222222222222202212020222220102122100022222222222012202012222202121222202122002222201222000221110221122200020121210120222221010201100202020010120022011202222212221012212200101221002002102002222212222002202201222200220222202022101222201120102021021221122110001111212222120212211102102200122102221120100202221122221022222121012120122022002022220202202122212020120210121120212012122222200022221112100221222001012222212020202100002212100210221022212120121222222022220202200201200000022202102122221102212101202200021211021221222202210211202121222122102222202210212110200022110022001020022200221010010111001202222212221112201211012012220022101202220202212122202100221200221020220122210200210122221211221222202122021220200201200101222202200211122221211021001212222002220202220101120121220202202022200022212120212021120222122020212002211222201020220122020222112120102000001021011101212111200221021221020202220220221112222122222012200202222102000022202022201021222210222212220122211202100221212222001010101121212201110010001220101022212102211200222000012010022202220222221022211220221202122222112010202222222010200010222222120222212222011200211220211220202022112222201121100221110120201211001201020211221111012202222112222012201101202211012122020121202102222121212002121222022021202002210201222220010221121021212021112221021100020220100012110112212200120110012011011110102120100002121222120200220100100120022122111001201011101100010210210002100101010102122002121122222000222120120210021"
        s-input (cs/split input #"")
        i-input (map #(Integer/parseInt %) s-input)
        g1 (partition-all 150  i-input)
        res (apply map (fn [& xs]
                     (let [r (some #{0 1} xs)]
                       (if (= r 0)
                         " "
                         "x")
                       ))
                   g1)]
    (map println (partition 25 res))))