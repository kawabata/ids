;;; ids-equiv.el --- IDS character equivalence tool -*- lexical-binding: t; -*-

;; Copyright (C) 2014  KAWABATA, Taichi

;; Author: KAWABATA, Taichi <kawabata.taichi@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; IDS canonicalization and equivalence database.

;;; Code:

(defvar ids-canonicals)
(setq
  ids-canonicals
  ;; Characters can be canonicalized when:
  ;; (1) characters can be equally decomposed (e.g. "土 vs. 士")
  ;; (2) Counterpart pairs can not be decomposed furthermore (e.g. "八 vs. 丷")
  ;; (3) Character may contain equivalent character inside. (e.g. "大 vs. 犬")
  '(
    ("壬" "𡈼") ; [1]
    ("土" "士") ; [1a] [寺]CT, [吉𠮷],[壮-壮]
    ("工" "") ; [9] 昂-昻
    ("八" "丷") ; [15a] 兌-兑, S1.5.h
    ("十" "𠂇") ; [24]
    ("几" "𠘧" "𠘨") ;
    ("禾" "𥝌") ;
    ("刊" "刋") ; [24]
    ("凢" "凣") ; [24-1]
    ("丁" "𠄐") ; [31a]
    ("木" "朩" "𣎳") ; [30a] [栗]CT
    ("月" "⺼" "" "" "" "") ; [36] [望]CT, [炙]CT, [然]CT,肉月
    ("戌" "戍") ; [34a] [㤜-㤜]
    ("盇" "𥁋") ; [35]
    ("凡" "卂" "𠁽") ; [38] [巩]CJ, [嬴]CT, [𪎒-𪎒], [汎-汎]
    ("牛" "𠂒") ; [42] 吿-告
    ("肀" "") ; [43] 唐
    ("畀" "𢌿") ; [45] [鼻]CJ
    ("夨" "") ;
    ("里" "") ;
    ("承" "") ;
    ;; ("異" "") ;
    ("冄" "") ; [53]
    ("另" "叧" "𠮠") ; [60] 枴-柺,別-别
    ("亏" "亐") ; [62-3] 汚-污, [𡧈-𡧈], [𥁄-𥁄], [𥃳-𥃳], [𧥦-𧥦]
    ("山" "屮") ; [62-5] [嶲-嶲], [㒞-㒞]
    ("臿" "𢆍") ; [62-7] 挿-插-揷
    ("羊" "𦍌") ; [62-10] [善]CT
    ("眞" "真") ; [63b] 鷆-鷏,顚-顛,鎭-鎮,槇-槙,愼-慎,巓-巔,塡-填 S1.5
    ("県" "") ; [63c] S1.5
    ("抛" "拋") ; [65]
    ("日" "冃" "曰") ; [68] [冒]TJ
    ("己" "巳" "㔾" "已") ; [71a] 67,70,71を含む
    ("本" "夲") ; [71-1]
    ;; 亼	𠓛	[79]
    ("夾" "㚒") ; [79a] [䀹]CJ, [㣣]CJ error unify
    ("⺈" "𠂉") ; [31a] 尓-尔,亇[CJ]
    ("⺈" "刀" "𠂊") ; [81] 絕-絶, [兔]CK, [免-免]
    ("内" "內") ; [80] 吶-呐
    ("丏" "丐") ; [81-1] 麪-麫
    ("厂" "丆") ; [81-2] 䂖
    ("一" "丶" "乀") ; 匆-匆　対策
    ("夬" "") ;
    ("茲" "兹" "玆") ; [83] 孳-孶 [嗞]CJ, [嵫]CK, [慈-慈]CT, [螆]CJ, [滋-滋]
    ("卉" "𠦃" "𠦄") ; [84]
    ("𦣝" "𦣞") ; [90] 姫-姬*, 煕-熙
    ("姫" "姬") ; [91]
    ;; 叟	⿱申又	[92]
    ("申" "𦥔") ; [92a] [叟-叟]
    ("电" "") ; [92b] 奄-𡘹,[淹-淹], [掩-掩]
    ("㬰" "臾") ; [92c] [𣢧-𣢧], [瘐-瘐]
    ;; 寛	寬	[93]
    ("萈" "莧") ; [93a] 寛-寬
    ("冊" "册" "𠕁") ; [94] 删-刪,姍-姗,柵-栅,[珊]CJ	(S1.5)
    ("廾" "廾") ; [95]
    ("开" "幵" "𠦅") ; [96] 妍-姸, 研-硏, 訮-詽, 豜-豣, 邢-郉, 鈃-銒, [汧]CJ
    ("并" "幷") ; [97] 併-倂,屏-屛,帡-帲,瓶-甁,胼-腁,軿-輧,迸-逬,餅-餠,駢-騈	(S1.5)
    ("毎" "每") ; [98] [侮]CJ, [悔]CJ, [敏]CJ	(S1.5)
    ("黑" "黒") ; [99]
    ("熏" "𤋱") ; [99] 薫-薰
    ("東" "柬") ; [100] 諫-諌, 錬-鍊, 鶇-鶫, 䦨-闌,[煉]CJ, [練]CJ
    ("曽" "曾") ; [101] 増-增	(S1.5)
    ("子" "孑") ;
    ("乙" "𠃉") ;
    ("㞋" "𠬝") ; [赧]CJ,[報-報]
    ("厄" "卮" "𢀴") ; 卮は䝈のみ
    ("勳" "勲") ; [爋]CT
    ("㠯" "") ;
    ("攵" "夂" "夊") ; [106] 夐-敻 [62-2]
    ("市" "巿") ; [109] [姉]CT,[昁],[沛],[肺],[閙],[鬧],[㧊],[㸬]
    ;; 穉	[110]
    ("郷" "鄉") ; [111]
    ("匚" "匸") ; [118]
    ("先" "兂" "旡") ; [123a,b]
    ("大" "犬") ; [124] [器]CJ, 獎-奬, 戻-戾, 达-迖, 涙-淚, [類]CJ, [莽]CK, 臭-𦤀
    ("大" "太") ; [124a] 馱-駄
    ("免" "兔") ; [127] 嬎-嬔, 晚-晩
    ;; 辶	[128]
    ("豕" "豖") ; [129] [啄]CJ, [琢]CJ
    ("尢" "𡯁") ; [66]
    ("尢" "尤") ; [129-4] 㞊[GT]
    ("王" "玉") ; [129-6] 囯-国
    ("刄" "刅") ; [129-7] 剏-剙
    ("刄" "刃") ; [170] S1.5.j
    ("叉" "㕚") ; [129-1]
    ("丈" "𠀋") ; [129-2]
    ("单" "単") ; [129-5] 弹-弾	(S1.5)
    ("曳" "曵") ; [129-8] [㡼-㡼]CT
    ("徴" "徵") ; [131]
    ("𡵉" "𡵂") ; [132a] 徳-德
    ("巛" "𡿧") ; [134-3] 菑-葘, 輜-輺, [災-災], [甾-甾, 㿳[GT]
    ("寜" "寧") ; [134-1]
    ("舃" "舄") ; [134-2]
    ("鳯" "鳳") ; [134-4]
    ("戋" "㦮" "𢦍") ; [134-5] [残]CJ, [浅]CJ
    ("焭" "煢") ; [134-6]
    ("㡳" "底") ; [134-7] [菧-菧]
    ("奥" "奧") ; [135]
    ("粤" "粵") ; [135a]
    ("𡭴" "𡭽" "𡮂") ; [137] [隙]CJ
    ("篡" "簒") ; [138]
    ("吕" "呂") ; [138-1] 宫-宮	(S1.5)
    ("𤰞" "卑") ; [138-2] [婢-婢] ;; 無限ループ防止
    ("虽" "𧈧") ; [139] 強-强
    ;;  口	厶	[139]
    ("肙" "䏍") ; [139a] [悁-悁], [捐-捐], [睊-睊], [蜎-蜎], [鋗-鋗], [蜎-蜎] 圎-圓
    ("兖" "兗")
    ("衮" "袞")
    ("圖" "圗") ; [139d]
    ("員" "貟") ; [139e] 圎-圓, [霣-霣], [𤠔-𤠔]
    ("黃" "黄") ; [141] 横-橫	(S1.5)
    ("菫" "堇") ; [142]
    ("堇" "𦰌") ; [142]
    ("𦰩" "") ; [143]
    ("隺" "寉") ; [143-2] [鶴-鶴]
    ("争" "爭") ; [144] 静-靜,筝-箏,浄-淨,峥-崢,净-凈	(S1.5)
    ("為" "爲") ; [144a] 蒍-蔿,溈-潙,媯-嬀,偽-僞	(S1.5)
    ("口" "") ; [145] 高-髙
    ("靑" "青") ; [146] 淸-清	(S1.5)
    ("昷" "𥁕") ; [147] 煴-熅, 媪-媼, 愠-慍, 揾-搵, 榅-榲, 氲-氳, 温-溫, 緼-縕, 腽-膃, 蒀-蒕, 蕰-薀, 藴-蘊, 輼-轀, 醖-醞, 鰛-鰮	(S1.5)
    ("同" "") ; [148] 爂釁爨
    ("魚" "𩵋") ; [149] IRG24
    ("頼" "賴") ; [151] 瀨-瀬
    ("疐" "𤴡") ; [152-1] 嚏-嚔
    ("眾" "衆") ; [152-2]
    ("么" "幺") ; [152-3] 麼-麽
    ("壽" "夀") ; [152-4]
    ("友" "犮") ; [152-5] 抜-拔, 髪-髮
    ("皐" "臯") ; [152-6] 翶-翺, 皡-皥
    ("兔" "兎") ; [152-7]
    ("稟" "禀") ; [152-9] 䕲
    ("釆" "采") ; [152-11] [釉]CT, [彩-彩]
    ("爽" "𡙁") ; [152-12]
    ("廉" "亷") ; [152-13] [𣀊-𣀊]
    ("正" "𤴔") ; [152-10] 頙
    ("皀" "" "艮" "𠧢") ; [153] S1.5 廄-廏,廄-廏,既-旣,匓[CJK]
    ;; 	[154]
    ("食" "飠" "𩙿") ; [155] 飮-飲	(S1.5)
    ("象" "𧰼") ; [156]
    ;; 巤	[157]
    ("煕" "𤋮") ; [158]
    ("囱" "囪") ; [159]
    ;; 鼠	[160]
    ("示" "礻") ; [161] S1.5
    ("示" "𤓯") ; [祖-祖]
    ;; 状	狀	[162]
    ("丬" "爿") ; [162a] 妆-妝, 壮-壯, 寝-寢, 将-將, 荘-莊, 蒋-蔣, 状-狀, 装-裝, 醤-醬
    ;; 	[163]	(𠂢)
    ("" "𧘇")
    ("車" "𨊥") ; [165] 撃-擊
    ("业" "") ; [166] 嘘-噓, 戯-戱, 虚-虛, [黹-黹], [普-晉]
    ("彐" "彑") ; [167] 彔录, 168
    ("殻" "𣪊" "㱿") ; [174] 本来はUCS対象外
    ("冒" "冐") ; [176a]
    ("畫" "畵") ; [177]
    ("俞" "兪") ; [178] 偷-偸,喩-喻,婾-媮,楡-榆
    ("専" "專")
    ("恵" "惠")
    ("晉" "晋") ; [178-2] 戩-戬	(S1.5)
    ("缶" "𦈢" "𠙻") ; [178-4] [啣-啣], [啕-啕], [徭-徭], [揺-摇], [滛-滛]
    ("羮" "羹") ; [178-5]
    ;; 丗	(8C4B)	[178-6]
    ("帯" "带") ; [178-6]
    ("走" "赱") ; [178-7]
    ("𣴎" "羕") ; [178-8]
    ("羡" "羨") ; [178-9]
    ("亡" "亾" "兦") ; [178-10] [慌-慌], 㡃/㡆,[惘-惘],巟-㠩,罔-㒺
    ("网" "𦉳") ; [178-11]
    ("睿" "𥈠") ; [179a] 㲊䜜
    ;; [180]
    ("手" "龵") ; 掰
    ("乑" "") ; [聚]
    ("歹" "歺") ; 殩 (𥹏vs粲)
    ("嬴" "𡣍") ; [瀛-瀛]
    ("厂" "𠂆") ; [𠨬-𠨬]

    ("小" "忄" "𡭔") ;
    ("冂" "冖" "") ;
    ("冈" "罓") ;
    ("卄" "艹") ;
    ("卄" "廾") ;

    ("口" "囗") ;
    ("王" "𤣩") ;
    ("竹" "𥫗") ;
    ("牛" "牜") ;
    ("糸" "糹") ;
    ("言" "訁") ;
    ("足" "𧾷") ;
    ("䜌" "龻") ;
    ("𠦝" "龺") ;
    ("金" "釒") ;
    ("卜" "⺊") ;
    ("㐫" "㓙" "囟") ;
    ("卥" "𠧧") ;

    ("贛" "𥫔") ;
    ("亇" "个") ;
    ("寇" "𡨥") ;

    ("帀" "币") ;
    ("夅" "𡕘") ;
    ("瓜" "𤓰") ;
    ("𣎼" "𡥀") ;
    ("𠬛" "") ;
    ("婁" "𡝤") ;
    ("与" "") ;
    ("𦔮" "耴") ;
    ("會" "㑹") ;
    ("工" "") ;
    ("㸚" "𠈌") ;
    ("𢏚" "") ;
    ("𠃬" "") ;
    ("希" "𢁫") ;
    ("龠" "𠎤") ;
    ("由" "𠙹") ;
    ("幺" "乡")
    ("円" "丹")
    ("厂" "")
    ("又" "")
    ("甚" "𫞪")
    ("今" "𫝆")
    ("" "")
    ("冉" "")
    ("𠂎" "")
    ("" "")
    ("" "")
    ("𠀉" "" "" "")
    ("亼" "" "亽" "𠓛")
    ("丩" "𠂈") ;
    ("厂" "") ;
    ("卩" "龴" "") ; [令]CJ, similarity by China
    ("止" "龰") ;
    ("㐅" "乂") ;
    ("" "𦣻") ; [戛-戛]
    ("永" "𣱵" "𣱳") ; [178-8] [羕-羕],D13056
    ("才" "") ;
    ;; ("口" "𠔼") ; 𢘖
    ("彐" "⺕" "") ;
    ("甾" "𠚋") ;
    ("𦬇" "𦬠") ;
    ("𦍋" "芈" "羋") ; [哶-哶], [䖹]
    ("二" "𠄠" "𠄟" "𠄞")
    ("" "⺂")
    ("日" "臼" "𦥑") ; [71-3] 捏-揑, 陧-隉, [叟]CJ, [臾]simsun

    ("丨" "丿") ; 亅U+4E85
    ("龶" "主") ; [112-1]
    ("土" "𠀆") ; [44a]
    ("" "")
    ("𠃍" "乛") ; 
    ("𠃍" "𠃌")
    ("兀" "丌") ; [123-1] [嬈-嬈],
    ("乚" "𠃊") ;乙
    ("廿" "龷") ; [143-1] 襔,㒼-䓣
    ("冫" "⺀") ; [25] [冬]CT
    ("九" "丸" "") ; [129-3] 骩-骫 no other examples
    ("且" "旦") ; [64] 查-査, [蔖-蔖]
    ("毋" "母")
    ("人" "入" "𠆢") ; [79] [全]TJ
    ("𫶬" "")
    ("" "甫") ;
    ))

;; Equivalences

(defvar ids-equivalents)
(setq
  ids-equivalents
  ;; Characters are equivalent when:
  ;; These characters can be decomposed differently (e.g. 王 vs. 壬)
  ;; e.g. 王→⿱一土, 壬→⿱丿士
  '(
    ("王" "壬") ; [1]
    ("干" "千") ; [1b]
    ("丰" "丯" "龶") ; [4] [契]CJ, [憲]CJ, [害]CJ
    ("戸" "戶" "户") ; [5] S3
    ("天" "夭") ; [6] 吞-呑, [忝]
    ("⺶" "⿱𢆉丆")

    ("孝" "𡥉" "𡥈") ; [8a]
    ("参" "叁") ; [10-1]
    ("參" "叄") ; [10-1]

    ("氺" "" "⿲𠄠丨𠄠") ; [17]
    ("水" "氺") ; [17a] [眔]CJ, [犀-犀]

    ("羽" "⿰彐彐") ; [19a]
    ("卯" "⿰𠂎刀") ; [23]
    ("殳" "⿱⺈又") ; [25-1] 沒-没, 歿-殁

    ("西" "覀") ; [27a]
    ("覀" "襾") ; [88] [覃],[覂],[䌁],要/S1.5.d

    ("四" "罒") ; [28a] XMLファイルには未掲載
    ("𠱠" "罒") ;

    ("月" "⿵⺆⺀") ; [36] 舟月
    ("凡" "⿵几一") ; [37]

    ("甫" "⿺𤰔丶") ; [40]
    ("甬" "⿱龴田") ; [41]
    ("吳" "呉" "吴") ; [48] [茣]CT, [虞]CT, [誤]CT	(S1.5)
    ("吳" "⿺夨口") ; [48] [茣]CT, [虞]CT, [誤]CT	(S1.5)

    ("毋" "毌") ; [98a] 毎-每, [侮]CJ, [瑇-瑇], [𦔣-𦔣], [貫-貫]
    ("田" "毌") ; [58]

    ("爰" "⿳爫土夂") ; [61a] 㬊[J]
    ("圼" "⿱臼土" "⿱臼工") ; [62] 捏-揑, 陧-隉, 毀-毁

    ("圣" "𢀖") ; [62-9] [茎]
    ("聿" "⿱𦘒一") ; [62-11] [衋]CJ

    ("九" "尢") ; [65a]
    ("兀" "尢") ; [62-8] 尪-尫, 抛-拋, 尶-尷

    ("儿" "几") ; [69]
    ("八" "儿") ; [26a]

    ("义" "叉") ; [71-2] [䁊]CT, [㳗]CK, [蚤]CT, [芆]CT

    ("产" "⿱文厂") ; [72] 産-產, 彥-彦, 顏-顔

    ("冈" "𦉪" "𠔿") ; [73] 嫓
    ("𠔿" "") ; 奐 𦉪 for BMP, 𠔿 for extB, [像-像]

    ("匕" "𠤎" "七") ; [78] [叱-叱]CJ

    ("犀" "⿸尸⿱氺㐄") ; 遲-遲

    ("卄" "卝") ; [82]

    ("𠫓" "⿱亠厶") ; [105] [育]CT, [㐬]
    ("巩" "⿰工几") ; [129-9] [築-築]
    ("𢛳" "⿳十罒心") ; [130] 徳-德
    ("㚅" "⿱夂生") ; [133a]
    ("" "⿱士冖") ; [134a] 壳

    ("小" "𣥂") ; [10a]
    ("少" "𣥂") ; [136] 步-歩,涉-渉,歲-歳	(S1.5)

    ("堇" "⿱廿⿻口土") ; [142b]
    ("曷" "⿱日匂") ; [150] 渇-渴, 掲-揭	(S1.5)
    ("韋" "⿳口帀") ; [152-8] 衛-衞
    ("庶" "庻" "⿸广⿱龷灬" "⿸广⿱龷从") ; [164]

    ("小" "⺌") ; [16] 尙-尚
    ("巛" "⺌" "⺍") ; [171] 䎩

    ("馬" "⿹廾") ; [173]
    ("疌" "⿳⺊⺕龰") ; [178-3] 婕-媫, [蜨-蜨]

    ("㕣" "⿱几口") ; [189] 兖-兗, 滚-滾, 衮-袞, [沿-沿]CT
    ("寽" "⿳爫丿寸") ; [192] 埒-埓
    ("蒙" "⿱冡") ; [懞-懞]
    ("匈" "⿹勹⿺𠃊㐅") ; 㕼[GJ], 𥑪-𥒚 詾-𧦷は矛盾
    ("切" "⿰土刀") ; [切-切]

    ("丰" "𠦂") ; [翺-翺]
    ("𠦂" "⿱⿲𠄠丨𠄠十") ; [110a] 噑-噑
    ("直" "⿱匕⿺𠃊目") ; [埴-埴]
    ("果" "⿻𦥑木") ; [巢-巢]

    ("荆" "荊") ; [181]
    ("臥" "卧") ; [187b]
    ("擧" "𦦙") ; [188]
    ("舍" "舎") ; [194]

    ("了" "𠄎") ;
    ("乙" "乚") ;
    ("囧" "⿴囗㕣") ; [㴄]CT
    ("大" "⿱一八") ; 𠨩
    ("夾" "⿱𠆢") ; 𣷚
    ("荒" "⿱艹㐬") ; 慌

    ("二" "冫") ; [33] 勻-匀, [次]CJ
    ("冫" "") ; 弱,𢏒𢏻𢏽𦸹

    ("匁" "⿹勹㐅") ;
    ("凢" "⿱一几") ; D35222.0

    ("吅" "⿴口丨") ;
    ("會" "⿳亼田日") ;
    ("" "𠂡" "⿵𠘨氺") ;
    ("𦲸" "⿱廿𠕒" "⿱廿雨") ;
    ("𣎵" "⿻屮八") ;
    ))

(provide 'ids-equiv)
;;; ids-equiv.el ends here
