長庚大學 大數據分析方法 作業三
================

網站資料爬取
------------

``` r
library(rvest)
```

    ## Warning: package 'rvest' was built under R version 3.3.3

    ## Loading required package: xml2

    ## Warning: package 'xml2' was built under R version 3.3.3

``` r
PTTMOVIE1<-"https://www.ptt.cc/bbs/movie/index5271.html"
PTTMOVIE2<-"https://www.ptt.cc/bbs/movie/index5272.html"
PTTMOVIE3<-"https://www.ptt.cc/bbs/movie/index5273.html"
PTTMOVIE4<-"https://www.ptt.cc/bbs/movie/index5274.html"
PTTMOVIE5<-"https://www.ptt.cc/bbs/movie/index5275.html"

url<-c(PTTMOVIE1,PTTMOVIE2,PTTMOVIE3,PTTMOVIE4,PTTMOVIE5)

totalPage<-NULL
for(i in url){
  tempPage<-{PTTContent<-read_html(i)
  post_title <- PTTContent %>% 
    html_nodes(".title") %>% 
    html_text()
  post_number<- PTTContent %>% 
    html_nodes(".nrec") %>% 
    html_text()
  post_author<- PTTContent %>% 
    html_nodes(".author") %>% 
    html_text()
  post_title<-gsub("[^[:alnum:]///' ]", "", post_title)
  pttmovie_posts<-data.frame(Title = post_title,PushNum = post_number,Author = post_author)
  }
  totalPage<-rbind(totalPage,tempPage)
}

totalPage
```

    ##                                                      Title PushNum
    ## 1                                            好雷 金剛戰士      26
    ## 2                                            討論 金爆內幕       5
    ## 3                                  極好雷 我和我的冠軍女兒      39
    ## 4                                            普雷 金剛戰士       6
    ## 5   情報艾咪舒默基嫚與史提夫卡爾將主演NY單身日記導演執導喜        
    ## 6                                  本文已被刪除 Balderston       1
    ## 7                                    好無雷 美女與野獸IMAX       8
    ## 8                                 請益 美女與野獸 貝兒衣服       8
    ## 9                                       討論 內褲超人 預告       8
    ## 10                  討論 電影相關YouTube頻道推薦合輯與精選       3
    ## 11                       翻譯 異星智慧兩大男主角超歡樂訪談       3
    ## 12                                     請益 聲之形字幕問題       2
    ## 13              新聞 李銘順為目擊者增胖 范文芳甜說還是很帥       2
    ## 14                    新聞 親友齊聚好萊塢 追憶黛比雷諾母女        
    ## 15                                微好雷 聲之形youtube影評       5
    ## 16                          好雷 月光下的藍色男孩Moonlight       6
    ## 17                      好雷 聲之形 心得  霸凌者與被霸凌者      10
    ## 18                           好雷 無雷短評神奇大隊長聲之形        
    ## 19                            討論 split 分裂 女主角的頭髮        
    ## 20                                   請益 聲之形一些小疑惑      11
    ## 21                                   請益 我和我的冠軍女兒      13
    ## 22               討論 當你把正義聯盟預告片撥給復仇者聯盟看      39
    ## 23                                     好雷 本能寺大飯店         1
    ## 24                                  請益 猜火車2DVD 會出嗎        
    ## 25                                       問片 問一部中文片       2
    ## 26                                 討論 美女與野獸角色問題       8
    ## 27                     好雷 金剛戰士 勵志好電影 但有點失望       5
    ## 28                               好雷 二十一世紀的金剛戰士       7
    ## 29                                討論用鏡頭說故事黑暗騎士       8
    ## 30                       好雷 聲<U+306E>形喜歡自己的重要性      10
    ## 31                      新聞 我和我的冠軍女兒 片名裡的玄機       9
    ## 32                    請益 剛看完我和我的冠軍女兒 有個疑問       7
    ## 33                          好雷那個靜默的陽光午後無語良師       6
    ## 34                              問片 找異形聖約 的一段預告        
    ## 35                新聞 北美週末美女與野獸連冠 金剛戰士居次      38
    ## 36                                                    請益       1
    ## 37                            問片求大神神一部科幻星際電影       3
    ## 38                         討論 那些今年滿20歲的電影二歐澳       6
    ## 39                           新聞 他我也可以王大陸強吻王凱        
    ## 40                 算是好雷 五重點讓美女與野獸帶你重拾感動       9
    ## 41                       好無雷 異星智慧 美女野獸 冠軍女兒       9
    ## 42                   雷 有人從冠軍女兒中看到成棒隊的影子嗎       7
    ## 43                                           好雷 金剛戰士       9
    ## 44                                   本文已被刪除 shinshow       1
    ## 45                                 討論 美女與野獸角色問題      18
    ## 46                        Re 好雷 隧道盡頭隧道的盡頭是光明        
    ## 47                   請益 如果只有明天可以看電影你會推哪部      96
    ## 48                       好雷 六弄咖啡館看的不是電影是人生        
    ## 49                 新聞 艾瑪史東傳記電影性別之戰將再戰獎季       6
    ## 50                       好雷 黑暗騎士 黎明昇起 善良的力量      60
    ## 51                         好雷 本能寺大飯店這一住驚心動魄       6
    ## 52                                            好雷 聲之形        6
    ## 53                                Re 請益 聲之形一些小疑惑       1
    ## 54                  分享 憑借書證到台中萬代福看片只要50元       16
    ## 55                             討論 19852005港台前三名電影      26
    ## 56                     普好雷 攻敵必救憤世嫉俗者的會心一擊       5
    ## 57                        請益 羅賓掛了嗎 after 自殺突擊隊      28
    ## 58                                   本文已被刪除 johnzzsh        
    ## 59                     新聞 景甜好棒棒金剛中國開片大勝北美      24
    ## 60                                   討論 聲之形  你的名字      17
    ## 61                        Re 討論 正義聯盟首支正式預告釋出      38
    ## 62                     Re 請益 羅賓掛了嗎 after 自殺突擊隊      14
    ## 63              Re 贈票 橫掃義大利金獎大導媽媽教我愛的一切       3
    ## 64                                好雷 金剛骷髏島不提景甜       10
    ## 65                  新聞 神片開賣5分鐘內GG 猜火車2加場爭取       6
    ## 66                       請益 女主角是壞人但討人喜歡的電影      80
    ## 67                               問片 在飛機上看到的一部片       7
    ## 68                                   雷 好雷的金剛戰士慎入      15
    ## 69              Re 請益 當初AI人工智慧為何不在前面收尾就好       9
    ## 70                               Fw 閒聊 聲之形 無雷觀後感       5
    ## 71                          贈票 玩命關頭8北中南推文搶先看      爆
    ## 72                   好雷 決戰異世界弒血之戰  狼人  吸血鬼       6
    ## 73                      無雷 我和我的冠軍女兒不爆雷觀後感        5
    ## 74                             分享猜火車2金馬奇幻加開四場      17
    ## 75                      好雷  七月與安生充滿糾結的友誼情感        
    ## 76                      問片農莊主人和姪女不倫情節的北歐片       1
    ## 77                                     本文已被刪除 an5607        
    ## 78                     Re 請益 羅賓掛了嗎 after 自殺突擊隊      19
    ## 79                         普暖雷生日卡片 主役的人不唱悲歌        
    ## 80                      新聞 景甜好棒 金剛中國開片大勝北美       3
    ## 81                                           好雷金剛戰士        8
    ## 82                             Fw 閒聊 聲之形二刷 心得有雷       9
    ## 83                   贈票 湯姆漢克斯大力推薦衝突的一天贈票      77
    ## 84                                  本文已被刪除 psooolder      36
    ## 85                      好雷 美女與野獸 1991  2017場景設計      24
    ## 86                       討論 短心得關於迪士尼公主的數學題       6
    ## 87              選片 明天我要和昨天的你約會 vs我的冠軍女兒      40
    ## 88                           問片 問一部日本片關於合唱團的       8
    ## 89           討論 艾瑪華森與湯姆漢克斯新片直播風暴相關資訊      18
    ## 90                   討論 樂高蝙蝠俠電影東森洋片現在播放中       7
    ## 91                 心得 韓國電影非常警探收賄刑警VS販毒警官       4
    ## 92                         好雷 韓國電影標靶電影介紹與心得       1
    ## 93                           問片 問一部臥底電影的片名有雷       1
    ## 94                 Fw 閒聊 Ghost int the shell系列介紹影片       2
    ## 95                       討論 原本沒興趣但卻意外好看的電影      爆
    ## 96                           討論 珍妮佛勞倫斯是不是過譽了      73
    ## 97                         新聞 蜘蛛人返校日反派禿鷹新情報      14
    ## 98                               請益 出現過這種角色的電影       4
    ## 99                            問片 一部日本片科幻 殺戮都市       7
    ## 100            翻譯 10個得按下暫停才看得到的迪士尼動畫彩蛋       1
    ##           Author
    ## 1      Beanoodle
    ## 2       archilee
    ## 3         rs6677
    ## 4     stocktonty
    ## 5         qpr322
    ## 6              -
    ## 7          kaleo
    ## 8        Keshang
    ## 9        arsl400
    ## 10     zach12348
    ## 11      TVpotato
    ## 12     pkq820116
    ## 13  iam168888888
    ## 14  iam168888888
    ## 15        orzwei
    ## 16       FaLaSol
    ## 17    zxcv820421
    ## 18   kevin781109
    ## 19    kurama1984
    ## 20      juyhnmki
    ## 21       reynaud
    ## 22   hsupohsiang
    ## 23      Aotearoa
    ## 24          loip
    ## 25          mazz
    ## 26    taiwansoul
    ## 27   peter080845
    ## 28        NL0623
    ## 29      cgmagic7
    ## 30        Nianyi
    ## 31       sony577
    ## 32       ejijojo
    ## 33       a122239
    ## 34      wanchu01
    ## 35   lovemelissa
    ## 36      kicker96
    ## 37      shallwei
    ## 38      peter220
    ## 39        DJSHD2
    ## 40      wyatt001
    ## 41   sunnyborage
    ## 42        Daboto
    ## 43         saywa
    ## 44             -
    ## 45     henin2003
    ## 46     devil0915
    ## 47      ooxx2014
    ## 48     TanyaJhan
    ## 49  sampsonlu919
    ## 50   happyyizhen
    ## 51    bestbamboo
    ## 52   JustMissing
    ## 53          nksm
    ## 54     pata20344
    ## 55      handsten
    ## 56       Roy3567
    ## 57          sun8
    ## 58             -
    ## 59        pneumo
    ## 60     aaa457001
    ## 61       yashiro
    ## 62  sunny1991225
    ## 63     indiasosp
    ## 64    BF109Pilot
    ## 65     asd591922
    ## 66     lemon7242
    ## 67  justforfun90
    ## 68         triff
    ## 69          sofc
    ## 70      a4040856
    ## 71        ChloeD
    ## 72       SuperSg
    ## 73         lasor
    ## 74     black99kk
    ## 75     pattichiu
    ## 76     channel79
    ## 77             -
    ## 78    alljerry04
    ## 79       jk10134
    ## 80       blaster
    ## 81      iamandre
    ## 82        gc9987
    ## 83      pelicula
    ## 84             -
    ## 85   mysmalllamb
    ## 86        wu05k3
    ## 87    kenlin0105
    ## 88        exporn
    ## 89    swizzleyeh
    ## 90    amandawang
    ## 91      KpopNote
    ## 92      KpopNote
    ## 93         laggy
    ## 94      Swampert
    ## 95       litann4
    ## 96          g21l
    ## 97      qn123456
    ## 98      carotyao
    ## 99    a128296578
    ## 100     TVpotato

爬蟲結果呈現
------------

``` r
knitr::kable(totalPage)
```

| Title                                                  | PushNum | Author       |
|:-------------------------------------------------------|:--------|:-------------|
| 好雷 金剛戰士                                          | 26      | Beanoodle    |
| 討論 金爆內幕                                          | 5       | archilee     |
| 極好雷 我和我的冠軍女兒                                | 39      | rs6677       |
| 普雷 金剛戰士                                          | 6       | stocktonty   |
| 情報艾咪舒默基嫚與史提夫卡爾將主演NY單身日記導演執導喜 |         | qpr322       |
| 本文已被刪除 Balderston                                | 1       | -            |
| 好無雷 美女與野獸IMAX                                  | 8       | kaleo        |
| 請益 美女與野獸 貝兒衣服                               | 8       | Keshang      |
| 討論 內褲超人 預告                                     | 8       | arsl400      |
| 討論 電影相關YouTube頻道推薦合輯與精選                 | 3       | zach12348    |
| 翻譯 異星智慧兩大男主角超歡樂訪談                      | 3       | TVpotato     |
| 請益 聲之形字幕問題                                    | 2       | pkq820116    |
| 新聞 李銘順為目擊者增胖 范文芳甜說還是很帥             | 2       | iam168888888 |
| 新聞 親友齊聚好萊塢 追憶黛比雷諾母女                   |         | iam168888888 |
| 微好雷 聲之形youtube影評                               | 5       | orzwei       |
| 好雷 月光下的藍色男孩Moonlight                         | 6       | FaLaSol      |
| 好雷 聲之形 心得 霸凌者與被霸凌者                      | 10      | zxcv820421   |
| 好雷 無雷短評神奇大隊長聲之形                          |         | kevin781109  |
| 討論 split 分裂 女主角的頭髮                           |         | kurama1984   |
| 請益 聲之形一些小疑惑                                  | 11      | juyhnmki     |
| 請益 我和我的冠軍女兒                                  | 13      | reynaud      |
| 討論 當你把正義聯盟預告片撥給復仇者聯盟看              | 39      | hsupohsiang  |
| 好雷 本能寺大飯店                                      | 1       | Aotearoa     |
| 請益 猜火車2DVD 會出嗎                                 |         | loip         |
| 問片 問一部中文片                                      | 2       | mazz         |
| 討論 美女與野獸角色問題                                | 8       | taiwansoul   |
| 好雷 金剛戰士 勵志好電影 但有點失望                    | 5       | peter080845  |
| 好雷 二十一世紀的金剛戰士                              | 7       | NL0623       |
| 討論用鏡頭說故事黑暗騎士                               | 8       | cgmagic7     |
| 好雷 聲<U+306E>形喜歡自己的重要性                      | 10      | Nianyi       |
| 新聞 我和我的冠軍女兒 片名裡的玄機                     | 9       | sony577      |
| 請益 剛看完我和我的冠軍女兒 有個疑問                   | 7       | ejijojo      |
| 好雷那個靜默的陽光午後無語良師                         | 6       | a122239      |
| 問片 找異形聖約 的一段預告                             |         | wanchu01     |
| 新聞 北美週末美女與野獸連冠 金剛戰士居次               | 38      | lovemelissa  |
| 請益                                                   | 1       | kicker96     |
| 問片求大神神一部科幻星際電影                           | 3       | shallwei     |
| 討論 那些今年滿20歲的電影二歐澳                        | 6       | peter220     |
| 新聞 他我也可以王大陸強吻王凱                          |         | DJSHD2       |
| 算是好雷 五重點讓美女與野獸帶你重拾感動                | 9       | wyatt001     |
| 好無雷 異星智慧 美女野獸 冠軍女兒                      | 9       | sunnyborage  |
| 雷 有人從冠軍女兒中看到成棒隊的影子嗎                  | 7       | Daboto       |
| 好雷 金剛戰士                                          | 9       | saywa        |
| 本文已被刪除 shinshow                                  | 1       | -            |
| 討論 美女與野獸角色問題                                | 18      | henin2003    |
| Re 好雷 隧道盡頭隧道的盡頭是光明                       |         | devil0915    |
| 請益 如果只有明天可以看電影你會推哪部                  | 96      | ooxx2014     |
| 好雷 六弄咖啡館看的不是電影是人生                      |         | TanyaJhan    |
| 新聞 艾瑪史東傳記電影性別之戰將再戰獎季                | 6       | sampsonlu919 |
| 好雷 黑暗騎士 黎明昇起 善良的力量                      | 60      | happyyizhen  |
| 好雷 本能寺大飯店這一住驚心動魄                        | 6       | bestbamboo   |
| 好雷 聲之形                                            | 6       | JustMissing  |
| Re 請益 聲之形一些小疑惑                               | 1       | nksm         |
| 分享 憑借書證到台中萬代福看片只要50元                  | 16      | pata20344    |
| 討論 19852005港台前三名電影                            | 26      | handsten     |
| 普好雷 攻敵必救憤世嫉俗者的會心一擊                    | 5       | Roy3567      |
| 請益 羅賓掛了嗎 after 自殺突擊隊                       | 28      | sun8         |
| 本文已被刪除 johnzzsh                                  |         | -            |
| 新聞 景甜好棒棒金剛中國開片大勝北美                    | 24      | pneumo       |
| 討論 聲之形 你的名字                                   | 17      | aaa457001    |
| Re 討論 正義聯盟首支正式預告釋出                       | 38      | yashiro      |
| Re 請益 羅賓掛了嗎 after 自殺突擊隊                    | 14      | sunny1991225 |
| Re 贈票 橫掃義大利金獎大導媽媽教我愛的一切             | 3       | indiasosp    |
| 好雷 金剛骷髏島不提景甜                                | 10      | BF109Pilot   |
| 新聞 神片開賣5分鐘內GG 猜火車2加場爭取                 | 6       | asd591922    |
| 請益 女主角是壞人但討人喜歡的電影                      | 80      | lemon7242    |
| 問片 在飛機上看到的一部片                              | 7       | justforfun90 |
| 雷 好雷的金剛戰士慎入                                  | 15      | triff        |
| Re 請益 當初AI人工智慧為何不在前面收尾就好             | 9       | sofc         |
| Fw 閒聊 聲之形 無雷觀後感                              | 5       | a4040856     |
| 贈票 玩命關頭8北中南推文搶先看                         | 爆      | ChloeD       |
| 好雷 決戰異世界弒血之戰 狼人 吸血鬼                    | 6       | SuperSg      |
| 無雷 我和我的冠軍女兒不爆雷觀後感                      | 5       | lasor        |
| 分享猜火車2金馬奇幻加開四場                            | 17      | black99kk    |
| 好雷 七月與安生充滿糾結的友誼情感                      |         | pattichiu    |
| 問片農莊主人和姪女不倫情節的北歐片                     | 1       | channel79    |
| 本文已被刪除 an5607                                    |         | -            |
| Re 請益 羅賓掛了嗎 after 自殺突擊隊                    | 19      | alljerry04   |
| 普暖雷生日卡片 主役的人不唱悲歌                        |         | jk10134      |
| 新聞 景甜好棒 金剛中國開片大勝北美                     | 3       | blaster      |
| 好雷金剛戰士                                           | 8       | iamandre     |
| Fw 閒聊 聲之形二刷 心得有雷                            | 9       | gc9987       |
| 贈票 湯姆漢克斯大力推薦衝突的一天贈票                  | 77      | pelicula     |
| 本文已被刪除 psooolder                                 | 36      | -            |
| 好雷 美女與野獸 1991 2017場景設計                      | 24      | mysmalllamb  |
| 討論 短心得關於迪士尼公主的數學題                      | 6       | wu05k3       |
| 選片 明天我要和昨天的你約會 vs我的冠軍女兒             | 40      | kenlin0105   |
| 問片 問一部日本片關於合唱團的                          | 8       | exporn       |
| 討論 艾瑪華森與湯姆漢克斯新片直播風暴相關資訊          | 18      | swizzleyeh   |
| 討論 樂高蝙蝠俠電影東森洋片現在播放中                  | 7       | amandawang   |
| 心得 韓國電影非常警探收賄刑警VS販毒警官                | 4       | KpopNote     |
| 好雷 韓國電影標靶電影介紹與心得                        | 1       | KpopNote     |
| 問片 問一部臥底電影的片名有雷                          | 1       | laggy        |
| Fw 閒聊 Ghost int the shell系列介紹影片                | 2       | Swampert     |
| 討論 原本沒興趣但卻意外好看的電影                      | 爆      | litann4      |
| 討論 珍妮佛勞倫斯是不是過譽了                          | 73      | g21l         |
| 新聞 蜘蛛人返校日反派禿鷹新情報                        | 14      | qn123456     |
| 請益 出現過這種角色的電影                              | 4       | carotyao     |
| 問片 一部日本片科幻 殺戮都市                           | 7       | a128296578   |
| 翻譯 10個得按下暫停才看得到的迪士尼動畫彩蛋            | 1       | TVpotato     |

解釋爬蟲結果
------------

``` r
nrow(totalPage) 
```

    ## [1] 100

總共有100篇文章標題

``` r
x<-table(totalPage$Author)
sort(x)
```

    ## 
    ##     archilee      arsl400    Beanoodle      FaLaSol     juyhnmki 
    ##            1            1            1            1            1 
    ##        kaleo      Keshang  kevin781109   kurama1984       orzwei 
    ##            1            1            1            1            1 
    ##    pkq820116       qpr322       rs6677   stocktonty    zach12348 
    ##            1            1            1            1            1 
    ##   zxcv820421      a122239     Aotearoa     cgmagic7       DJSHD2 
    ##            1            1            1            1            1 
    ##      ejijojo  hsupohsiang     kicker96         loip  lovemelissa 
    ##            1            1            1            1            1 
    ##         mazz       Nianyi       NL0623  peter080845     peter220 
    ##            1            1            1            1            1 
    ##      reynaud     shallwei      sony577   taiwansoul     wanchu01 
    ##            1            1            1            1            1 
    ##     wyatt001    aaa457001   bestbamboo       Daboto    devil0915 
    ##            1            1            1            1            1 
    ##     handsten  happyyizhen    henin2003  JustMissing         nksm 
    ##            1            1            1            1            1 
    ##     ooxx2014    pata20344       pneumo      Roy3567 sampsonlu919 
    ##            1            1            1            1            1 
    ##        saywa         sun8  sunnyborage    TanyaJhan     a4040856 
    ##            1            1            1            1            1 
    ##   alljerry04    asd591922   BF109Pilot    black99kk      blaster 
    ##            1            1            1            1            1 
    ##    channel79       ChloeD    indiasosp      jk10134 justforfun90 
    ##            1            1            1            1            1 
    ##        lasor    lemon7242    pattichiu         sofc sunny1991225 
    ##            1            1            1            1            1 
    ##      SuperSg        triff      yashiro   a128296578   amandawang 
    ##            1            1            1            1            1 
    ##     carotyao       exporn         g21l       gc9987     iamandre 
    ##            1            1            1            1            1 
    ##   kenlin0105        laggy      litann4  mysmalllamb     pelicula 
    ##            1            1            1            1            1 
    ##     qn123456     Swampert   swizzleyeh       wu05k3 iam168888888 
    ##            1            1            1            1            2 
    ##     TVpotato     KpopNote            - 
    ##            2            2            5

統計出來最多文章數的作者"-"，代表這篇文章被刪除，因此不考慮。所以最多文章數的作者是"iam168888888","TVpotato","KpopNote"

從上面的資料顯示，最近熱門的話題是電影的話題，尤其以美女與野獸、金剛戰士這兩部電影最多。
