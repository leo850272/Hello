1928-1969間，小兒麻痺在美國各州的發生率變化
================


資料前處理
----------

``` r
#讀csv資料
polio<-read.csv("POLIO_Incidence.csv",stringsAsFactors = F)
head(polio)
```

    ##   YEAR WEEK ALABAMA ALASKA ARIZONA ARKANSAS CALIFORNIA COLORADO
    ## 1 1928    1       0      -       0        0       0.17     0.39
    ## 2 1928    2       0      -       0        0       0.15      0.2
    ## 3 1928    3    0.04      -       0        0       0.11        0
    ## 4 1928    4       0      -    0.24     0.11       0.07      0.2
    ## 5 1928    5       0      -    0.24        0       0.32        0
    ## 6 1928    6       0      -       0        0       0.22      0.1
    ##   CONNECTICUT DELAWARE DISTRICT.OF.COLUMBIA FLORIDA GEORGIA HAWAII IDAHO
    ## 1           0        0                    -       0    0.03      -     0
    ## 2           0        0                    -       0       0      -     0
    ## 3        0.06        0                    -       0       -      -     0
    ## 4        0.06        0                    0       0       0      -     0
    ## 5        0.13        0                    0       0       0      -     0
    ## 6           0        0                    0       0       0      -     -
    ##   ILLINOIS INDIANA IOWA KANSAS KENTUCKY LOUISIANA MAINE MARYLAND
    ## 1     0.03    0.03 0.08      0        0         0     0     0.06
    ## 2     0.01    0.03    -   0.22        0      0.05  0.13     0.06
    ## 3     0.03    0.03    -      0        0         0     0        0
    ## 4     0.05    0.12    0      0        0         0     0        0
    ## 5     0.04       0 0.04      0        0         0  0.38     0.12
    ## 6     0.03       0    0      0        0         0     0        0
    ##   MASSACHUSETTS MICHIGAN MINNESOTA MISSISSIPPI MISSOURI MONTANA NEBRASKA
    ## 1          0.14     0.04         0           0     0.03    0.18     0.07
    ## 2          0.14     0.04      0.04           0     0.06       0     0.07
    ## 3          0.07     0.02         0           0     0.03    0.18        0
    ## 4          0.02     0.02         0           0     0.06       0        0
    ## 5          0.02     0.04         0           0        0       0     0.15
    ## 6          0.05     0.06         0           0        0       0     0.07
    ##   NEVADA NEW.HAMPSHIRE NEW.JERSEY NEW.MEXICO NEW.YORK NORTH.CAROLINA
    ## 1      -             -       0.08          0     0.08              0
    ## 2      -             -       0.03          0     0.05           0.03
    ## 3      -             -          0          0     0.03              0
    ## 4      -             0       0.03          0     0.06              0
    ## 5      -             0       0.03       0.48     0.07              0
    ## 6      -             0          0          0     0.03              0
    ##   NORTH.DAKOTA OHIO OKLAHOMA OREGON PENNSYLVANIA RHODE.ISLAND
    ## 1            - 0.02        0   0.64            0            0
    ## 2         0.45    -     0.04   0.43         0.03            0
    ## 3            0 0.06        0   1.07         0.02            0
    ## 4         0.15    0     0.09   0.53         0.02            0
    ## 5            0 0.03        0   0.32            0            0
    ## 6            0 0.05     0.04   0.21         0.04            0
    ##   SOUTH.CAROLINA SOUTH.DAKOTA TENNESSEE TEXAS UTAH VERMONT VIRGINIA
    ## 1           0.06            0      0.04  0.05    0       0        -
    ## 2           0.06            0      0.04  0.04    0       0        -
    ## 3           0.35            0         0     0    0       0        -
    ## 4           0.23            0      0.04  0.05    0       0        -
    ## 5           0.17         0.15         0  0.05    0       0        -
    ## 6           0.06         0.29      0.04     0  0.2       0     0.04
    ##   WASHINGTON WEST.VIRGINIA WISCONSIN WYOMING
    ## 1       0.26          0.06      0.03       0
    ## 2       0.39          0.24      0.03       0
    ## 3       0.13          0.12      0.03       0
    ## 4       0.06          0.12         0       0
    ## 5       0.13          0.06      0.03       0
    ## 6       0.06             0      0.14       0

``` r
#將寬表格轉為長表格，以年(YEAR)/週(WEEK)為基準，各州名column打散變長
library(reshape2)
polio.m<-melt(polio,id.vars = c('YEAR','WEEK'))
head(polio.m)
```

    ##   YEAR WEEK variable value
    ## 1 1928    1  ALABAMA     0
    ## 2 1928    2  ALABAMA     0
    ## 3 1928    3  ALABAMA  0.04
    ## 4 1928    4  ALABAMA     0
    ## 5 1928    5  ALABAMA     0
    ## 6 1928    6  ALABAMA     0

``` r
polio.m[polio.m$value=="-",]$value<-NA #處理缺值,將"-"轉為NA
polio.m$value<-as.numeric(polio.m$value)#將value欄位轉為數字
polio.sumYear<- #各州各年度加總，計算該年度的總發生率
    aggregate(value~YEAR+variable,data=polio.m,FUN=sum,na.rm=F)
head(polio.sumYear)
```

    ##   YEAR variable value
    ## 1 1928  ALABAMA  2.39
    ## 2 1929  ALABAMA  2.25
    ## 3 1930  ALABAMA  2.57
    ## 4 1931  ALABAMA  2.07
    ## 5 1932  ALABAMA  1.38
    ## 6 1933  ALABAMA  1.12

視覺畫呈現
----------

解釋如何選擇圖形種類:因為要呈現發生率，年度，各州的名稱三種資料，x軸放年度，y軸放各州的名字，每個點則放小兒麻痺的發生率，嘗試過其他的圖，發現字體都擠在一起，因此選用Heatmap，能清楚比較每個洲每年的發生率之間的關係

解釋圖形:1955年開始有小兒麻痺疫苗, 所以發生率快速下降，可見疫苗有一定效果

``` r
library(ggplot2)
ggplot(polio.sumYear, aes(YEAR, variable)) + #aes(x,y)
    geom_tile(aes(fill = value),colour = "white")+ #geom_tile: 區塊著色
    geom_vline(xintercept = 1955)+
    scale_fill_gradient(low = "gray",high = "red") #數值低：灰色
```

![](https://github.com/leo850272/Hello/blob/master/Rplot.png)
