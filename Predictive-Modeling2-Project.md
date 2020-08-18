Visual story telling part 1: green buildings
--------------------------------------------

    library(mosaic)
    library(tidyverse)
    library(ggplot2)
    library(readr)
    greenbuilding <- read.csv('/Users/davidcruz/Documents/Predictive Modeling2 Project/Green Buildings/greenbuildings.csv')

    # remove low occupancy buildings
    hist(greenbuilding$leasing_rate)

![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-1-1.png)

    favstats(~leasing_rate, data=greenbuilding)

    ##  min    Q1 median    Q3 max     mean       sd    n missing
    ##    0 77.85  89.53 96.44 100 82.60637 21.38031 7894       0

    greenbuilding <- greenbuilding %>%
      filter(leasing_rate > 10)

I wanted to check out the staff’s analysis first before discussing my
own analysis and what I think he missed. As stated in his
recommendation, there is a notable group of building that had very low
occupancy rate(less than 10%). I think he was right in removing these
buildings from this analysis with the given reason, so I will also
remove them.\*

    # separate green and non-green buildings
    tally(~green_rating, data=greenbuilding)

    ## green_rating
    ##    0    1 
    ## 6995  684

    ggplot(data=greenbuilding) + 
      geom_point(mapping=aes(x=cluster_rent, y=Rent, colour=green_rating, alpha=0.8)) +
      labs(x="Cluster Rent", y='Rent', title = 'Green Buildings: Cluster Rent VS Rent',
           color='Green Buildings')

![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    green <- greenbuilding %>%
      filter(green_rating == '1') 
    non_green <- greenbuilding %>%
      filter(green_rating == '0') 

    favstats(~Rent, data=green)

    ##   min      Q1 median    Q3    max     mean       sd   n missing
    ##  8.87 21.4975   27.6 35.54 138.07 30.02848 12.95545 684       0

    hist(green$Rent)

![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-2-2.png)

    favstats(~Rent, data=non_green)

    ##   min    Q1 median    Q3 max     mean       sd    n missing
    ##  2.98 19.43  25.03 34.18 250 28.44478 15.32829 6995       0

    hist(non_green$Rent)

![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-2-3.png)

    hist(green$leasing_rate)

![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-2-4.png)

    favstats(~leasing_rate, data=green)

    ##    min      Q1 median      Q3 max     mean       sd   n missing
    ##  12.39 85.4525 92.925 97.7025 100 89.41243 11.82425 684       0

> The distributions of rent are skewed for both green and non\_green
> buildings, so he was right in considering the median rent value. The
> median rent for green buildings is $27.6, and the median rent for
> non\_green buildings is $25.03. Green buildings are, on average, $2.57
> more per square foot. Generally speaking, since our building would be
> 250,000 square feet, we could earn about $642,500 more with a green
> building than with a non-green building. Considering the median
> leasing rate for green buildings, 92.92%, we could recuperate the
> baseline construction and green certification costs ($5m) in about
> 8.37 years through this revenue. However, this conclusion does not
> take into account other variables that could impact our profit (other
> factors that impacts rent).\*

    favstats(~size, data=green)

    ##    min     Q1 median       Q3     max     mean       sd   n missing
    ##  10560 120000 241199 417449.2 1721242 325965.2 289945.2 684       0

    favstats(~size, data=non_green)

    ##   min    Q1 median     Q3     max     mean       sd    n missing
    ##  2378 48873 123250 285000 3781045 231007.2 299636.6 6995       0

    ggplot(data=greenbuilding) + 
      geom_point(mapping=aes(x=size, y=Rent, colour=green_rating, alpha=0.8)) +
      labs(x="Size", y='Rent', title = 'Green Buildings: Size VS Rent',
           color='Green Buildings')

![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    favstats(~age, data=green)

    ##  min Q1 median Q3 max     mean       sd   n missing
    ##    0 18     22 26 116 23.88012 15.55513 684       0

    favstats(~age, data=non_green)

    ##  min Q1 median Q3 max     mean       sd    n missing
    ##    0 24     36 80 187 49.30808 32.46818 6995       0

    ggplot(data=greenbuilding) + 
      geom_point(mapping=aes(x=age, y=Rent, colour=green_rating, alpha=0.8)) +
      labs(x="Building Age", y='Rent', title = 'Green Buildings: Age VS Rent',
           color='Green Buildings')

![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-3-2.png)

    favstats(~Electricity_Costs, data=green)

    ##     min     Q1 median     Q3    max       mean          sd   n missing
    ##  0.0178 0.0235 0.0341 0.0378 0.0628 0.03158567 0.007819872 684       0

    favstats(~Electricity_Costs, data=non_green)

    ##         min         Q1    median         Q3        max       mean          sd
    ##  0.01781946 0.02330012 0.0327374 0.03780774 0.06277843 0.03089267 0.008592756
    ##     n missing
    ##  6995       0

    ggplot(data=greenbuilding) + 
      geom_point(mapping=aes(x=Electricity_Costs, y=Rent, colour=green_rating, alpha=0.8)) +
      labs(x="Electricity Costs", y='Rent', title = 'Green Buildings: Electricity Costs VS Rent',
           color='Green Buildings')

![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-3-3.png)

    tally(~net, data=green)

    ## net
    ##   0   1 
    ## 645  39

    tally(~net, data=non_green)

    ## net
    ##    0    1 
    ## 6761  234

    hist(green$net)

![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-3-4.png)

    green_similar <- green %>%
      filter(size <= 300000 & size >= 200000) %>%
      filter(age<=24) %>%
      filter(net == 0)
    nongreen_similar <- non_green %>%
      filter(size <= 300000 & size >= 200000) %>%
      filter(age<=24) %>%
      filter(net == 0)

    favstats(~Rent, data=green_similar)

    ##    min   Q1 median   Q3 max     mean       sd  n missing
    ##  13.81 25.2  33.36 38.4  84 32.87612 11.01452 85       0

    favstats(~Rent, data=nongreen_similar)

    ##  min Q1 median    Q3  max    mean       sd   n missing
    ##  9.1 24     35 42.89 89.7 34.6883 13.15785 300       0

    hist(green_similar$Rent)

![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-3-5.png)

    hist(nongreen_similar$Rent)

![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-3-6.png)

    green_similar_nocover <- green %>%
      filter(size <= 300000 & size >= 200000) %>%
      filter(age<=24) %>%
      filter(net == 1)
    nongreen_similar_nocover <- non_green %>%
      filter(size <= 300000 & size >= 200000) %>%
      filter(age<=24) %>%
      filter(net == 1)
    favstats(~Rent, data=green_similar_nocover)

    ##   min      Q1 median      Q3   max     mean       sd n missing
    ##  17.5 19.5675  20.74 22.0925 27.63 21.38833 3.484867 6       0

    favstats(~Rent, data=nongreen_similar_nocover )

    ##    min    Q1 median   Q3   max     mean       sd n missing
    ##  15.42 19.43  23.21 33.5 33.54 24.63556 7.043623 9       0

It seems that the proportion of green to non-green buildings changes
with size, there is more smaller building that are non-green and there
are more green buildings as size increases. The changes with building
age as well. Green building are generally newer than non-green
buildings. I had expected green buildings to have less electricity costs
than non-green building, because one of the attractions of them is to
cut recurring costs. On average green buildings’ elevtrivity costs is
higher than non-green buildings. This is a troublesome discovery, since
a lot of buildings (both green and non-green) have utilities included in
rent price. So let’s take a look at green and non-green buildings with
similar conditions as us. I’m only considering the buildings between
200,000 and 300,000 square feet, under the age 24 (Q1), and have
utilities covered by rent. Now that we are only considering buildings
with similar coniditions as ours, we see that green building’s rent is
cheaper than non-green buildings on average. Even if we do not cover
utilities, the rent for green buildings are still less than non-gree
buildings. This suggests we might earn less than non-green buildings
with this project. Thus, investing in a green building woudl not be
worth it.\*

**Visual Story Telling: Part 2-Flights at ABIA**
------------------------------------------------

### **Visualizations**

**Exploratory Data Analysis**
-----------------------------

Exploratory data analysis of ‘flight’ data during the year 2008 at
Austin airport. Data set contains ~99k records of flight data

![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-8-1.png)![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-8-2.png)

Based on the histograms of arrival and departure delays, the mean delays
of both are centered around zero with large skews in both towards
delays. Based on the histograms there are a few instances where
departures and delays are early, I thought this was interesting
considering that I was unaware flights were able to depart early besides
everyone arriving early.

Next step taken was to look at the correlation between arrival and
departure delays to observe if any particular carrier is deviating from
normal behavior. This will be done by looking at a distribution plot
with proper color coding.
![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-9-1.png)

There are outliers for some of the carriers when looking at the
distribution plot. Almost perfect correlation between the arrival and
departure delays suggesting a linear relationship. Some carriers did
compensate for the departure delays(going really really fast) were
outliers.

**Air carrier operation at Austin Airport**
-------------------------------------------

Next is to observe the carrier operation at Austin Airport.

![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-10-1.png)

Southwest(WN) tops the list with almost 40k operations, followed by
Alaskan Airlines(AA).

What is the most reliable carrier?

Probability(carrier delay &gt; 30 mins): 1. Lowest: Southwest(WN) and
Frontier Airlines(F9) 2. YV and 9E &gt; 60%.

Summary of Reliable Carriers: Southwest(WN) is the most reliable with
40k operations suggesting the results are reliable. The avg carrier
delay is 18 minutes. Avg departure delay is less than avg arrival delay.
The airlines – F9 MQ, US, WN and XE &gt; 30% probability of delay &gt;30
min, but they have a low number of operations unlike Southwest.
Relatively, Alaskan Airlines(AA) which has ~20k operations outperforms
many carriers that have less operations.

Unreliable Carriers: 1. 9E and YV have an avg carrier delay &gt; 1hr. 2.
With just 121 operations, NW has a high avg carrier delay of 48 minutes.

![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-13-1.png)![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-13-2.png)

**Portfolio Modeling**
----------------------

The ETFs selected were SPY, QQQ, DIA, and GLD. SPY follows the S&P 500
which are large cap stocks, QQQ follows the NASDAQ which is full of tech
heavy stocks, DIA follows the DJIA which includes Apple, and GLD follows
gold which is typically used as a hedge during times of market
instability.

**Data importing from the interweb and processing**
---------------------------------------------------

![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-17-1.png)![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-17-2.png)![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-17-3.png)![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-17-4.png)

    ##                ClCl.SPYa     ClCl.QQQa    ClCl.DIAa    ClCl.GLDa
    ## 2015-07-01            NA            NA           NA           NA
    ## 2015-07-02 -0.0009156723  0.0002779744 -0.001972892 -0.001964645
    ## 2015-07-06 -0.0028459650 -0.0023158870 -0.002089862  0.002684288
    ## 2015-07-07  0.0062887142  0.0025070010  0.005377021 -0.011600893
    ## 2015-07-08 -0.0167772567 -0.0174122903 -0.014806085  0.002979361
    ## 2015-07-09  0.0018090011 -0.0005655293  0.001485686  0.002430507

![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-19-1.png)![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-19-2.png)![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-19-3.png)![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-19-4.png)
The ditrbutions plotted in the first graph make sense as the market has
been doing well the past five years overall so there is a linear trend.
The plots comparing with GLD also make sense as even right now GLD is
being used as a hedge against inflation and has reached all time highs.
There is no correlation between day to day returns.

![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-20-1.png)

The graphs show that returns are uncorrelated from one day to the next
which is important as they would be easy to exploit by looking at past
information suggesting a weak form market efficiency.

**Setup**
---------

![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-21-1.png)
Verifying data makes sense again for a single day.

**Sample Simulation **
----------------------

$100000 to invest with equal weighting in each Update the value of
holdings Assumes an equal allocation to each asset

    ##            ClCl.SPYa ClCl.QQQa ClCl.DIAa ClCl.GLDa
    ## 2015-09-10  20108.84   20217.6  20107.06  20047.11

    ## [1] 80480.61

Lost about $18500.

Now loop over four trading weeks let’s run the following block of code 5
or 6 times to eyeball the variability in performance trajectories

    ## [1] 78640.4

![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-23-1.png)
Lost $15000 this time.

\*\*Simulations with Boorstrapping
----------------------------------

**Simulation 1**
----------------

$100000 to invest with equal weighting in each Update the value of
holdings Assumes an equal allocation to each asset

    ##               [,1]      [,2]      [,3]      [,4]      [,5]     [,6]     [,7]
    ## result.1 100168.42 100019.81  99224.76  98573.71  99206.69 100570.0 100717.5
    ## result.2 100166.42 100316.33 100212.29 100697.02 102331.45 102107.1 100911.6
    ## result.3  99647.64 100631.57 101756.09 101871.28 101922.47 102327.3 101651.6
    ## result.4  99888.68  99078.62 100235.22 100122.56 100398.01 100407.6 100564.7
    ## result.5 101143.59 101503.43 101711.74 100521.65 101480.58 101470.3 101518.9
    ## result.6  99976.26 100733.97 100888.07 100634.59 101238.53 101716.0 102419.9
    ##               [,8]      [,9]     [,10]     [,11]    [,12]     [,13]     [,14]
    ## result.1  99205.16  99606.51  99657.46  99974.18 100199.9  99822.00 100431.89
    ## result.2 102035.77 102471.12  98195.05  97941.63  97442.4  97812.32  98258.10
    ## result.3 101082.74 101160.17 102508.13 102056.41 102706.5 103025.59 103147.62
    ## result.4 101004.26 100828.79 100989.72 101035.76 100288.0  98991.85  98966.12
    ## result.5 102734.34 102516.75 103769.00  99429.20  99731.8  99347.42  98597.08
    ## result.6 102052.18 102146.98 102516.05 102811.86 102770.7 102680.15 103986.64
    ##              [,15]     [,16]     [,17]     [,18]     [,19]     [,20]
    ## result.1 100429.81 102645.28 103247.99 103820.93 103681.37 106378.50
    ## result.2  98581.56  98246.64  98250.75  99703.32  99947.55  99477.22
    ## result.3  98454.42  98489.49  96094.63  96375.14  96862.54  99249.38
    ## result.4  98884.64  99977.28 100092.89 100546.11 100592.41 100599.34
    ## result.5  98675.95  98906.15  99209.56  99291.01  98789.73  98780.88
    ## result.6 104799.09 104851.73 113483.27 113279.49 113995.38 114567.46

![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-25-1.png)

    ## [1] 101155.7

    ## [1] 1155.7

    ## [1] 0.011557

![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-25-2.png)

    ##        5% 
    ## -5718.214

The expected profit is $1270.42 and a return of 1.27% which shows some
return. The 5% VaR = 5744.82 so thats how much they can expect to lose
at the 5% quantile performance.

**Simulation 2**
----------------

$100000 to invest with aggressive weighting in SPY, QQQ, DIA and nothing
in GLD. Update the value of holdings.

    ##               [,1]      [,2]      [,3]      [,4]      [,5]      [,6]      [,7]
    ## result.1 100307.00 101020.44 101173.22 101597.49 101220.94 103130.66 102758.62
    ## result.2  99753.60  99646.79 100257.67  99870.39  99281.21  98746.94  99394.40
    ## result.3 100834.64 100703.37  99880.95 100364.09  96120.15  96327.72  96230.78
    ## result.4  98878.22  99217.57 100638.97 101834.15 102402.22 105015.76 105995.50
    ## result.5 101239.98 102680.62 102918.70 103121.54 103240.40 102302.61  99617.31
    ## result.6 100070.19 100781.41 101268.51  98664.76  99203.77  98842.58  98459.07
    ##               [,8]      [,9]     [,10]     [,11]     [,12]     [,13]     [,14]
    ## result.1 102826.91 102887.22 102678.76 103673.28 104982.16 104619.72 101858.50
    ## result.2  99320.91  99309.48 100274.44 100552.38 101034.44 101571.30 101910.63
    ## result.3  96223.93  95393.21  94643.73  94959.05  94838.19  94851.43  95598.84
    ## result.4 105934.62 105993.48 106167.53 106128.35 106867.05 106734.95 106808.40
    ## result.5  99434.73  99326.26  99533.25  99803.57  92276.07  92627.92  93166.54
    ## result.6  99926.00  98005.39  98618.91  97797.70  98786.38  98474.88  95727.79
    ##              [,15]     [,16]     [,17]     [,18]     [,19]     [,20]
    ## result.1  98801.67 100013.56 100406.53  99448.34  99738.63 105474.97
    ## result.2 101724.12 100612.98 101029.91 101212.56 101456.06  99995.23
    ## result.3  95727.14  95149.97  95140.18  94245.34  94285.11  94744.55
    ## result.4 107424.69 106110.94 112216.56 112574.02 113154.70 113520.13
    ## result.5  94474.75  95831.61  96009.55  96592.40  97439.46  97469.80
    ## result.6  95053.62  95058.60  95233.51  96066.87  96416.64  95471.85

![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-27-1.png)

    ## [1] 101134.2

    ## [1] 1134.224

    ## [1] 0.01134224

![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-27-2.png)

    ##        5% 
    ## -8052.164

The expected profit is 1283.11 and a return of 1.28% over 4 weeks and
this produces returns which appear to be normal based on the plots which
could be due to normal variability in the market since that is where all
the weights are. The 5% VaR = 7726.34 so thats how much they can expect
to lose at the 5% quantile performance. This VaR is higher than in
simulation 1 meaning it is rskier. This isn’t bad but there is no gold
hedge so when times are tough the portfolio will likely take a huge fall
since there is full exposure to the market but when times are good it
will likely perform better.

**Simulation 3**
----------------

$100000 to invest in mostly weighting in GLD and less in SPY, QQQ, DIA.
Update the value of holdings

    ##               [,1]      [,2]      [,3]      [,4]      [,5]      [,6]      [,7]
    ## result.1  99410.15 100103.55  99749.53  98687.87  98753.58  98632.30  98676.76
    ## result.2 100550.42 101314.56 100836.80 100815.47 100189.67  99935.25  99792.32
    ## result.3  99923.59 100443.09 100717.64 100724.37 100422.37 101052.84 100743.07
    ## result.4  99651.28  99623.19 100048.27 100501.79  99617.66  99297.39  98983.33
    ## result.5 100739.43 100103.12  99887.17  99640.02  99432.61  99052.90  98741.20
    ## result.6  99725.25 100482.47 100409.01 100758.90 101568.53 101438.36 101571.53
    ##               [,8]      [,9]     [,10]     [,11]     [,12]     [,13]     [,14]
    ## result.1  98653.36  98467.64  98786.42  98528.78  98007.45  97519.85  97346.87
    ## result.2 100161.18 100696.95 100350.37 100456.36 100565.84 100575.38 100662.77
    ## result.3 100355.69  99831.37  99690.53  99656.87 100268.97  99783.23  98992.68
    ## result.4  98227.56  97364.99  97799.74  97230.76  97586.42  98033.76  97443.50
    ## result.5  99962.56  99286.89  99848.58  99821.02 100771.79 101080.55 101731.75
    ## result.6 101538.09 101647.31 102039.53 101460.36 101375.03 102010.96 101951.40
    ##              [,15]     [,16]     [,17]     [,18]     [,19]     [,20]
    ## result.1  97159.65  91731.94  91979.30  90390.01  90417.78  90073.70
    ## result.2 100643.70 100375.70 100057.34 100270.03 100626.56 101350.96
    ## result.3  99352.89  99129.21  99094.00  98403.73  98680.11  97561.84
    ## result.4  97858.99  98120.08  98169.07 100914.33 101280.24 102052.80
    ## result.5 102333.85 102178.96 102836.89 102574.23 103019.88 103801.64
    ## result.6 101930.70 100883.76 101141.38 102393.33 102556.65 102419.36

![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-29-1.png)

    ## [1] 101007

    ## [1] 1006.962

    ## [1] 0.01006962

![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-29-2.png)

    ##        5% 
    ## -4007.815

The expected profit is $932.78 and a return of 0.93% over 4 weeks and
this produces returns which appear to be normal based on the plots which
could be due to normal variability in the market since that is where all
the weights are. The 5% VaR = 4244.35 so thats how much they can expect
to lose at the 5% quantile performance. This VaR is lower than in
simulation 1 meaning it performs better. This still has investments in
the market but is much more stable and less risky becasue of the
investments in gld as a hedge but slightly lower investments.

Market Segmentation
-------------------

**Data Processing and the such**
--------------------------------

    ##  [1] 244311.0 222796.5 209096.5 197541.5 187691.6 179608.8 172624.6 165830.9
    ##  [9] 160933.2 156969.4 153538.4 150309.6 148321.7 146424.0 144523.7

![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-32-1.png)

![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-34-1.png)
Ugly clustering over the data.

<img src="Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-36-1.png" width="50%" /><img src="Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-36-2.png" width="50%" /><img src="Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-36-3.png" width="50%" /><img src="Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-36-4.png" width="50%" /><img src="Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-36-5.png" width="50%" /><img src="Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-36-6.png" width="50%" /><img src="Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-36-7.png" width="50%" /><img src="Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-36-8.png" width="50%" /><img src="Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-36-9.png" width="50%" /><img src="Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-36-10.png" width="50%" />

**Market segmentation**
-----------------------

K-means using PCA data

Five clusters was the most ideal after testing and five variables were
removed including spam, chatter and adult.

#### **Correlation plot**

![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-38-1.png)

Many variables are correlated. some examples include: personal fitness
and health nutrition,and online gaming and college university variables
have a high correlation.

#### **Principal Component Analysis**

PCA will be used to reduce the dimensions to create fewer uncorrelated
variables.

    ## Importance of components:
    ##                           PC1     PC2     PC3     PC4     PC5    PC6     PC7
    ## Standard deviation     2.0739 1.62224 1.57567 1.47152 1.29814 1.2770 1.19334
    ## Proportion of Variance 0.1387 0.08489 0.08009 0.06985 0.05436 0.0526 0.04594
    ## Cumulative Proportion  0.1387 0.22363 0.30372 0.37357 0.42793 0.4805 0.52647
    ##                           PC8     PC9    PC10    PC11    PC12    PC13    PC14
    ## Standard deviation     1.0968 1.05760 1.00154 0.96661 0.95607 0.93754 0.93123
    ## Proportion of Variance 0.0388 0.03608 0.03236 0.03014 0.02949 0.02835 0.02797
    ## Cumulative Proportion  0.5653 0.60136 0.63372 0.66386 0.69334 0.72170 0.74967
    ##                           PC15    PC16    PC17    PC18    PC19   PC20   PC21
    ## Standard deviation     0.90714 0.89512 0.83291 0.80770 0.75366 0.6953 0.6704
    ## Proportion of Variance 0.02655 0.02585 0.02238 0.02104 0.01832 0.0156 0.0145
    ## Cumulative Proportion  0.77621 0.80206 0.82444 0.84548 0.86381 0.8794 0.8939
    ##                           PC22    PC23   PC24    PC25    PC26   PC27    PC28
    ## Standard deviation     0.65360 0.64035 0.6323 0.61717 0.59883 0.5945 0.55315
    ## Proportion of Variance 0.01378 0.01323 0.0129 0.01229 0.01157 0.0114 0.00987
    ## Cumulative Proportion  0.90768 0.92091 0.9338 0.94609 0.95766 0.9691 0.97893
    ##                           PC29    PC30    PC31
    ## Standard deviation     0.48605 0.47625 0.43602
    ## Proportion of Variance 0.00762 0.00732 0.00613
    ## Cumulative Proportion  0.98655 0.99387 1.00000

![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-39-1.png)

Based on the Kaiser criterion, drop principal components with eigen
values less than 1.0.
![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-40-1.png)

    cumsum(pca_var1)[10]

    ## [1] 0.6337156

63.37% of the variation is explained using 10 princial components.

#### **K-Means**

    ## Warning: did not converge in 10 iterations

    ## Warning: did not converge in 10 iterations

![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-45-1.png)

Hard to determine the number of clusters so we are using k=5 and see how
it works.

#### **Cluster visualization**

![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-48-1.png)

The clusters have clear boundaries.

Cluster character ID

#### **Results**

<img src="Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-50-1.png" width="50%" /><img src="Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-50-2.png" width="50%" /><img src="Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-50-3.png" width="50%" /><img src="Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-50-4.png" width="50%" /><img src="Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-50-5.png" width="50%" />

**Market segments identified** Using K-Means clustering, we can identify
unique market segments that NutrientH20 can make use of to advertise
better.

1.  Sports Fandom, Travel, Cooking, politics, College Uni Cluster 1 is
    younger people with extroverted interests.

2.  Travel, current events, outdoors, Business, cooking, college Uni
    Cluster 2 has people who are in college, athletic, and finance
    oriented.

3.  TV Film, Automotive, Politics, religion Cluster 3 seems like people
    who are older, more conservative.

4.  Crafts, current events, politics, food, cooking Cluster 4 - seems
    like people who are into artistic stuff.

5.  Current Events, cooking, food, college uni Cluster 5 - college age
    people who stay indoors but want to maintain healthy intakes.

**Association rule mining**
---------------------------

    ## [1] "citrus fruit,semi-finished bread,margarine,ready soups"             
    ## [2] "tropical fruit,yogurt,coffee"                                       
    ## [3] "whole milk"                                                         
    ## [4] "pip fruit,yogurt,cream cheese ,meat spreads"                        
    ## [5] "other vegetables,whole milk,condensed milk,long life bakery product"
    ## [6] "whole milk,butter,yogurt,rice,abrasive cleaner"

**Processing, exploratory analysis**
------------------------------------

Transform data into ‘transactions’ class

    ## transactions as itemMatrix in sparse format with
    ##  9835 rows (elements/itemsets/transactions) and
    ##  169 columns (items) and a density of 0.02609146 
    ## 
    ## most frequent items:
    ##       whole milk other vegetables       rolls/buns             soda 
    ##             2513             1903             1809             1715 
    ##           yogurt          (Other) 
    ##             1372            34055 
    ## 
    ## element (itemset/transaction) length distribution:
    ## sizes
    ##    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16 
    ## 2159 1643 1299 1005  855  645  545  438  350  246  182  117   78   77   55   46 
    ##   17   18   19   20   21   22   23   24   26   27   28   29   32 
    ##   29   14   14    9   11    4    6    1    1    1    1    3    1 
    ## 
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   2.000   3.000   4.409   6.000  32.000 
    ## 
    ## includes extended item information - examples:
    ##             labels
    ## 1 abrasive cleaner
    ## 2 artif. sweetener
    ## 3   baby cosmetics

Translation of summary results: There are 9835 transactions in our
dataset. Whole milk, vegetables, buns, soda, and yogurt are the most
frequently bought items. Based on the median, half of the transactions
contain 3 or less items and those items are more than likely going to be
milk, vegetables, buns, or soda.

![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-55-1.png)

#### **Networks**

support &gt; 0.04, confidence &gt; 0.1 and length &lt;= 2

    ##      lhs                   rhs                support    confidence coverage 
    ## [1]  {tropical fruit}   => {whole milk}       0.04229792 0.4031008  0.1049314
    ## [2]  {whole milk}       => {tropical fruit}   0.04229792 0.1655392  0.2555160
    ## [3]  {root vegetables}  => {other vegetables} 0.04738180 0.4347015  0.1089985
    ## [4]  {other vegetables} => {root vegetables}  0.04738180 0.2448765  0.1934926
    ## [5]  {root vegetables}  => {whole milk}       0.04890696 0.4486940  0.1089985
    ## [6]  {whole milk}       => {root vegetables}  0.04890696 0.1914047  0.2555160
    ## [7]  {soda}             => {whole milk}       0.04006101 0.2297376  0.1743772
    ## [8]  {whole milk}       => {soda}             0.04006101 0.1567847  0.2555160
    ## [9]  {yogurt}           => {other vegetables} 0.04341637 0.3112245  0.1395018
    ## [10] {other vegetables} => {yogurt}           0.04341637 0.2243826  0.1934926
    ## [11] {yogurt}           => {whole milk}       0.05602440 0.4016035  0.1395018
    ## [12] {whole milk}       => {yogurt}           0.05602440 0.2192598  0.2555160
    ## [13] {rolls/buns}       => {other vegetables} 0.04260295 0.2316197  0.1839349
    ## [14] {other vegetables} => {rolls/buns}       0.04260295 0.2201787  0.1934926
    ## [15] {rolls/buns}       => {whole milk}       0.05663447 0.3079049  0.1839349
    ## [16] {whole milk}       => {rolls/buns}       0.05663447 0.2216474  0.2555160
    ## [17] {other vegetables} => {whole milk}       0.07483477 0.3867578  0.1934926
    ## [18] {whole milk}       => {other vegetables} 0.07483477 0.2928770  0.2555160
    ##      lift      count
    ## [1]  1.5775950 416  
    ## [2]  1.5775950 416  
    ## [3]  2.2466049 466  
    ## [4]  2.2466049 466  
    ## [5]  1.7560310 481  
    ## [6]  1.7560310 481  
    ## [7]  0.8991124 394  
    ## [8]  0.8991124 394  
    ## [9]  1.6084566 427  
    ## [10] 1.6084566 427  
    ## [11] 1.5717351 551  
    ## [12] 1.5717351 551  
    ## [13] 1.1970465 419  
    ## [14] 1.1970465 419  
    ## [15] 1.2050318 557  
    ## [16] 1.2050318 557  
    ## [17] 1.5136341 736  
    ## [18] 1.5136341 736

![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-57-1.png)

here are 18 rules generated. Those numbers were used by trial and error
to get results we could look at visually and interpret. min length 2 is
ideal or we would have a monstrous plot with hundreds of rules. It is
obvious most relationships in this item set include whole milk, yogurt,
buns, and soda which matches what was predicted earlier when looking at
the summary results and the item frequency plot.

Decrease support & increase confidence network with support &gt; 0.02,
confidence &gt; 0.2 and length &lt;= 2

![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-59-1.png)

72 rules were generated and includes a lot more items. A graph with 10
rules was displayed as it was the easiest to look at as there are lot
more paths than before due loosening the conditions. Whole milk and
vegetables are still the most common.

Decrease the support & increase confidence level again network with
support &gt; 0.0015, confidence &gt; 0.8 and length &lt;= 2

![](Predictive-Modeling2-Project_files/figure-markdown_strict/unnamed-chunk-61-1.png)

60 rules were generated with 153 items with way more paths than before,
one of the reasons the graph with 10 rules displayed was chosen.
Vegetables and whole milk are still top contenders.

#### **Summary**

Given that the last network had the roadest parameters, it gave us the
most information. Looking at the association rules we can infer: People
are more likely to buy bottled beer if they purchased red wine or liquor
with confidence of 0.9. If people buy flour, root vegetables, and
whipped/sour cream they will buy whole milk with confidence of 1! They
should probably put some type of stand with those items next to the
dairy section at HEB. lol Whole milk and vegetables are the most common
items purchased by customers in each network explored.
