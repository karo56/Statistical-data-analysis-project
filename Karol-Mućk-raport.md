raport SAD projekt
================
Karol Mućk 394362
4 05 2020

``` r
#Wczytanie danych
url <- "https://www.mimuw.edu.pl/~szczurek/SAD1/ZadanieZaliczeniowe/people.tab"

df <- read.table(url, sep = '\t',header = TRUE)
```

``` r
head(df)
```

    ##   age weight height gender married number_of_kids      pet   expenses
    ## 1  25   61.7 121.12  other   FALSE              2   ferret   23.44299
    ## 2  37   63.9 145.00    man    TRUE              6      dog   96.83683
    ## 3  41   50.2 145.03  woman    TRUE              2 hedgehog  312.67693
    ## 4  43   72.4 179.90    man   FALSE              1      dog  447.42838
    ## 5  26   78.4 163.91    man   FALSE              1 hedgehog  -78.22799
    ## 6  49   59.4 151.86  woman    TRUE              2   ferret 1241.98263

W projekcie mamy do dyspozycji dane opisujące 8 różnych zmiennych. Pięć
z nich jest zmiennymi ilościowymi: wiek, wzrost, waga, liczba dzieci
oraz wydatki, natomiast trzy z nich są zmiennymi jakoświowymi: płeć,
stan cywilny, posiadane zwierzę domowe. Łącznie mamy 500 obserwacji. Aby
przyjrzeć się bliżej danym użyjemy funkcji summary.

``` r
summary(df)
```

    ##       age            weight           height         gender         
    ##  Min.   :17.00   Min.   : 19.40   Min.   :113.6   Length:500        
    ##  1st Qu.:33.00   1st Qu.: 57.60   1st Qu.:155.6   Class :character  
    ##  Median :39.00   Median : 66.60   Median :169.0   Mode  :character  
    ##  Mean   :39.48   Mean   : 66.39   Mean   :168.2                     
    ##  3rd Qu.:45.00   3rd Qu.: 75.30   3rd Qu.:180.1                     
    ##  Max.   :72.00   Max.   :107.20   Max.   :235.2                     
    ##   married        number_of_kids      pet               expenses      
    ##  Mode :logical   Min.   :0.000   Length:500         Min.   :-685.68  
    ##  FALSE:327       1st Qu.:0.750   Class :character   1st Qu.:  74.51  
    ##  TRUE :173       Median :1.000   Mode  :character   Median : 402.22  
    ##                  Mean   :1.558                      Mean   : 478.60  
    ##                  3rd Qu.:2.000                      3rd Qu.: 802.72  
    ##                  Max.   :6.000                      Max.   :3503.90

Widzimy, że dane kompletne. Infomracje dotyczące m. in. mediany
wykorzystamy później.

Możemy policzyć VIF, aby zobaczyć korelację pomiedzy danymi.

``` r
buff <- lm(expenses~. ,data=df)
VIF(buff)
```

    ##                    GVIF Df GVIF^(1/(2*Df))
    ## age            1.008430  1        1.004206
    ## weight         1.822536  1        1.350013
    ## height         1.824757  1        1.350836
    ## gender         1.029538  2        1.007304
    ## married        1.664691  1        1.290229
    ## number_of_kids 1.649830  1        1.284457
    ## pet            1.045822  4        1.005616

<b> 2)Wizualizacja danych</b>

Do lepszego zrozumienia danych warto je zwizualizować kilkoma wykresami.
![](Karol-Mućk-raport_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Wykres przedstawia wykresy wszystkich kombinacji zmiennych ilościowych.
Widizmy, że w zmiennych waga i wzrost wystepuje liniowa korelacja, tak
samo jak dla zmiennych wiek i wydadki.
![](Karol-Mućk-raport_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Wykres słupkowy przedstawia rozkład zmiennej waga, kolorem oznaczona
jest płeć.

![](Karol-Mućk-raport_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

Do wizulaizacji zmiennej wzrost zastosujemy wykres pudełkowy, ponowinie
kolorem zostaje oznaczona płeć.

![](Karol-Mućk-raport_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Wykres przedstawia rozkład zmiennej wiek. Widzimy, że dane zawierają
głównie ludzi w średnim wieku co się zgadza z medianą która wynosi 39.

![](Karol-Mućk-raport_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

Na wykresie kołowym widać które zwierzę jest najpopularniejsze wśród
naszych obserwacji. Najwięcej osób nie posiada zwierzęcia, jest to
dokładnie 187 osoby.

![](Karol-Mućk-raport_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

Wykres przedstawia rozkład wydadków (expenss) właścicieli róznych
zierząt.

<img src="plot.png" width="90%" />

Wykres typu parallel przedstawia wszystkie zmienne na jednym wykresie,
kolorem oznaczona jest płeć. Z uwagii na ilość obserwacji wykres nie
jest bardzo czytelny, ale możemy z niego wyczytać, że wszystkie zmienne
ilościowe nie mają korelacji z płcią. Ponownie widzimy też zależność
pomiędzy wiekiem a wydatkami.

<b> 3)przedziały ufności dla wartości średniej i wariancji dla zmiennych
wiek i wzrost</b>

Policzymy przedział ufności zakładając, że zmienna wiek i wzrost są
zmiennymi o rozkładzie normalnym, aby to sprawdzić narysujemy wykres
kwantylowy dla zmiennej wiek oraz wzrost.

Zmienna wiek:

``` r
ggplot(df, aes(sample = age), sub="wiek") + stat_qq()+ stat_qq_line()
```

![](Karol-Mućk-raport_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

Zmienna wzrost:

``` r
ggplot(df, aes(sample = height)) + stat_qq()+ stat_qq_line()
```

![](Karol-Mućk-raport_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

Widzimy, że w obu przypadkach dane znajdują się bardzo blisko prostej,
dlatego możemy zakładać, że mają rozkład normalny. Teraz, policzymy
przedział ufności dla średniej o wspólczyniku ufności 0.95.

Jak się później okaże (testując hipotezy) zmienna wiek nie ma rozkładu
normalnego. Najprawdopodobiej ma to miejsce przez odstające obserwacje,
dlatego spodziewamy się, że dla zmiennej wiek wyniki będą bardziej
prawidłowe.

``` r
wiek = df[[1]]
wzrost = df[[3]]

n = length(wiek) #wiek wzorst mają tyle samo prób

#średnia z próby
mu<-mean(wiek)
mu2<-mean(wzrost)

#odchylenie standardowe
sigma<-sd(wiek)
sigma2<-sd(wzrost)
```

Ostatecznie dostajemy przedział ufności dla zmiennej wiek.

``` r
round(mu+c(-1,1)*sigma/sqrt(n)*qnorm(.975),2)
```

    ## [1] 38.70 40.27

Oraz dla zmiennej wzrost.

``` r
round(mu2+c(-1,1)*sigma2/sqrt(n)*qnorm(.975),2)
```

    ## [1] 166.46 169.90

Teraz policzymy przedział ufności dla wariancji o wspólczyniku ufności
0.95 dla zmiennej wiek.

``` r
conf_level = 0.95

x = qchisq((1 - conf_level)/2, n-1, lower.tail = FALSE)
y = qchisq((1 - conf_level)/2, n-1)

v = var(wiek)
wiek_var = c((n-1)*v/x,(n-1)*v/y)
wiek_var
```

    ## [1] 71.44212 91.58782

Oraz dla zmiennej wzrost.

``` r
v = var(wzrost)
wzrost_var = c((n-1)*v/x,(n-1)*v/y)
wzrost_var
```

    ## [1] 342.6017 439.2107

Warto tutaj zobaczyć jak wyglądają przedziały ufności dla odchyleń
standardowych.

Dla zmiennej wiek.

``` r
sqrt(wiek_var)
```

    ## [1] 8.452344 9.570153

Oraz dla zmiennej wzrost.

``` r
sqrt(wzrost_var)
```

    ## [1] 18.50950 20.95735

<b> 4)Testowanie hipotez</b>

<b>Podpunkt 1</b>

Niech \(\mu_{0}\) będzie średnim wzrostem dla kobiet, natomiast
\(\mu_{1}\) średnim wzrostem dla mężczyzn.  

Sformułujmy hipotezę zerową  

\(H_{0}\) : \(\mu_{0} = \mu_{1}\)  

Wobec hipotezy alternatywnej  

\(H_{1}\): \(\mu_{0} \ne \mu_{1}\)

Do weryfikacji hipotezy zastosujemy test t-studenta. Aby móc
przeprowadziś test wzrost musi być zmienną o rozkładzie normalnym.
Wiemy, że tak jest dzięki porzednim analizom.

``` r
wzrost_men = df[df$gender == "man",3]
wzrost_kob = df[df$gender == "woman",3]

t.test(wzrost_men, wzrost_kob, alternative = "two.sided", var.equal = FALSE)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  wzrost_men and wzrost_kob
    ## t = 1.3099, df = 457.54, p-value = 0.1909
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -1.205973  6.027510
    ## sample estimates:
    ## mean of x mean of y 
    ##  169.6021  167.1913

P-value świadczy o tym, że nie ma powodu do odrzucenia hipotezy zerowej,
na rzecz hipotezy alternatywnej.

<b>Podpunkt 2</b>

Sformułujmy hipotezę zerową  

\(H_{0}\) : zmienne wiek i wzrost są niezależne  

Wobec hipotezy alternatywnej  

\(H_{1}\): istnieje jakikolwiek rodzaj zależności między zmiennymi wiek
i wzrost  

Do przetestwoania hipotezy skorzystamy z testu \(\rho-Persona\)

``` r
cor.test(wiek, wzrost, method = "pearson")
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  wiek and wzrost
    ## t = -0.92244, df = 498, p-value = 0.3567
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.1285253  0.0465590
    ## sample estimates:
    ##         cor 
    ## -0.04130021

Wysoka wartość p value świadczy o tym,że nie ma podstaw do odrzucenia
hipotezy o niezależności wieku i wzrostu.

<b>Podpunkt 3</b>

Sformułujmy hipotezę zerową

\(H_{0}\) : zmienne zwierzę i stan cywilny są niezależne

Wobec hipotezy alternatywnej

\(H_{1}\): istnieje jakikolwiek rodzaj zależności między zmiennymi
zwierzę i stan cywilny

Do weryfikacji hipotezy zastosujemy test chi-kwadrat \((X^{2})\)

``` r
tbl = table(df$pet,df$married) #macierz kontyngencji
chisq.test(tbl) 
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  tbl
    ## X-squared = 5.807, df = 4, p-value = 0.214

P- value jest większa niż poziom istotność 0.05 zatem nie mamy powodu do
odrzuecnia hipotezy zerowej.

<b>Podpunkt 4</b>

Sformułujmy hipotezę zerową

\(H_{0}\) : zmienna wiek jest zmienną o rozkładzie normlanym

Wobec hipotezy alternatywnej

\(H_{1}\): zmienna wiek nie jest zmienną o rozkładzie normlanym

Do zweryfikowania hipotez zastosujemy test Shapiro-Wilka.

``` r
shapiro.test(wiek)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  wiek
    ## W = 0.98179, p-value = 6.589e-06

Niska wartość p świadczy o tym, że możemy odrzucić hipotezę zerową na
rzecz hipotezy altenatywnej.

<b> 5)Regresja liniowa</b>

Zaimplementujmy model regresji liniowej używając wszystkich zmiennych.

``` r
attach(df)
linearMod <- lm(expenses ~ ., data = df)
```

Aby zobaczyć wszystkie interesujace nas parametry takie jak \(RSS\) czy
\(R^{2}\) zastosujemy funkcję summary.

``` r
summary(linearMod)
```

    ## 
    ## Call:
    ## lm(formula = expenses ~ ., data = df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -768.28 -127.24   -2.79  130.54  905.98 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    -2276.7455   100.1447 -22.735  < 2e-16 ***
    ## age               57.5889     1.0712  53.759  < 2e-16 ***
    ## weight             1.2078     0.9984   1.210  0.22698    
    ## height             2.0637     0.6580   3.136  0.00182 ** 
    ## genderother       44.1656    37.7199   1.171  0.24222    
    ## genderwoman      -21.7164    20.1406  -1.078  0.28146    
    ## marriedTRUE       -9.8079    25.9461  -0.378  0.70559    
    ## number_of_kids   -12.4393     8.8614  -1.404  0.16103    
    ## petdog            29.2695    30.0801   0.973  0.33101    
    ## petferret        406.6324    36.3026  11.201  < 2e-16 ***
    ## pethedgehog      242.0460    35.9101   6.740 4.47e-11 ***
    ## petnone           21.7454    26.2294   0.829  0.40748    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 213.9 on 488 degrees of freedom
    ## Multiple R-squared:  0.861,  Adjusted R-squared:  0.8579 
    ## F-statistic: 274.9 on 11 and 488 DF,  p-value: < 2.2e-16

Widzimy, że zmienną która najlepiej opisuje wydadtki jest wiek.
Możnabyło się tego spodziewać patrząć na wykresy na samym początku.
Przypomnijmy ten wykres.

![](Karol-Mućk-raport_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

Przeanalizyjmy teraz model za pomocą wykresów diagnostycznych.

![](Karol-Mućk-raport_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

Dzięki temu wykresowi możemy zobaczyć czy zależność danych jest liniowa.
Widzimy, że tam gdzie mamy dużo danych czerwona linia bardzo przypomina
prostą. Dalej, mamy do czynienia z małą liczbą danych i krzywa zaczyna
się wykrzywiać.

![](Karol-Mućk-raport_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

Wykres przedstawia zależność \(Y\) od \(\hat{Y}\). Dzięki temu wykresowi
możemy sprawdzić założenie o homoscedastyczności składnika losowego.
Czerwona linia przedstawia trend. Widzimy, że wariancja jest bardzo
zbliżona dla danych. Widać również, że linia delitaknie rośnie wraz ze
wraz ze zwiększeniem wydatków jendak wtedy również mamy doczynienia z
małą liczbą danych, co sprawia, że estymacja jest bardzo niepewna.

![](Karol-Mućk-raport_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

Widzimy, że rozkład residuów jest bardzo zbliżony do rozkłądu
normalnego. Nie musimy przeprowadzać żadnych transofrmacji zmiennych.

![](Karol-Mućk-raport_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

Wykres przedstawia jaki wpływ na ocenę współczyników ma dana oserwacja.
Widzimy, że wartości residuów nie są znacząco różne od zera, co dobrze
świadczy o naszym modelu. Widzimy również małą liczbę danych
odstających.

![](Karol-Mućk-raport_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

Możemy również przedstawić wpływ każdej obserwacji na postać regresji za
pomocą odległości Cooka. Widzimy, trzy wyraźnie odstające dane, jednak
dla żadnej z nich odległość jest większa niż 1 więc nie są one wpływowe.

Na koniec policzmy przedział ufności dla parametrów.

``` r
confint(linearMod)
```

    ##                        2.5 %       97.5 %
    ## (Intercept)    -2473.5135002 -2079.977557
    ## age               55.4840320    59.693688
    ## weight            -0.7539471     3.169481
    ## height             0.7707755     3.356657
    ## genderother      -29.9478322   118.279119
    ## genderwoman      -61.2894344    17.856718
    ## marriedTRUE      -60.7877919    41.171958
    ## number_of_kids   -29.8505420     4.971953
    ## petdog           -29.8329474    88.371957
    ## petferret        335.3037468   477.961067
    ## pethedgehog      171.4885527   312.603360
    ## petnone          -29.7911599    73.281929
