# Appendix  B - Network Statistics.
Ben Weinstein  
2/1/2016  


```
## [1] "Run Completed at 2016-06-02 08:39:52"
```




************



#Read in data

From Generate.Rmd



#Daily interaction rate


#Overall Network

## Ordered by interactions

<img src="figure/unnamed-chunk-6-1.png" title="" alt="" style="display: block; margin: auto;" />

##Ordered by traits

<img src="figure/unnamed-chunk-7-1.png" title="" alt="" style="display: block; margin: auto;" />

##Calculate overall statistics.



## Permutation tests

We want to randomize the underlying total matrix, then recalculate daily interaction rates. We maintain column sums since cameras were placed at individual flowers, those identities are fixed through sampling. It is only the identity of hummingbirds that could vary in create realistic alternative networks. 

### Calculate randomized matrix

Maintaining the total number of observations, but shuffling where those visitations fall.

<img src="figure/unnamed-chunk-9-1.png" title="" alt="" style="display: block; margin: auto;" />


```
## png 
##   2
```

#Temporal change in network

##View Sub-Networks


```
## $`6.2013`
```

<img src="figure/unnamed-chunk-11-1.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`7.2013`
```

<img src="figure/unnamed-chunk-11-2.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`8.2013`
```

<img src="figure/unnamed-chunk-11-3.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`10.2013`
```

<img src="figure/unnamed-chunk-11-4.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`11.2013`
```

<img src="figure/unnamed-chunk-11-5.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`12.2013`
```

<img src="figure/unnamed-chunk-11-6.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`1.2014`
```

<img src="figure/unnamed-chunk-11-7.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`2.2014`
```

<img src="figure/unnamed-chunk-11-8.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`3.2014`
```

<img src="figure/unnamed-chunk-11-9.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`4.2014`
```

<img src="figure/unnamed-chunk-11-10.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`5.2014`
```

<img src="figure/unnamed-chunk-11-11.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`6.2014`
```

<img src="figure/unnamed-chunk-11-12.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`7.2014`
```

<img src="figure/unnamed-chunk-11-13.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`8.2014`
```

<img src="figure/unnamed-chunk-11-14.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`9.2014`
```

<img src="figure/unnamed-chunk-11-15.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`10.2014`
```

<img src="figure/unnamed-chunk-11-16.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`11.2014`
```

<img src="figure/unnamed-chunk-11-17.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`12.2014`
```

<img src="figure/unnamed-chunk-11-18.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`1.2015`
```

<img src="figure/unnamed-chunk-11-19.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`2.2015`
```

<img src="figure/unnamed-chunk-11-20.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`3.2015`
```

<img src="figure/unnamed-chunk-11-21.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`4.2015`
```

<img src="figure/unnamed-chunk-11-22.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`5.2015`
```

<img src="figure/unnamed-chunk-11-23.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`6.2015`
```

<img src="figure/unnamed-chunk-11-24.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`7.2015`
```

<img src="figure/unnamed-chunk-11-25.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`8.2015`
```

<img src="figure/unnamed-chunk-11-26.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`9.2015`
```

<img src="figure/unnamed-chunk-11-27.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`10.2015`
```

<img src="figure/unnamed-chunk-11-28.png" title="" alt="" style="display: block; margin: auto;" />


## Randomize with respect to sampling



##Which months were sufficiently sampled?



###Apply function to each period




#Observed versus null values of nestedness, connectance within time periods

<img src="figure/unnamed-chunk-15-1.png" title="" alt="" style="display: block; margin: auto;" />


