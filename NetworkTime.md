# Appendix  B - Network Statistics.
Ben Weinstein  
2/1/2016  


```
## [1] "Run Completed at 2016-06-03 15:03:31"
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
## $`2013-06-01`
```

<img src="figure/unnamed-chunk-11-1.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`2013-08-01`
```

<img src="figure/unnamed-chunk-11-2.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`2013-10-01`
```

<img src="figure/unnamed-chunk-11-3.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`2013-12-01`
```

<img src="figure/unnamed-chunk-11-4.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`2014-02-01`
```

<img src="figure/unnamed-chunk-11-5.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`2014-04-01`
```

<img src="figure/unnamed-chunk-11-6.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`2014-06-01`
```

<img src="figure/unnamed-chunk-11-7.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`2014-08-01`
```

<img src="figure/unnamed-chunk-11-8.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`2014-10-01`
```

<img src="figure/unnamed-chunk-11-9.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`2014-12-01`
```

<img src="figure/unnamed-chunk-11-10.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`2015-02-01`
```

<img src="figure/unnamed-chunk-11-11.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`2015-04-01`
```

<img src="figure/unnamed-chunk-11-12.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`2015-06-01`
```

<img src="figure/unnamed-chunk-11-13.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`2015-08-01`
```

<img src="figure/unnamed-chunk-11-14.png" title="" alt="" style="display: block; margin: auto;" />

```
## 
## $`2015-10-01`
```

<img src="figure/unnamed-chunk-11-15.png" title="" alt="" style="display: block; margin: auto;" />

## Randomize with respect to sampling






###Apply function to each period





<img src="figure/unnamed-chunk-16-1.png" title="" alt="" style="display: block; margin: auto;" />

## Difference from expected

<img src="figure/unnamed-chunk-17-1.png" title="" alt="" style="display: block; margin: auto;" />

##Resource Regression

<img src="figure/unnamed-chunk-18-1.png" title="" alt="" style="display: block; margin: auto;" />



