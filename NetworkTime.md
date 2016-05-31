# Appendix  B - Network Statistics.
Ben Weinstein  
2/1/2016  


```
## [1] "Run Completed at 2016-05-31 10:00:03"
```




************



#Read in data

From Generate.Rmd



#Overall Network

<img src="figure/unnamed-chunk-5-1.png" title="" alt="" style="display: block; margin: auto;" />

##Calculate overall statistics.



## Permutation tests

There is some discussion as to what the best null model for nestedness is. Should we constrain the matrix? Try both ways to see if it makes a difference in inference.

### Calculate randomized matrix

Maintaining the total number of observations, but shuffling where those visitations fall.

<img src="figure/unnamed-chunk-7-1.png" title="" alt="" style="display: block; margin: auto;" />


```
## png 
##   2
```

#Temporal change in network

##View Sub-Networks

<img src="figure/unnamed-chunk-9-1.png" title="" alt="" style="display: block; margin: auto;" />


## Null function



###Apply function to each period




#Observed versus null values of nestedness, connectance

<img src="figure/unnamed-chunk-12-1.png" title="" alt="" style="display: block; margin: auto;" />

#Randomize with respect to resource membership.  

<img src="figure/unnamed-chunk-13-1.png" title="" alt="" style="display: block; margin: auto;" />


