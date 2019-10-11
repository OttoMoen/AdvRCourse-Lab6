# AdvRCourse-Lab6
[![Build Status](https://travis-ci.org/steliossid/AdvRCourse-Lab6.svg?branch=master)](https://travis-ci.org/steliossid/AdvRCourse-Lab6)

**Collaborative project for Lab 6 in Advanced R Programming course (Stastistics &amp; Machine Learning MSc - LiU)**

This R package solves the Knapsack problem. It is implemented in three different approaches: the brute force, dynamic and greedy heuristic methods. The brute force algorithm can be paralellized, for better code performance.

**Group 16:**

* Vasileios Karapoulios - vaska579

* Stylianos Sidiropoulos - stysi607

## Installation
Run the following commands in R to install this package:

```r
install.packages('devtools')
devtools::install_github("steliossid/AdvRCourse-Lab6", subdir = "Lab6/")
```

## Import libraries
```r
library(Lab6)
library(parallel)
```

## Set the data
```r
set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )
```

## Examples
### Brute Force Search
```r
brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
```
### Dynamic programming
```r
knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
```
### Greedy heuristic
```r
greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
```
### Brute Force with Parallelization
```r
brute_force_knapsack(x = knapsack_objects[1:10,], W = 3500, parallel = TRUE)
```
