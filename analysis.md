# When would you reccomend each algorithm?

- Is there a size cutoff at which a different algorithm becomes best? & How does the type of test data affect which algorithm you should choose?
- - Insertion sort is best in all categories at a low number of data values, n <= 30, and still best for almost sorted arrays upp to n <= 3000. Insertion sort always performs best for sorted arrays
- - Quick sort is the best at sorting random arrays for n >= 100 but does not perform well with nearly sorted arrays and extreamly poorly with sorted arrays. 
- - Merge sort is decent in all cases and performs the best for lager data sets espesially for somewhat sorted arrays


- Which should you choose if you don’t know anything about the test data? & Are there circumstances when you should definitely avoid a certain algorithm?
- - If you know nothing about the data set, merge sort is an all round decent algorithm and a good middleground not overly dependent on data sets. 
- - If you know the data is at least almost sorted, don't use quickSort. If you have a large set of data, avoid insertionSort unless you know all data is already sorted. 


# Benchmark results
<div>
Arrays of length 10
=================================================================
Algorithm      |         Random |     95% sorted |         Sorted <br> 
Insertion sort |       0.000053 |       0.000020 |       0.000025 <br>
Quicksort      |       0.000565 |       0.000155 |       0.000123 <br>
Merge sort     |       0.000916 |       0.000601 |       0.000449 <br>



Arrays of length 30
================================================================= <br>
Algorithm      |         Random |     95% sorted |         Sorted <br>
Insertion sort |       0.000241 |       0.000049 |       0.000048 <br>
Quicksort      |       0.000356 |       0.000622 |       0.000734 <br>
Merge sort     |       0.001681 |       0.001412 |       0.001442 <br>



Arrays of length 100
=================================================================
Algorithm      |         Random |     95% sorted |         Sorted
Insertion sort |       0.002715 |       0.000455 |       0.000168
Quicksort      |       0.001424 |       0.003423 |       0.005292
Merge sort     |       0.005946 |       0.005176 |       0.005756


Arrays of length 300
=================================================================<br>
Algorithm      |         Random |     95% sorted |         Sorted
Insertion sort |       0.016736 |       0.001769 |       0.000468
Quicksort      |       0.004855 |       0.012325 |       0.044853
Merge sort     |       0.024554 |       0.019995 |       0.018947

Arrays of length 1000
=================================================================
Algorithm      |         Random |     95% sorted |         Sorted
Insertion sort |       0.167955 |       0.015402 |       0.001443
Quicksort      |       0.034269 |       0.043857 |       0.456050
Merge sort     |       0.114421 |       0.077612 |       0.067495

Arrays of length 3000
=================================================================
Algorithm      |         Random |     95% sorted |         Sorted
Insertion sort |       1.482746 |       0.109535 |       0.004308
Quicksort      |       0.209967 |       0.298328 |       3.996325
Merge sort     |       0.415708 |       0.220956 |       0.217327

Arrays of length 10000
=================================================================
Algorithm      |         Random |     95% sorted |         Sorted
Insertion sort |      16.564400 |       1.199489 |       0.014126
Quicksort      |       0.807329 |       1.172531 |      44.089700
Merge sort     |       1.572578 |       1.082003 |       0.912766

Arrays of length 30000
=================================================================
Algorithm      |         Random |     95% sorted |         Sorted
Insertion sort |     147.650600 |      10.184600 |       0.042444
Quicksort      |       2.742300 |       3.222175 | STACK OVERFLOW
Merge sort     |       6.347150 |       3.039950 |       2.151690

Arrays of length 100000
=================================================================
Algorithm      |         Random |     95% sorted |         Sorted
Insertion sort |    1673.536900 |     111.757200 |       0.140757
Quicksort      |       9.877850 |      14.275900 | STACK OVERFLOW
Merge sort     |      17.858100 |      10.923350 |      10.684800
</div>
