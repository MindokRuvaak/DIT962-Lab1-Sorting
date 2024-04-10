# When would you reccomend each algorithm?

- Is there a size cutoff at which a different algorithm becomes best? & How does the type of test data affect which algorithm you should choose?
- - Insertion sort is best in all categories at a low number of data values, n <= 30, and still best for almost sorted arrays upp to n <= 3000. Insertion sort always performs best for sorted arrays
- - Quick sort is the best at sorting random arrays for n >= 100 but does not perform well with nearly sorted arrays and extreamly poorly with sorted arrays.
- - Merge sort is decent in all cases and performs the best for lager data sets espesially for somewhat sorted arrays

- Which should you choose if you don’t know anything about the test data? & Are there circumstances when you should definitely avoid a certain algorithm?
- - If you know nothing about the data set, merge sort is an all round decent algorithm and a good middleground not overly dependent on data sets.
- - If you know the data is at least almost sorted, don't use quickSort. If you have a large set of data, avoid insertionSort unless you know all data is already sorted.

# Benchmark results

## arrays of length 10
<table>
    <tr>
        <th> algoritm </th>
        <th> random </th>
        <th> 95% sorted </th>
        <th> sorted </th>
    </tr>
    <tr>
        <th> Insertion sort</th>
        <th> 0.000045</th>
        <th> 0.000021</th>
        <th> 0.000017 </th>
    </tr>
    <tr>
        <th> Quicksort</th>
        <th> 0.000048</th>
        <th> 0.000064</th>
        <th> 0.000084</th>
    </tr>
    <tr>
        <th> Merge sort</th>
        <th> 0.000231</th>
        <th> 0.000198</th>
        <th> 0.000203</th>
    </tr>
</table>

## arrays of length 30
<table>
    <tr>
        <th> algoritm </th>
        <th> random </th>
        <th> 95% sorted </th>
        <th> sorted </th>
    </tr>
    <tr>
        <th> Insertion sort</th>
        <th> 0.000124</th>
        <th> 0.000055</th>
        <th> 0.000046 </th>
    </tr>
    <tr>
        <th> Quicksort</th>
        <th> 0.000155</th>
        <th> 0.000644</th>
        <th> 0.000504</th>
    </tr>
    <tr>
        <th> Merge sort</th>
        <th> 0.000984</th>
        <th> 0.000774</th>
        <th> 0.000742</th>
    </tr>
</table>


## arrays of length 100
<table>
    <tr>
        <th> algoritm </th>
        <th> random </th>
        <th> 95% sorted </th>
        <th> sorted </th>
    </tr>
    <tr>
        <th> Insertion sort</th>
        <th> 0.001144</th>
        <th> 0.000251</th>
        <th> 0.000132 </th>
    </tr>
    <tr>
        <th> Quicksort</th>
        <th> 0.000832</th>
        <th> 0.002094</th>
        <th> 0.003122</th>
    </tr>
    <tr>
        <th> Merge sort</th>
        <th> 0.002739</th>
        <th> 0.002507</th>
        <th> 0.002964</th>
    </tr>
</table>


## arrays of length 300
<table>
    <tr>
        <th> algoritm </th>
        <th> random </th>
        <th> 95% sorted </th>
        <th> sorted </th>
    </tr>
    <tr>
        <th> Insertion sort</th>
        <th> 0.011804</th>
        <th> 0.000849</th>
        <th> 0.000299 </th>
    </tr>
    <tr>
        <th> Quicksort</th>
        <th> 0.002611</th>
        <th> 0.010300</th>
        <th> 0.034181</th>
    </tr>
    <tr>
        <th> Merge sort</th>
        <th> 0.008948</th>
        <th> 0.010394</th>
        <th> 0.010349</th>
    </tr>
</table>


## arrays of length 1000
<table>
    <tr>
        <th> algoritm </th>
        <th> random </th>
        <th> 95% sorted </th>
        <th> sorted </th>
    </tr>
    <tr>
        <th> Insertion sort</th>
        <th> 0.106001</th>
        <th> 0.013854</th>
        <th> 0.001582</th>
    </tr>
    <tr>
        <th> Quicksort</th>
        <th> 0.015591</th>
        <th> 0.026156</th>
        <th> 0.446350</th>
    </tr>
    <tr>
        <th> Merge sort</th>
        <th> 0.053368</th>
        <th> 0.041475</th>
        <th> 0.034642</th>
    </tr>
</table>


## arrays of length 3000
<table>
    <tr>
        <th> algoritm </th>
        <th> random </th>
        <th> 95% sorted </th>
        <th> sorted </th>
    </tr>
    <tr>
        <th> Insertion sort</th>
        <th> 0.875176</th>
        <th> 0.069765</th>
        <th> 0.003738 </th>
    </tr>
    <tr>
        <th> Quicksort</th>
        <th> 0.150946</th>
        <th> 0.104610</th>
        <th> 3.551212</th>
    </tr>
    <tr>
        <th> Merge sort</th>
        <th> 0.241981</th>
        <th> 0.144392</th>
        <th> 0.102991</th>
    </tr>
</table>


## arrays of length 10000
<table>
    <tr>
        <th> algoritm </th>
        <th> random </th>
        <th> 95% sorted </th>
        <th> sorted </th>
    </tr>
    <tr>
        <th> Insertion sort</th>
        <th> 8.984166</th>
        <th> 0.696985</th>
        <th> 0.011362</th>
    </tr>
    <tr>
        <th> Quicksort</th>
        <th> 0.666222</th>
        <th> 0.705636</th>
        <th> 34.283564</th>
    </tr>
    <tr>
        <th> Merge sort</th>
        <th> 0.882807</th>
        <th> 0.396481</th>
        <th> 0.380574</th>
    </tr>
</table>


## arrays of length 30000
<table>
    <tr>
        <th> algoritm </th>
        <th> random </th>
        <th> 95% sorted </th>
        <th> sorted </th>
    </tr>
    <tr>
        <th> Insertion sort</th>
        <th> 80.985731</th>
        <th> 5.470988</th>
        <th> 0.028772 </th>
    </tr>
    <tr>
        <th> Quicksort</th>
        <th> 1.701307</th>
        <th> 2.029560</th>
        <th> STACK OVERFLOW</th>
    </tr>
    <tr>
        <th> Merge sort</th>
        <th> 3.132644</th>
        <th> 2.113990</th>
        <th> 1.334938</th>
    </tr>
</table>


## arrays of length 100000
<table>
    <tr>
        <th> algoritm </th>
        <th> random </th>
        <th> 95% sorted </th>
        <th> sorted </th>
    </tr>
    <tr>
        <th> Insertion sort</th>
        <th> 1128.490564</th>
        <th> 65.975997</th>
        <th> 0.095926</th>
    </tr>
    <tr>
        <th> Quicksort</th>
        <th> 6.502958</th>
        <th> 6.968899</th>
        <th> STACK OVERFLOW</th>
    </tr>
    <tr>
        <th> Merge sort</th>
        <th> 11.276677</th>
        <th> 6.003096</th>
        <th> 8.072359</th>
    </tr>
</table>

