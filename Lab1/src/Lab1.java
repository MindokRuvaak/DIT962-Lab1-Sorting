import java.util.Arrays;

public class Lab1 {
    /**
     * Sorting algorithms
     **/

    // Insertion sort.
    public static void insertionSort(int[] array) {
        for (int lastSortedIndex = 0; lastSortedIndex < array.length - 1; lastSortedIndex++) {
            // store the next element to be sorted in a temporary variable
            int toBeInserted = array[lastSortedIndex + 1]; 
            // step through the sorted part of the array
            for (int i = lastSortedIndex; i >= 0; i--) {
                // if the to-be-inserted element is smaller than the current sorted element:
                if (array[i] > toBeInserted) { 
                    // shift the already sorted elemet ''upp'' one step
                    array[i + 1] = array[i];
                    // and if the front of the array has been reached
                    if (i == 0) { 
                        // inserted element is smaller than all the already sorted elements
                        array[i] = toBeInserted;
                    }
                } else { // otherwise the place for the to-be-inserted element has been found 
                    // place the element
                    array[i + 1] = toBeInserted;
                    // exit the for-loop and start again
                    break;
                }
            }
        }
    }

    // Quicksort.
    public static void quickSort(int[] array) {
        if (array.length > 0) {
            quickSort(array, 0, array.length - 1); // quicksort the entire array
        }
    }

    // Quicksort part of an array
    private static void quickSort(int[] array, int begin, int end) {
        // inclusive origin, exclusive bound
        int pivotElement = partition(array, begin, end);
        if (begin < pivotElement - 1) {
            quickSort(array, begin, pivotElement - 1);
        }
        if (end > pivotElement + 1) {
            quickSort(array, pivotElement + 1, end);
        }
    }

    // Partition part of an array, and return the index where the pivot
    // ended up.
    private static int partition(int[] array, int begin, int end) {
        int piv = array[begin];
        int lo = begin + 1; // pivot element located at index begin, begin at element after
        int hi = end;

        while (hi >= lo) {
            while (lo <= end && array[lo] <= piv) {
                lo++;
            }
            while (hi > begin && array[hi] >= piv) {
                hi--;
            }
            if (hi >= lo) {
                swap(array, lo, hi);
                lo++;
                hi--;
            }
        }
        swap(array, begin, hi);

        return hi;
    }


    // Swap two elements in an array
    private static void swap(int[] array, int i, int j) {
        int x = array[i];
        array[i] = array[j];
        array[j] = x;
    }

    // Mergesort.
    public static int[] mergeSort(int[] array) {
        if (array.length <= 1) {
            return array;
        }
        int mid = (0 + array.length) / 2;
        return merge(mergeSort(Arrays.copyOfRange(array, 0, mid)), 
                    mergeSort(Arrays.copyOfRange(array, mid, array.length)));
    }

    // Merge two sorted arrays into one
    private static int[] merge(int[] left, int[] right) {
        int leftPointer = 0, rightPointer = 0, ind = 0;
        int[] returnArr = new int[left.length + right.length];
        while (leftPointer < left.length && rightPointer < right.length) {
            if (left[leftPointer] <= right[rightPointer]) {
                returnArr[ind++] = left[leftPointer++];
            } else {
                returnArr[ind++] = right[rightPointer++];
            }
        }
        for (int i = ind; i < returnArr.length; i++) {
            if (left.length > leftPointer) {
                returnArr[i] = left[leftPointer++];
            } else {
                returnArr[i] = right[rightPointer++];
            }
        }
        return returnArr;
    }
}
