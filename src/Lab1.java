import java.util.Arrays;

public class Lab1 {
    /**
     * Sorting algorithms
     **/
    public static void main(String[] args) {
        int[] arr1 = { 1, 2, 3, 4, 5, 6, 7, 8 };
        int[] arr2 = { 1, 2, 3, 4, 5 };
        System.out.println(Arrays.toString(merge(arr1, arr2)));
    }

    // Insertion sort.
    public static void insertionSort(int[] array) {
        for (int lastSortedIndex = 0; lastSortedIndex < array.length - 1; lastSortedIndex++) {
            insert(array, lastSortedIndex, array[lastSortedIndex + 1]);
        }
    }

    private static void insert(int[] array, int lastSortedIndex, int toBeInserted) {
        for (int i = lastSortedIndex; i >= 0; i--) {
            if (array[i] < toBeInserted) {
                return;
            }
            swap(array, i, i + 1);
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
            lo = advanceLo(array, lo, piv, end);
            hi = advanceHi(array, hi, piv, begin + 1);
            if (hi >= lo) {
                swap(array, lo, hi);
                lo++;
                hi--;
            }
        }
        swap(array, begin, hi);

        return hi;
    }

    private static int advanceHi(int[] array, int hi, int piv, int begin) {
        while (hi >= begin && array[hi] >= piv) {
            hi--;
        }
        return hi;
    }

    private static int advanceLo(int[] array, int lo, int piv, int end) {
        while (lo <= end && array[lo] <= piv) {
            lo++;
        }
        return lo;
    }

    // Swap two elements in an array
    private static void swap(int[] array, int i, int j) {
        int x = array[i];
        array[i] = array[j];
        array[j] = x;
    }

    // Mergesort.

    public static int[] mergeSort(int[] array) {
        throw new UnsupportedOperationException();
    }

    // Mergesort part of an array
    private static int[] mergeSort(int[] array, int begin, int end) {
        throw new UnsupportedOperationException();
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
