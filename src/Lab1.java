
import java.util.Arrays;

public class Lab1 {

    // for testing
    public static void main(String[] args) {
        int[] ints = { 2, 3, 5 };
        insertionSort(ints);
        System.out.println(Arrays.toString(ints));
        ints = new int[] { 1, 3 };
        insertionSort(ints);
        System.out.println(Arrays.toString(ints));
        ints = new int[] { 4, 2 };
        insertionSort(ints);
        System.out.println(Arrays.toString(ints));
        ints = new int[] { 2, 4 };
        insertionSort(ints);
        System.out.println(Arrays.toString(ints));

        ints = new int[] { 5, 3, 9, 2, 8 };
        insertionSort(ints);
        System.out.println(Arrays.toString(ints));
    }

    /**
     * Sorting algorithms
     **/

    // Insertion sort.
    public static void insertionSort(int[] array) {
        for (int lastSortedIndex = 0; lastSortedIndex < array.length - 1; lastSortedIndex++) {
            insert(array, lastSortedIndex, array[lastSortedIndex+1]);
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
        throw new UnsupportedOperationException();
    }

    // Quicksort part of an array
    private static void quickSort(int[] array, int begin, int end) {
    }

    // Partition part of an array, and return the index where the pivot
    // ended up.
    private static int partition(int[] array, int begin, int end) {
        throw new UnsupportedOperationException();
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
        throw new UnsupportedOperationException();
    }
}
