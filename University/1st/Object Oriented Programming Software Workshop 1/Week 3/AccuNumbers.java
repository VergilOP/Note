import java.util.Scanner;

public class AccuNumbers {

    public void workWithNumbersArray(int[] myNumbers) {
        System.out.println("The number is - entering the method " + myNumbers[0]);
        Scanner sc = new Scanner(System.in);
        myNumbers[0] = 100;
        System.out.println("The number is " + myNumbers[0]);

        String[] names = {"Jack", "John", "Jill"};

        int[] moreNumbers = new int[10];


        int total = 0;

        String aName;

        //for(int x = 0; x < 10; x++)
        for (int x = 0; x < myNumbers.length; x++) {//length is a parameter
            System.out.println("Number " + (x + 1) + " is " + myNumbers[x]);
            //total = total + myNumbers[x];
            total += myNumbers[x]; //accumulating values to total
        }
        System.out.println("The total is " + total);

        namesInputting(names);
        //input elements into the array
        for (int x = 0; x < names.length; x++) {
            System.out.println("Please enter a name");
            names[x] = sc.nextLine();
        }

        namesInputting(names);
        //searching for an element in an array
        System.out.println("Please enter a name you are searching for ");
        aName = sc.nextLine();
        for (int x = 0; x < names.length; x++) {
            //if (aNum == anotherNum)shallow comparison
            if (aName.equals(names[x])) {//deep comparison
                //Sarah
                System.out.println("Found");
                break;
            }
        }
        System.out.println("Before sorting");
        for (int x = 0; x < myNumbers.length; x++) {
            System.out.println("Number " + (x + 1) + " is " + myNumbers[x]);
        }
        myNumbers = sortingNums(myNumbers);
        System.out.println("After sorting");
        for (int x = 0; x < myNumbers.length; x++) {
            System.out.println("Number " + (x + 1) + " is " + myNumbers[x]);
        }
    }

    public void namesInputting(String[] names) {
        
        for (int x = 0; x < names.length; x++) {
            System.out.println("The name is " + names[x]);
        }

    }

    public int[] sortingNums(int[] myNums) {//bubble sort
        int temp;
        //outer loop pass 1, 2, 3, -each element sorted one by one
        for (int x = 0; x < myNums.length; x++) {//outer inner loops
            for (int y = 0; y < myNums.length - 1; y++) {
                if (myNums[y] > myNums[y + 1]) {
                    temp = myNums[y];
                    myNums[y] = myNums[y + 1];
                    myNums[y + 1] = temp;
                }
            }
        }
        return myNums;
    }

    public static void main(String[] args) {
        int[] theNumbers = {5, 2, 8, 90, 12, 56, 56, 9, 32, 45};//<- elements
        // position          0  1  2   3   4   5   6  7   8   9 <- subscript / index
        //10 elements in the array, number of elements in the array is 10
        AccuNumbers an = new AccuNumbers();
        an.workWithNumbersArray(theNumbers);
    }
}
