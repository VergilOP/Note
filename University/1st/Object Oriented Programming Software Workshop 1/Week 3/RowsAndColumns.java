/**
 * @author: Jacqui Chetty
 *  * The RowsAndColumns class describes how to enter and print data using
 *  * multidimensional arrays
 */

import java.util.ArrayList;
import java.util.Scanner;

public class RowsAndColumns {
    //Java enums is a special Java type to define a collection of constants -> it is a class of its own
    public enum Week { //enum is used instead of the word class
        MONDAY,
        TUESDAY,
        WEDNESDAY,
        THURSDAY,
        FRIDAY}

    Scanner sc = new Scanner(System.in);

    /**
     * This method creates a multidim array
     */
    public void multiArrays() {

        int[][] moreStudentMarks = new int[4][3];
        int[][] studentMarks = {{5, 7, 1},
                {9, 3, 6},
                {4, 8, 5},
                {2, 7, 3}};

        for (int i = 0; i < 4; i++) {
            for (int j = 0; j < 3; j++) {
                System.out.print(studentMarks[i][j] + "  ");
            }
            System.out.println();
        }

        for (int i = 0; i < 4; i++) {
            for (int j = 0; j < 3; j++) {
                System.out.println("Please enter a number for row " + i + " Column " + j);
                moreStudentMarks[i][j] = sc.nextInt();
            }
            System.out.println();
        }

        for (int i = 0; i < 4; i++) {
            for (int j = 0; j < 3; j++) {
                System.out.print(moreStudentMarks[i][j] + "  ");
            }
            System.out.println();
        }
    }

    /**
     * @param whichDay
     * This parameter allows a day of the week to be entered
     */
    public void eNums(Week whichDay) {
        if (whichDay.equals(Week.FRIDAY))
            System.out.println("You can take leave");
    }

    public void eNumsSwitch(Week whichDay) {
        switch (whichDay){
            case MONDAY:break;
            case TUESDAY:break;
            case WEDNESDAY:break;
            case THURSDAY:break;
            case FRIDAY:break;
        }
    }

    public void eNumsLoops() {
        for (Week week: Week.values()){
            System.out.println(week);
        }
    }

    /**
     * Creates arrayLists for names and nums and adds data to the lists
     */
    public void arrayLists() {
        ArrayList<String> names = new ArrayList<>(); //creating an arraylist with type String, ArrayList is a class()
        ArrayList<Integer> nums = new ArrayList<>();
        names.add("Damian");//call a method add()
        nums.add(9);
        displayData(names, nums);
    }

    /**
     * @param names
     * @param nums
     * pass the parameters to the method to print
     */
    public void displayData(ArrayList<String> names, ArrayList<Integer> nums) {
        System.out.println("You have " +names.size() +" items in the list");
        for (int x = 0; x < names.size() && x < nums.size(); x++) {
            System.out.println(names.get(x) + " " + nums.get(x));
            ;
        }
    }

    public void enterNumbers() {
        int num;
        System.out.println("Please enter a number");
        num = Integer.parseInt(sc.nextLine());//wrapper classes
    }

    public static void main(String[] args) {
        RowsAndColumns rc = new RowsAndColumns();
        //rc.multiArrays();
        String weekDay = "FRIDAY";
        Week whichDay = Week.valueOf(weekDay);
        rc.eNums(whichDay);
        rc.eNumsSwitch(whichDay);
        rc.eNumsLoops();
        //rc.arrayLists();
    }
}
