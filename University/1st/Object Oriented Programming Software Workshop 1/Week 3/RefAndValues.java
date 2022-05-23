import java.util.Arrays;

public class RefAndValues {

    public void values(){
        int myNum = 5;
        int myOtherNum = 10;
    }
    //toString() -> returns the String representation of the object, any object
    public void references(int [] theArray){
        System.out.println("in the method My array "+Arrays.toString(theArray));
        theArray[1] = 555;
    }

    public int [] returningAnArray(int [] myOther){
        myOther[3] = 222;
        return myOther;
    }

    public static void main(String[] args) {
        RefAndValues rv = new RefAndValues();
        //System.out.println("The string representaion is "+rv.toString());
        rv.values();
        int[] myArray = new int[5]; //reference types, holds an address to object
        //myArray holds an address to where the object is
        int[] myOtherArray = myArray;
        //both point to the SAME address
        System.out.println("My array " + Arrays.toString(myArray));
        System.out.println("My other array " + Arrays.toString(myOtherArray));
        myOtherArray[0] = 1;
        System.out.println("after My array " + Arrays.toString(myArray));
        System.out.println("after My other array " + Arrays.toString(myOtherArray));
        rv.references(myArray);
        System.out.println("after the method My array " + Arrays.toString(myArray));
        myOtherArray = rv.returningAnArray(myOtherArray);
        System.out.println("after the return method My other " + Arrays.toString(myOtherArray));
        myOtherArray[2] = 44444;
        System.out.println("after the change in main() My other array " + Arrays.toString(myOtherArray));
    }
}
/**
 * Areas of self-study
 * 1. wrapper classes
 * 2. StringBuilder
 * 3. toString() look at other examples
 * 4. Ensure that you understand reference vs. value
 */