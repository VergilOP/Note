/* DebugFive4.java
 Outputs highest of four numbers

 TODO 1 Open the TODO tab.
 TODO 2 Fix any problems you can see in the code.
 TODO 3 Use the Problems tab to see if you can make any further improvements.
 TODO 4 Break the code into blocks by adding blank lines: this is called
          paragraphing. Make a block of the variable declarations, then make
          blocks for each of the parts that reads and processes one line of
          input. Now separate the if statement from the line before it.
 TODO 5 Use Code > Optimize Imports and note the before-and-after difference.
 TODO 6 Replace the print statements with printf using %n for a new line.
 TODO 7 Run Inspect Code... from the Code menu on this file.
 TODO 8 Run Code > Reformat Code and do a before-and-after comparison.
 TODO 9 Adjust the printf template from %s to %S to print in ALL CAPS.
 TODO 10 Undo the previous change to the template because SHOUTING IS RUDE.
 TODO 11 Put the cursor on different parts of the different if statements
           and look at the Context Actions (light bulbs) to see how IntelliJ
           can help you. Change things to see what they do but keep testing
           your program to ensure it still does what it is supposed to do.
           Make use of the Undo function.
 TODO 12 We will now do some DRY programming: Don't Repeat Yourself
           Create a constant whose value is "Enter an integer" and reuse it.
 TODO 13 Try to optimise the printing of the String output so that you do not
           need to have "Highest is " repeated multiple times.
 TODO 14 How would creating a new variable called highest help make the code
           clearer?
*/
import java.util.*;
public class DebugFive4
{
   public static void main (String[] args)
   {
      Scanner input = new Scanner(System.in);
      int one, two, three, four;
      String str, output;
      System.out.println("Enter an integer");
      str = input.next();
      one = Integer.parseInt(str);
      System.out.println("Enter an integer");
      str = input.next();
      two = Integer.parseInt(str);
      System.out.println("Enter an integer");
      str = input.next();
      three = Integer.parseInt(str);
      System.out.println("Enter an integer");
      str = input.next();
      four = Integer.parseInt(str);
      if(one > two > one > three > one > four)
         output = "Highest is " + four;
      else
         if(two > one && two > three !! two > four)
            output = "Highest is " + three;
         else
           if(three > one && three > two && three > four)
              output = "Highest is " + three;
           else
              output = "Highest is " + four;
      System.out.println(output);
   }
}


