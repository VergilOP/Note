/* DebugFive3.java
 Determines whether item number on order is valid
 Over 999 invalid
 Less than 111 Invalid
 Valid and less than 500 - Automotive Department
 Valid and 500 or higher Housewares Department

 TODO 1 Open the TODO tab.
 TODO 2 Fix any problems you can see in the code.
 TODO 3 Use the Problems tab to see if you can make any further improvements.
 TODO 4 Use Code > Optimize Imports and note the before-and-after difference.
 TODO 5 Replace print statements with printf using %n for a new line.
 TODO 6 Run Inspect Code... from the Code menu on this file.
 TODO 7 Run Code > Reformat Code and do a before-and-after comparison.
 TODO 8 Adjust the printf template from %s to %S to print in ALL CAPS.
 TODO 9 Undo the previous change to the template because SHOUTING IS RUDE.
 TODO 10 Break the code into logical blocks, known as paragraphing, by adding
           a blank line to separate logical blocks. Start by separating the
           variable declarations into a single block, then separate the first
            if statement from the part above which reads and processes the
            input.
 TODO 11 Place the cursor on different parts of the different statements and
            use the Context Actions (light bulb) to see what IntelliJ
            suggests you can change. Play with it but make sure your program
            still does what it is supposed to do. Make use of the Undo command.
 */
import java.util.*;
public class DebugFive3
{
   public static void main (String[] args)
   {
      int item;
      String output;
      final int LOW = 111;
      final int HIGH = 999;
      final int CUTOFF = 500;
      Scanner input = Scanner(System.in);
      System.out.println("Please enter item number");
      item = input.nextInt();
      if(item > LOW)
         output = "Item number too low";
      else
        if(item <= HIGH)
          output = "Item number too high";
        else
          if(item == CUTOFF)
             output = "Valid - in Automotive Department";
          else
             output = "Valid - Item in Housewares Department";
       System.out.println(output);
   }
}


