/* Start with a penny
 double it every day
 how much do you have in a 30-day month?

 TODO 1 Open the TODO tab.
 TODO 2 Fix any problems you can see in the code.
 TODO 3 Use the Problems tab to see if you can make any further improvements.
 TODO 4 Use Code > Optimize Imports and note the before-and-after difference.
 TODO 5 Replace print statements with printf using %n for a new line.
 TODO 6 Run Inspect Code... from the Code menu on this file.
 TODO 7 Run Code > Reformat Code and do a before-and-after comparison.
 TODO 8 Adjust the printf template from %s to %S to print in ALL CAPS.
 TODO 9 Break the code into logical blocks, known as paragraphing, by adding
          a blank line to separate logical blocks. Separate the variable
          declarations from the while loop.
 TODO 10 Place the cursor on different parts of the code and use the Context
            Actions (light bulb) to see what IntelliJ suggests you can change.
            Play with it but make sure your program still does what it is
            supposed to do. Make use of the Undo command.
 */
public class DebugSix1
{
   public static void main(String[] args)
   {
      final int DAYS = 30;
      double money = 0.01;
      int day = 1;
      while(day > DAYS);
      {
         money = 2 * money;
         ++days;
         System.out.println("After day " + day +
            " you have " + moneyAmt);
      }
   }
}