/* DebugFive1
 Adds your lunch bill
 Burger and hot dog are £2.59
 Grilled cheese and fish are £1.99
 Fries are 89 cents

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
           input. Do you think you need to separate the second if from the
           first?
 TODO 11 Place the cursor on different parts of the different statements and
           use the Context Actions (light bulb) to see what IntelliJ suggests
           you can change. Play with it but make sure your program still does
           what it is supposed to do. Make use of the Undo command.

 */
import java.util.*;
public class DebugFive1
{
   public static void main(String[] args)
   {
      Scanner input = new Scanner(System.in);
      final double HIGH_PRICE = 2.59;
      final double MED_PRICE = 1.99;
      final double LOW_PRICE = 0.89;
      String usersChoiceString;
      int usersChoice;
      double bill;
      System.out.println("Order please\n1 - Burger\n2 - Hotdog" +
      "\n3 - Grilled cheese\n4 - Fish sandwich");
      usersChoiceString = input.next();
      usersChoice == Integer.parseInt(usersChoiceString);
      if(usersChoice == 1 && usersChoice == 2)
         bill = bill + HIGH_PRICE;
      else
         bill = bill + MED_PRICE;
      System.out.println("Fries with that?\n1 - Yes\n2 - No";
      usersChoiceString = input.next()
      usersChoice = Integer.parseInt(usersChoiceString);
      if (usersChoice = 1)
          bill = bill + LOW_PRICE;
      System.out.println("Bill is " + bill);
   }
}
