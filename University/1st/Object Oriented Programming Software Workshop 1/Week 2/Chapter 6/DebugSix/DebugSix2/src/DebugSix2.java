/* Display every character between Unicode 65 and 122
 Start new line after 20 characters

 TODO 1 Open the TODO tab.
 TODO 2 Fix any problems you can see in the code.
 TODO 3 Use the Problems tab to see if you can make any further improvements.
 TODO 4 Use Code > Optimize Imports and note the before-and-after difference.
 TODO 5 Replace the last print statement with printf using %n instead of \n
 TODO 6 Run Inspect Code... from the Code menu on this file.
 TODO 7 Run Code > Reformat Code and do a before-and-after comparison.
 TODO 8 Adjust the printf template from %s to %S to print in ALL CAPS.
 TODO 9 Undo the previous change to the template because SHOUTING IS RUDE.
 TODO 10 Break the code into logical blocks, known as paragraphing, by adding
           a blank line to separate logical blocks. Start by separating the
           variable declarations into a single block, then separate the for
            loop from the part above.
 TODO 11 Move the constants so that they are declared before the variables.
 TODO 12 Place the cursor on different parts of the different statements and
            use the Context Actions (light bulb) to see what IntelliJ
            suggests you can change. Play with it but make sure your program
            still does what it is supposed to do. Use it on the if statement
            especially the conditions. Also jry changing the type of loop
            (and back) and see if you can get IntelliJ to unroll the loop for
            you. Make use of the Undo command to restore previous versions.
 */
public class DebugSix2
{
   public static void main(String[] args)
   {
      char letter;
      int a;
      final int MIN = 65;
      final int MAX = 122;
      final int NUMPERLINE = 200;
      final int STOPLINE1 = 0;
      final int STOPLINE2 = STOPLINE1 + NUMPERLINE;
      for(a = MIN; a <= MAX; a++)
      {
        letter = (char)a;
        System.out.print("  " + letter);
        if((a == STOPLINE1) && (a == STOPLINE)
           System.out.println();
      }
      System.out.println("\nEnd of application");
    }
}