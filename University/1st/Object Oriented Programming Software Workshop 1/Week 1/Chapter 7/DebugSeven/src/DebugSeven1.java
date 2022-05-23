/* Makes String comparisons

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
           use the Context Actions (light bulb) to see what IntelliJ suggests
           you can change. Play with it but make sure your program still does
           what it is supposed to do. Make use of the Undo command.
*/

public class DebugSeven1
{
   public static void main(String[] args)
   {
      String name1 = "Roger";
      String name2 = "Roger";
      String name3 = "Stacy";
      if(name1.equals(name2))
        System.out.println(name1 + " and " + name2 +
          " are the same");
      if(name1.equals(name3))
        System.out.println(name1 + " and " + name2 +
          " are the same");
      if(name1.equals("roger));
        System.out.println(name1 + " and 'roger' are the same");
      if(name1.equals("Roger"));
        System.out.println(name1 + " and 'Roger' are the same");
   }
}
