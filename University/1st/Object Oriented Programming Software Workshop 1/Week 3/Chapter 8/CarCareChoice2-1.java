import java.util.*;
public class CarCareChoice2
{
   public static void main (String[] args)
   {
      Scanner input = new Scanner(System.in);
      boolean isMatch = false;
      String[] items =  { "oil change", "tire rotation",
         "battery check", "brake inspection"};
      int[] prices = {25, 22, 15, 5};
      int x;
      int matchIndex = 0;
      String menu = "Enter selection:";
      for(x = 0; x < items.length; ++x)
        menu += "\n   " + items[x];
      System.out.println(menu);
      String selection = input.nextLine();
      for (x = 0; x < items.length; x++)
      if(selection.substring(0, 3).equals(items[x].substring(0, 3)))
      {
	  isMatch = true;
	  matchIndex = x;
      }
      if(isMatch)
          System.out.println(selection + " price is $" + prices[matchIndex]);
      else
          System.out.println("Invalid Entry");
  }
}
