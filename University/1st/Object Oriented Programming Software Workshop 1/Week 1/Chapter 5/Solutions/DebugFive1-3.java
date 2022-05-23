//  DebugFive1
// Adds your lunch bill
// Burger and hot dog are $2.59
// Grilled cheese and fish are $1.99
// Fries are 89 cents
import java.util.*;
public class  DebugFive1
{
   public static void main(String args[])
   {
      Scanner input = new Scanner(System.in);
      final double HIGH_PRICE = 2.59;
      final double MED_PRICE = 1.99;
      final double LOW_PRICE = 0.89;
      String usersChoiceString;
      int usersChoice;
      double bill = 0.0;
      System.out.println("Order please\n1 - Burger\n2 - Hotdog" +
      "\n3 - Grilled cheese\n4 - Fish sandwich");
      usersChoiceString = input.next();
      usersChoice = Integer.parseInt(usersChoiceString);
      if(usersChoice == 1 || usersChoice == 2)
         bill = bill + HIGH_PRICE;
      else
         bill = bill + MED_PRICE;
      System.out.println( "Fries with that?\n1 - Yes\n2 - No");
      usersChoiceString = input.next();
      usersChoice = Integer.parseInt(usersChoiceString);
      if (usersChoice == 1)
          bill = bill + LOW_PRICE;
      System.out.println("Bill is " + bill);
   }
}
