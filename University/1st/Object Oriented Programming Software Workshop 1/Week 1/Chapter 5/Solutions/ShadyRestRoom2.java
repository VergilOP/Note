import java.util.Scanner;
public class ShadyRestRoom2
{
   public static void main (String args[])
   {
      int selection;
      int price;
      String result;
      String view = "no";
      final int QUEEN = 1, KING = 2, SUITE = 3;
      final int QPRICE = 125, KPRICE = 139,
         SPRICE = 165;
      final String QSTRING = "Queen bed", KSTRING = "King bed",
         SSTRING = "Suite with a king bed and pull-out couch",
         INVALIDSTRING = "an invalid option";
      final int LAKE = 1, PARK = 2;
      final int LPREMIUM = 15;
      final String LSTRING = "a lake", PSTRING = "a park",
         VIEW_ERRORSTRING = "an invalid view, so using lake";
      Scanner in = new Scanner(System.in);
      System.out.println("\t\n\nMenu\n");
      System.out.println("(" + QUEEN + ") " + QSTRING);
      System.out.println("(" + KING + ") " + KSTRING);
      System.out.println("(" + SUITE + ") " + SSTRING);
      System.out.print("Enter Selection (1, 2, or 3) >> ");
      selection = in.nextInt();
      if(selection == QUEEN)
      {
         result = QSTRING;
         price = QPRICE;
      }
      else
         if(selection == KING)
         {
            result = KSTRING;
            price = KPRICE;
         }
         else
            if (selection == SUITE)
            {
                result = SSTRING;
                price = SPRICE;
            }
            else
            {
                result = INVALIDSTRING;
                price = 0;
             }
      if(price != 0)
      {
         System.out.println("Please choose a view:");
         System.out.println("(" + LAKE + ") " + LSTRING);
         System.out.println("(" + PARK + ") " + PSTRING);
         System.out.print("Enter Selection (1 or 2) >> ");
         selection = in.nextInt();
         if(selection == LAKE)
         {
            view = LSTRING;
            price += LPREMIUM;
         }
         else
            if(selection == PARK)
            {
               view = PSTRING;
            }
            else
            {
               price += LPREMIUM;
               view = VIEW_ERRORSTRING;
            }
      }
      System.out.println("You selected " + result +
          " with " + view + " view for $" + price);
   }
}
