import java.util.*;
public class DebugTwo4
{
   public static void main(String[] args)
   {
      Scanner input = new Scanner(System.in);
      String costString;
      double cost;
      final double TAX = 0.06;
      System.out.println("Enter price of item you are buying");
      costString = input.next();
      cost = Double.parseDouble(costString);
      System.out.println("With " + TAX * 100 +
      "% tax,  purchase  is $" + (cost + cost * TAX));
   }
}
