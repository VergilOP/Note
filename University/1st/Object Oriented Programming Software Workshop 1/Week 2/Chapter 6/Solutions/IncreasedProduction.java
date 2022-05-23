public class IncreasedProduction
{
   public static void main (String[] args)
   {
      int month;
      int monthMetGoal = 0;
      double production = 4_000;
      double netProfit;
      final double PRODUCTION_INCREASE = 0.06;
      final double GOAL = 7_000;
      final int MONTHS = 24;
      boolean isGoalMet = false;
      for(month = 1; month <= MONTHS; ++month)
      {
         production += production * PRODUCTION_INCREASE;
         if(production > GOAL && !isGoalMet)
         {
            monthMetGoal = month;
            isGoalMet = true;
         }
         System.out.println("Month " + month + "     Predicted production is " +
            production);

      }
      System.out.println("\nThe month in which production exceeds " +
         GOAL + " is month " + monthMetGoal);
   }
}

