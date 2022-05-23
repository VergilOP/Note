import java.util.Scanner;
class ElectionStatistics
{
   public static void main(String[] args)
   {
      int vote1, vote2, vote3;
      String party1, party2, party3;
      int total;
      double pct1, pct2, pct3;
      Scanner input = new Scanner(System.in);
      System.out.print("Enter name for first party >> ");
      party1 = input.nextLine();
      System.out.print("Enter votes received >> ");
      vote1 = input.nextInt();
      input.nextLine();
      System.out.print("Enter name for second party >> ");
      party2 = input.nextLine();
      System.out.print("Enter votes received >> ");
      vote2 = input.nextInt();
      input.nextLine();
      System.out.print("Enter name for third party >> ");
      party3 = input.nextLine();
      System.out.print("Enter votes received >> ");
      vote3 = input.nextInt();
      input.nextLine();
      total = vote1 + vote2 + vote3;
      pct1 = (double) vote1 / total * 100;
      pct2 = (double) vote2 / total * 100;
      pct3 = (double) vote3 / total * 100;
      System.out.println("The " + party1 + " party got " +
         pct1 + " percent of the vote");
      System.out.println("The " + party2 + " party got " +
         pct2 + " percent of the vote");
      System.out.println("The " + party3 + " party got " +
         pct3 + " percent of the vote");
   }
}