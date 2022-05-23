/* Exercise 11
 * JDBC driver postgresql-42.3.3.jar required
 * Date: 25 Apr 2022
 */

//1. Import
import java.sql.*;
import java.util.ArrayList;
import java.util.Scanner;

public class Ex11 {
    private static Connection con;
    /*
     * This method returns an ArrayList that contains titles of all records in the
     * album table with price greater than the provided budget
     */
    public static ArrayList<String> getTitles(double budget) {
       //Complete this method
    }
    
    //This method executes a query and returns a  ResultSet
    public static ResultSet executeThisQuery(String sql, double budget) {
        ResultSet rs = null;
        try {
            PreparedStatement pstmt = con.prepareStatement(sql);
            pstmt.setDouble(1, budget);
            rs = pstmt.executeQuery();

        } catch (SQLException e) {
            e.printStackTrace();
        }
        return rs;
    }
    //This method establishes a DB connection & returns a boolean status
    public static boolean establishDBConnection() {
        // Update the following with your postgres id and password
        String USERNAME = "postgres";
        String PASSWORD = "password";
        String URL = "jdbc:postgresql://localhost:5432/Music";
        try {
            // 2. Initialize driver
            Class.forName("org.postgresql.Driver");
            // 3. Open connection
            con = DriverManager.getConnection(URL, USERNAME, PASSWORD);
            return con.isValid(2);// 2 seconds timeout

        } catch (SQLException e) {
            e.printStackTrace();
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
        return false;

    }
    //This method prints all of the values inside an ArrayList
    public static void print(ArrayList<String> titles) {
        if (titles.size() == 0)
            System.out.print("No record found");
        else
            System.out.println("Titles are:");
            for (int i = 0; i < titles.size(); i++)
                System.out.println(titles.get(i));

    }
    //Main method
    public static void main(String[] args) {
        if (establishDBConnection()) {
            Scanner myInput = new Scanner( System.in );
            System.out.println("Please enter your budget");
            double budget = myInput.nextDouble();
            print(getTitles(budget));
        } else
            System.out.println("Connection cannot be established");
        try {
            con.close();

        } catch (SQLException e) {
            e.printStackTrace();
        }
    }
}

