package fsd.assignment.assignment1.datamodel;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Iterator;

public class StudentData {
    /* TODO: include an instance of StudentData
    */
    private static StudentData instance;
    private static String filename = "student-data.txt";

    private ObservableList<Student> students;

    public static StudentData getInstance() {
        /* TODO: complete the getter for the instance created
        */
        return null;
    }

    public ObservableList<Student> getStudents() {
        /* TODO: complete the getter for the observable arraylist
        */
        return null;
    }

    public void loadStudentData() throws IOException {
        students = FXCollections.observableArrayList();
        Path path = Paths.get(filename);
        BufferedReader br = Files.newBufferedReader(path);

        String input;

        try {
            while ((input = br.readLine()) != null) {
                /* TODO: split each input line using a tab
                 */
                String[] studentItem;
                /* TODO: using the String create each piece of data so that all the instance variables
                         have a value accordingly
                 */
                String studId;
                String yearStudy;
                String mod1;
                String mod2;
                String mod3;
                /* TODO: complete the call to the constructor by passing in the parameters
                 */
                Student studDataItem;
                /* TODO: add the studentDataItem to the students array
                 */
                //>include the statement here
            }
        } finally {
            if (br != null) {
                br.close();
            }
        }
    }

    public void storeStudentData() throws IOException {
        Path path = Paths.get(filename);
        BufferedWriter bw = Files.newBufferedWriter(path);
        try {
            /* TODO: complete the iterator
             */
            Iterator<Student> it = null;
            while (it.hasNext()) {
                /* TODO: accept the next item from the iterated list
                 */
                Student item = null;
                /* TODO: complete the write() using String.format
                         remember to separate each string with a tab
                 */
                bw.write("");
                /* TODO: once a student item is written to the file ensure that
                         the next item is stored on a new line
                 */
                //>insert statement here
            }
        } finally {
            if (bw != null) {
                bw.close();
            }
        }
    }

    public void addStudentData(Student studentToAdd){
        /* TODO: complete the addStudentData so that a student can be added
                 to students
         */
    }
    public void deleteStudent(Student stu){
        /* TODO: complete the addStudentData so that a student can be removed
                 from students
         */
    }
}
