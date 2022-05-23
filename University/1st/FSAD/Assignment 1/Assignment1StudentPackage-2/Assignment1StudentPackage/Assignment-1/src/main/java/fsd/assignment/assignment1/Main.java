package fsd.assignment.assignment1;

import fsd.assignment.assignment1.datamodel.StudentData;
import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.stage.Stage;

import java.io.IOException;
//this class is given to you
public class Main extends Application {
    @Override
    public void start(Stage stage) throws IOException {
        FXMLLoader fxmlLoader = new FXMLLoader(Main.class.getResource("main-view.fxml"));
        Scene scene = new Scene(fxmlLoader.load(), 700, 550);
        stage.setTitle("Student Capture of Module Choices:...");
        stage.setScene(scene);
        stage.show();
    }

    public static void main(String[] args) {
        launch();
    }

    @Override
    public void stop() throws IOException{
        try{
            StudentData.getInstance().storeStudentData();
        }catch (IOException e){
            System.out.println(e.getMessage());
        }
    }

    @Override
    public void init() throws IOException{
        try{ //load the data in
            StudentData.getInstance().loadStudentData();
        }catch (IOException e){
            System.out.println(e.getMessage());
        }
    }
}