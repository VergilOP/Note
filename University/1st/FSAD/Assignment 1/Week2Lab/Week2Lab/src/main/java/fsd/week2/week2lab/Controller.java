package fsd.week2.week2lab;

import fsd.week2.week2lab.model.DataModel;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.fxml.FXML;
import javafx.scene.control.ChoiceBox;
import javafx.scene.control.Label;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextField;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class Controller {
    private static String filename = "StoreMessages.txt";

    List<DataModel> captureDetails = new ArrayList<DataModel>();

    @FXML
    private TextField nameTextField;

    @FXML
    private TextArea commentsTextArea;

    private String name;

    private String option;

    private String comments;


    @FXML
    private ChoiceBox<String> choice;

    public void initialize(){
        choice.getSelectionModel().selectedItemProperty().addListener(new ChangeListener<String>() {
            @Override
            public void changed(ObservableValue<? extends String> observable, String oldValue, String newValue) {
                option = choice.getSelectionModel().getSelectedItem();
            }
        });
    }

    @FXML
    public void storeData()throws IOException{
        name = nameTextField.getText();
        comments = commentsTextArea.getText();
        DataModel capture = new DataModel(name, option, comments);
        captureDetails.add(capture);
        System.out.println(capture.getName()+"\t"+capture.getRequest() +"\t"+capture.getMessage());

        Path path = Paths.get(filename);
        BufferedWriter bw = Files.newBufferedWriter(path);
        try{
            Iterator<DataModel> it = captureDetails.iterator();
            while (it.hasNext()){
                DataModel item = it.next();
                bw.write(String.format("%s\t%s\t%s",
                        item.getName(),
                        item.getRequest(),
                        item.getMessage()));
                bw.newLine();
            }
        }finally {
            if (bw != null){
                bw.close();
            }
        }
    }

}