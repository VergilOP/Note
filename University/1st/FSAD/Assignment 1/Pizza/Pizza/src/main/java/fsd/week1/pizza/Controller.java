package fsd.week1.pizza;

import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.*;

import java.net.URL;
import java.util.ArrayList;
import java.util.ResourceBundle;

public class Controller implements Initializable {

    @FXML
    private TextField nameField;
    @FXML
    private RadioButton thickCrust;

    @FXML
    private RadioButton thinCrust;

    @FXML
    private ChoiceBox<String> choices;

    @FXML
    private Label branchLabel;

    private String pizzaChoices [] = {"Cheese", "Peppers", "Tofu"};

    @FXML
    private ListView<String> myListView;

    private String branches [] = {"Selly Oak", "Bournville", "Solihull"};

    private double cost;

    String whichBranch;

    //initialise a controller after the root has been processed, i.e. window is up and running
    public void initialize(URL location, ResourceBundle rb) {
        choices.getItems().addAll(pizzaChoices);
        myListView.getItems().addAll(branches);
        myListView.getSelectionModel().selectedItemProperty().addListener(new ChangeListener<String>() {
            @Override
            public void changed(ObservableValue<? extends String> observable, String oldValue, String newValue) {
                whichBranch = myListView.getSelectionModel().getSelectedItem();
                branchLabel.setText(whichBranch);
            }
        });
        cost = 0.00;
        choices.setOnAction(this::getChoice);

    }

    @FXML
    public void radioSelect(ActionEvent e) {

        if (thickCrust.isSelected()) {
            cost += 0.90;
        }

        if (thinCrust.isSelected()) {
            cost += 0.75;
        }
    }
    public void getChoice(ActionEvent e) {
    String choice = choices.getValue();
        if (choice.equals("Cheese"))
            cost += 1.00;
        if (choice.equals("Peppers"))
            cost += 0.50;
        if (choice.equals("Tofu"))
            cost += 1.20;
    }

    @FXML
    public void onButtonPressed() {
        System.out.println("Thank you, " +nameField.getText()
                +" The total cost of the pizza is £" +cost +" " +whichBranch +" will deliver");
    }

}