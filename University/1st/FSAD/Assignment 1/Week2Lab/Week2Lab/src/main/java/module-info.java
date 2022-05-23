module fsd.week2.week2lab {
    requires javafx.controls;
    requires javafx.fxml;


    opens fsd.week2.week2lab to javafx.fxml;
    exports fsd.week2.week2lab;
}