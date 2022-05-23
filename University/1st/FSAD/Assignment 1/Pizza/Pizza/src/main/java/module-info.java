module fsd.week1.pizza {
    requires javafx.controls;
    requires javafx.fxml;


    opens fsd.week1.pizza to javafx.fxml;
    exports fsd.week1.pizza;
}