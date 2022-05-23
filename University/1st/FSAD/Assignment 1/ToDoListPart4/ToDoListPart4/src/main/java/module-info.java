module fsd.week3.todolistpart4 {
    requires javafx.controls;
    requires javafx.fxml;


    opens fsd.week3.todolistpart4 to javafx.fxml;
    exports fsd.week3.todolistpart4;
}