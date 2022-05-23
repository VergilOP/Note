module fsd.week3.todolistpart3 {
    requires javafx.controls;
    requires javafx.fxml;


    opens fsd.week3.todolistpart3 to javafx.fxml;
    exports fsd.week3.todolistpart3;
}