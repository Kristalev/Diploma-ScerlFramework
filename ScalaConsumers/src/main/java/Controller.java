import akka.actor.ActorRef;
import akka.actor.ActorSystem;
import akka.actor.Inbox;
import akka.actor.Props;
import bigdata.MainConsumer;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import scala.Tuple2;
import scala.concurrent.duration.Duration;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

public class Controller {

    private scala.collection.mutable.HashSet<String>  setNodes = new scala.collection.mutable.HashSet<>();
    final ActorSystem system = ActorSystem.create("ScalaConsumer");
    final ActorRef clientActor = system.actorOf(Props.create(MainConsumer.class), "bigdata.MainConsumer");
    private final Inbox inbox = Inbox.create(system);


    @FXML
    private Label lableGlobalMin;

    @FXML
    private Label lableLocalMin;

    @FXML
    void clickStartButton(ActionEvent event) {
        String mes = "Start system";
        inbox.send(clientActor, mes);
    }

    @FXML
    void clickUpdateButton(ActionEvent event) {
        inbox.send(clientActor, "Get min");
        try {
            Object answ = inbox.receive(Duration.create(10, TimeUnit.SECONDS));
            if (answ instanceof Tuple2) {
                Tuple2 t = (Tuple2) answ;
                Integer globalMin = (Integer) t._1();
                Integer localMin = (Integer) t._2();
                lableGlobalMin.setText(globalMin.toString());
                lableLocalMin.setText(localMin.toString());
            }
        } catch (TimeoutException e) {
            lableGlobalMin.setText("Very long answer");
            lableLocalMin.setText("Very long answer");
        }
    }

}

