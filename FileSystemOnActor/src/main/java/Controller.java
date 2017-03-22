import akka.actor.ActorRef;
import akka.actor.ActorSystem;
import akka.actor.Inbox;
import akka.actor.Props;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import scala.Tuple2;
import scala.Tuple3;
import scala.Tuple4;
import scala.collection.immutable.List;
import scala.collection.immutable.Set;
import scala.concurrent.duration.Duration;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import filesystem.ClientForGUI;


public class Controller {

    private scala.collection.mutable.HashSet<String>  setNodes = new scala.collection.mutable.HashSet<>();
    final ActorSystem system = ActorSystem.create("FileSystemOnActor");
    final ActorRef clientActor = system.actorOf(Props.create(ClientForGUI.class), "clientActor");
    private final Inbox inbox = Inbox.create(system);
    @FXML
    private Label labelNodes;

    @FXML
    private ListView<String> listViewCompliteCommand;

    @FXML
    private TreeView<String> treeViewNodesFiles;

    @FXML
    private TextField textFieldNode;

    @FXML
    private Button buttonAddNode;

    @FXML
    private Button buttonStartFS;

    @FXML
    private TextField textFieldCopy;

    @FXML
    private TextField textFieldRename;

    @FXML
    void clickOnButtonAddNode(ActionEvent event) {
        if(!textFieldNode.getText().equals("")){
            setNodes.add(textFieldNode.getText());
            labelNodes.setText(setNodes.reversed().mkString("; "));
            textFieldNode.clear();
        }
    }

    @FXML
    void clickOnButtonStartFS(ActionEvent event) {
        textFieldNode.setDisable(true);
        buttonAddNode.setDisable(true);
        buttonStartFS.setDisable(true);
        final scala.Tuple2<String,scala.collection.mutable.Set> tuple = scala.Tuple2.apply("init",setNodes);
        inbox.send(clientActor, tuple);
        try {
            Object answ = inbox.receive(Duration.create(12, TimeUnit.SECONDS));
            if (answ instanceof List){
                List listTuples = (List) answ;
                TreeItem<String> rootTree = new TreeItem<>("FileSystem");
                for(int i = 0 ; i < listTuples.length() ; i++){
                    if (listTuples.apply(i) instanceof Tuple2){
                        Tuple2 t = (Tuple2)listTuples.apply(i);
                        String node = (String)t._1();
                        Set<String> setFile = (Set<String>)t._2();
                        List<String> listFile = setFile.toList();
                        TreeItem<String> nodeTree = new TreeItem<>(node);
                        for(int j = 0 ; j  < listFile.length() ; j++){
                            TreeItem<String> item = new TreeItem<>(listFile.apply(j));
                            nodeTree.getChildren().add(item);
                        }
                        rootTree.getChildren().add(nodeTree);
                    }
                }
                treeViewNodesFiles.setRoot(rootTree);
            }else{
                if (answ instanceof String) {
                    String str = (String) answ;
                    textFieldNode.setText(str);
                }
            }
        } catch (java.util.concurrent.TimeoutException e) {
            // timeout
            textFieldNode.setText("Что-то пошло не так?!");
        }
    }

    @FXML
    void clickOnCopyButton(ActionEvent event) {
        String oldFile = treeViewNodesFiles.getSelectionModel().getSelectedItem().getValue();
        String node = treeViewNodesFiles.getSelectionModel().getSelectedItem().getParent().getValue();
        String newFile = textFieldCopy.getText();
        final Tuple4<String,String,String,String> mes = Tuple4.apply("copy",node,oldFile,newFile);
        inbox.send(clientActor, mes);
        textFieldCopy.clear();
    }

    @FXML
    void clickOnRenameButton(ActionEvent event) {
        String oldFile = treeViewNodesFiles.getSelectionModel().getSelectedItem().getValue();
        String node = treeViewNodesFiles.getSelectionModel().getSelectedItem().getParent().getValue();
        String newFile = textFieldRename.getText();
        final Tuple4<String,String,String,String> mes = Tuple4.apply("rename",node,oldFile,newFile);
        inbox.send(clientActor, mes);
        textFieldRename.clear();
    }

    @FXML
    void clickOnDeleteButton(ActionEvent event) {
        String file = treeViewNodesFiles.getSelectionModel().getSelectedItem().getValue();
        String node = treeViewNodesFiles.getSelectionModel().getSelectedItem().getParent().getValue();
        final Tuple3<String,String,String> mes = Tuple3.apply("delete",node,file);
        inbox.send(clientActor, mes);
    }

    private  Thread myThread = new Thread(()-> {
        while(true){
            try {
                Object answ = inbox.receive(Duration.create(5, TimeUnit.MILLISECONDS));
                if (answ instanceof String){
                    String s = (String) answ;
                    listViewCompliteCommand.getItems().add(s);
                }else{
                    if (answ instanceof Tuple2) {
                        Tuple2 t = (Tuple2) answ;
                        TreeItem<String> rootTree = treeViewNodesFiles.getRoot();
                        String node = (String) t._1();
                        List<String> listFile = (List<String>) t._2();
                        TreeItem<String> nodeTree = new TreeItem<>(node);
                        rootTree.getChildren().remove(
                                rootTree.getChildren().stream()
                                        .filter(x -> x.getValue().equals(node)).findFirst().orElse(null));
                        for (int j = 0; j < listFile.length(); j++) {
                            TreeItem<String> item = new TreeItem<>(listFile.apply(j));
                            nodeTree.getChildren().add(item);
                        }
                        rootTree.getChildren().add(nodeTree);
                    }
                }
            }catch (TimeoutException e) {continue;}
        }
    });

    @FXML
    void clickOnRunButton(ActionEvent event) {
        inbox.send(clientActor, "exe");
        if (!myThread.isAlive())
            myThread.start();
    }

}

