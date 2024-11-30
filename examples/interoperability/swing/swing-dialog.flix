import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.lang.Runnable
import javax.swing.JButton
import javax.swing.JFrame
import javax.swing.JOptionPane
import javax.swing.SwingUtilities

def main(): Unit \ IO = {
    createAndShowGUI()
}

def createAndShowGUI(): Unit \ IO = {
    let frame = new JFrame("Dialog App");
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

    let button = button("Click me to open a dialog", _ -> showDialog(title = "Are you sure?", content = "The button was clicked. Pick an option", frame));

    frame.getContentPane().add(button);
    frame.pack();
    frame.setVisible(true)
}

def button(content: String, onClick: ActionEvent -> Unit \ ef): JButton \ IO = {
    let button = new JButton();
    button.setText(content);
    let listener = new ActionListener {
        def actionPerformed(_: ActionListener, evt: ActionEvent): Unit \ ef = onClick(evt)
    };
    button.addActionListener(listener);
    button
}

def showDialog(title: { title = String }, content: { content = String }, frame: JFrame): Unit \ IO = {
    JOptionPane.showConfirmDialog(frame, content#content, title#title, JOptionPane.YES_NO_CANCEL_OPTION);
    ()
}
