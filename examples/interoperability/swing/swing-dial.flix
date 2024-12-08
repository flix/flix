import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.awt.GridLayout
import java.lang.Runnable
import javax.swing.JButton
import javax.swing.JFrame
import javax.swing.JOptionPane
import javax.swing.JSeparator
import javax.swing.SwingConstants
import javax.swing.SwingUtilities

def main(): Unit \ IO = {
    createAndShowGUI()
}

def createAndShowGUI(): Unit \ IO = {
    let frame = new JFrame("Button Dial");
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

    frame.setLayout(new GridLayout(3, 3));
    frame.getContentPane().add(mkButton("1"));
    frame.getContentPane().add(mkButton("2"));
    frame.getContentPane().add(mkButton("3"));
    frame.getContentPane().add(mkButton("4"));
    frame.getContentPane().add(mkButton("5"));
    frame.getContentPane().add(mkButton("6"));
    frame.getContentPane().add(mkButton("7"));
    frame.getContentPane().add(mkButton("8"));
    frame.getContentPane().add(mkButton("9"));

    frame.pack();
    frame.setVisible(true)
}

def mkButton(content: String): JButton \ IO = {
    let button = new JButton();
    button.setText(content);
    let listener = new ActionListener {
        def actionPerformed(_: ActionListener, _: ActionEvent): Unit = ()
    };
    button.addActionListener(listener);
    button
}
