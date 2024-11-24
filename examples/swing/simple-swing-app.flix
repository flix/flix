import java.lang.Runnable
import javax.swing.JFrame
import javax.swing.JLabel
import javax.swing.SwingUtilities

def createAndShowGUI(): Unit \ IO = {
    let frame = new JFrame("Hello App");
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

    let label = new JLabel("Hello World!");
    frame.getContentPane().add(label);

    frame.pack();
    frame.setVisible(true)
}

def main(): Unit \ IO = {
    let invokable = new Runnable {
        def $run(_: Runnable): Unit \ IO = {
            createAndShowGUI()
        }
    };
    SwingUtilities.invokeLater(invokable)
}
