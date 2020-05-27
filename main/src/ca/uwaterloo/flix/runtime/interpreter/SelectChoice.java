package ca.uwaterloo.flix.runtime.interpreter;

// TODO: Move this to the Flix runtime.

public class SelectChoice {
  public static SelectChoice DEFAULT_CHOICE = new SelectChoice(-1, null, true);

  public int branchNumber;
  public boolean defaultChoice;
  public Object element;

  public SelectChoice(int branchNumber, Object element, boolean defaultChoice) {
    this.branchNumber = branchNumber;
    this.element = element;
    this.defaultChoice = defaultChoice;
  }

  public SelectChoice(int branchNumber, Object element) {
    this(branchNumber, element, false);
  }
}