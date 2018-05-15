package ca.uwaterloo.flix;

public class SelectResult {
    private String ident;
    private Object result;
    private Object expression;

    public SelectResult(String ident, Object result, Object expression) {
        this.ident = ident;
        this.result = result;
        this.expression = expression;
    }

    public String getIdent() {
        return ident;
    }

    public Object getResult() {
        return result;
    }

    public Object getExpression() {
        return expression;
    }
}
