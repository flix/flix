package ca.uwaterloo.flix;

public class SelectRule {
    private String ident;
    private Channel channel;
    private Object expression;

    public SelectRule(String ident, Channel channel, Object expression) {
        this.ident = ident;
        this.channel = channel;
        this.expression = expression;
    }

    public String getIdent() {
        return ident;
    }

    public Channel getChannel() {
        return channel;
    }

    public Object getExpression() {
        return expression;
    }
}
