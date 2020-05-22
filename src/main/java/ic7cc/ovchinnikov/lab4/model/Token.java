package ic7cc.ovchinnikov.lab4.model;

public class Token extends Terminal {

    public static final Token LBRACKET = new Token("LBRACKET", "(");
    public static final Token RBRACKET = new Token("RBRACKET", ")");
    public static final Token OR = new Token("OR", "!");
    public static final Token AND = new Token("AND", "&");
    public static final Token NOT = new Token("NOT", "~");
    public static final Token EQUAL = new Token("EQUAL", "==");
    public static final Token NOT_EQUAL = new Token("NOT_EQUAL", "~=");
    public static final Token LESS_EQUAL = new Token("LESS_EQUAL", "<=");
    public static final Token MORE_EQUAL = new Token("MORE_EQUAL", ">=");
    public static final Token LESS = new Token("LESS", "<");
    public static final Token MORE = new Token("MORE", ">");
    public static final Token TRUE = new Token("TRUE", "true");
    public static final Token FALSE = new Token("FALSE", "false");
    public static final Token IDENT = new Token("IDENT", "");
    public static final Token ERROR = new Token("ERROR", "");
    public static final Token END = new Token("END", "\0");

    private Token(String name, String spell) {
        super(name, spell);
    }

    public static Token buildAtom(String spell) {
        if (!(spell.contains("!") && spell.contains("~") && spell.contains("&") && spell.contains("=") &&
            spell.contains("(") && spell.contains(")") && spell.contains("true") && spell.contains("false") &&
                spell.contains("==") && spell.contains("~=") && spell.contains("<=") && spell.contains(">=") &&
                spell.contains("<") && spell.contains(">")))
            return new Token("IDENT", spell);
        throw new UnsupportedOperationException();
    }

    public static Token buildError(String spell, Integer rowNumber, Integer columnNumber) {
        return new Token("ERROR", String.format("%s: (%d, %d)", spell, rowNumber, columnNumber));
    }

    public static boolean isValidToken(Token token) {
        return token.equals(LBRACKET) || token.equals(RBRACKET) || token.equals(OR) || token.equals(AND) ||
                token.equals(NOT) || token.equals(TRUE) || token.equals(FALSE) || token.getName().equals(IDENT.getName()) ||
                token.equals(EQUAL) || token.equals(NOT_EQUAL) || token.equals(LESS_EQUAL) || token.equals(MORE_EQUAL) ||
                token.equals(LESS) || token.equals(MORE);
    }

    public static boolean isOperator(Token token) {
        return token.equals(NOT) || token.equals(OR) || token.equals(AND);
    }

    public static boolean isLiteral(Token token) {
        return token.equals(TRUE) || token.equals(FALSE);
    }

    public static Token buildRelation(String spell) {
        if (spell.contains("==") || spell.contains("~=") || spell.contains("<=") || spell.contains(">=") ||
                spell.contains("<") || spell.contains(">"))
            return new Token("RELATIONSHIP", spell);
        throw new UnsupportedOperationException();
    }

    @Override
    public String toString() {
        return "Token{" +
                "spell='" + super.getSpell() + '\'' + ", " +
                "name='" + super.getName() + '\'' +
                '}';
    }
}
