package ic7cc.ovchinnikov.lab4.parser;

import ic7cc.ovchinnikov.lab4.lexer.Lexer;
import ic7cc.ovchinnikov.lab4.model.Token;

import java.util.*;

public class OperatorPrecedenceParsing {

    private final Map<String, Map<String, String>> operatingPrecedenceParsingMatrix;
    private final Deque<Token> stack;
    private final Lexer lexer;

    public OperatorPrecedenceParsing(String path) {
        operatingPrecedenceParsingMatrix = new LinkedHashMap<>();
        operatingPrecedenceParsingMatrix.put(Token.IDENT.getName(),
                new LinkedHashMap<>() {{
                    put(Token.IDENT.getName(), "e3");
                    put(Token.TRUE.getName(), "e3");
                    put(Token.FALSE.getName(), "e3");
                    put(Token.NOT.getName(), ".>");
                    put(Token.AND.getName(), ".>");
                    put(Token.OR.getName(), ".>");
                    put(Token.LBRACKET.getName(), "e3");
                    put(Token.RBRACKET.getName(), ".>");
                    put(Token.END.getName(), ".>");
                }});
        operatingPrecedenceParsingMatrix.put(Token.TRUE.getName(),
                new LinkedHashMap<>() {{
                    put(Token.IDENT.getName(), "e3");
                    put(Token.TRUE.getName(), "e3");
                    put(Token.FALSE.getName(), "e3");
                    put(Token.NOT.getName(), ".>");
                    put(Token.AND.getName(), ".>");
                    put(Token.OR.getName(), ".>");
                    put(Token.LBRACKET.getName(), "e3");
                    put(Token.RBRACKET.getName(), ".>");
                    put(Token.END.getName(), ".>");
                }});
        operatingPrecedenceParsingMatrix.put(Token.FALSE.getName(),
                new LinkedHashMap<>() {{
                    put(Token.IDENT.getName(), "e3");
                    put(Token.TRUE.getName(), "e3");
                    put(Token.FALSE.getName(), "e3");
                    put(Token.NOT.getName(), ".>");
                    put(Token.AND.getName(), ".>");
                    put(Token.OR.getName(), ".>");
                    put(Token.LBRACKET.getName(), "e3");
                    put(Token.RBRACKET.getName(), ".>");
                    put(Token.END.getName(), ".>");
                }});
        operatingPrecedenceParsingMatrix.put(Token.NOT.getName(),
                new LinkedHashMap<>() {{
                    put(Token.IDENT.getName(), "<.");
                    put(Token.TRUE.getName(), "<.");
                    put(Token.FALSE.getName(), "<.");
                    put(Token.NOT.getName(), "<.");
                    put(Token.AND.getName(), ".>");
                    put(Token.OR.getName(), ".>");
                    put(Token.LBRACKET.getName(), "<.");
                    put(Token.RBRACKET.getName(), ".>");
                    put(Token.END.getName(), ".>");
                }});
        operatingPrecedenceParsingMatrix.put(Token.AND.getName(),
                new LinkedHashMap<>() {{
                    put(Token.IDENT.getName(), "<.");
                    put(Token.TRUE.getName(), "<.");
                    put(Token.FALSE.getName(), "<.");
                    put(Token.NOT.getName(), "<.");
                    put(Token.AND.getName(), ".>");
                    put(Token.OR.getName(), ".>");
                    put(Token.LBRACKET.getName(), "<.");
                    put(Token.RBRACKET.getName(), ".>");
                    put(Token.END.getName(), ".>");
                }});
        operatingPrecedenceParsingMatrix.put(Token.OR.getName(),
                new LinkedHashMap<>() {{
                    put(Token.IDENT.getName(), "<.");
                    put(Token.TRUE.getName(), "<.");
                    put(Token.FALSE.getName(), "<.");
                    put(Token.NOT.getName(), "<.");
                    put(Token.AND.getName(), "<.");
                    put(Token.OR.getName(), ".>");
                    put(Token.LBRACKET.getName(), "<.");
                    put(Token.RBRACKET.getName(), ".>");
                    put(Token.END.getName(), ".>");
                }});
        operatingPrecedenceParsingMatrix.put(Token.LBRACKET.getName(),
                new LinkedHashMap<>() {{
                    put(Token.IDENT.getName(), "<.");
                    put(Token.TRUE.getName(), "<.");
                    put(Token.FALSE.getName(), "<.");
                    put(Token.NOT.getName(), "<.");
                    put(Token.AND.getName(), "<.");
                    put(Token.OR.getName(), "<.");
                    put(Token.LBRACKET.getName(), "<.");
                    put(Token.RBRACKET.getName(), "=");
                    put(Token.END.getName(), "e4");
                }});
        operatingPrecedenceParsingMatrix.put(Token.RBRACKET.getName(),
                new LinkedHashMap<>() {{
                    put(Token.IDENT.getName(), "<.");
                    put(Token.TRUE.getName(), "<.");
                    put(Token.FALSE.getName(), "<.");
                    put(Token.NOT.getName(), ".>");
                    put(Token.AND.getName(), ".>");
                    put(Token.OR.getName(), ".>");
                    put(Token.LBRACKET.getName(), "e3");
                    put(Token.RBRACKET.getName(), ".>");
                    put(Token.END.getName(), ".>");
                }});
        operatingPrecedenceParsingMatrix.put(Token.END.getName(),
                new LinkedHashMap<>() {{
                    put(Token.IDENT.getName(), "<.");
                    put(Token.TRUE.getName(), "<.");
                    put(Token.FALSE.getName(), "<.");
                    put(Token.NOT.getName(), "<.");
                    put(Token.AND.getName(), "<.");
                    put(Token.OR.getName(), "<.");
                    put(Token.LBRACKET.getName(), "<.");
                    put(Token.RBRACKET.getName(), "e2");
                    put(Token.END.getName(), "e1");
                }});

        stack = new ArrayDeque<>();
        lexer = new Lexer(path);
    }

    public String build() {
        printMatrix();
        stack.push(Token.END);
        List<Token> tokens = readTokensExpression();
        List<Token> rpn = new ArrayList<>();

        System.out.print("Expression: ");
        for (Token token : tokens) {
            System.out.print(token.getSpell() + " ");
        }
        System.out.println();

        int i = 0;
        Token token = tokens.get(i);
        while (!(token.equals(Token.END) && stack.peek().equals(Token.END))) {
            Map<String, String> rules = operatingPrecedenceParsingMatrix.get(stack.peek().getName());

            switch (rules.get(token.getName())) {
                case "<.":
                case "=":
                    stack.push(token);
                    i++;
                    token = tokens.get(i);
                    break;
                case ".>":
                    Token temp;
                    do {
                        temp = stack.pop();

                        if (!temp.equals(Token.LBRACKET) && !temp.equals(Token.RBRACKET)) {
                            rpn.add(temp);
                        }
                    } while (!operatingPrecedenceParsingMatrix.get(stack.peek().getName()).get(temp.getName()).equals("<."));
                    break;
                default:
                    switch (rules.get(token.getName())) {
                        case "e1":
                            System.out.println("Missing operand");
                            tokens.add(i, Token.TRUE);
                            token = tokens.get(i);
                            break;
                        case "e2":
                            System.out.println("Unbalanced right bracket");
                            tokens.remove(token);
                            token = tokens.get(i);
                            break;
                        case "e3":
                            System.out.println("Missing operator");
                            tokens.add(i, Token.AND);
                            token = Token.AND;
                            break;
                        case "e4":
                            System.out.println("Missing right bracket");
                            stack.pop();
                            break;
                    }
                    break;
            }
        }

        StringBuilder result = new StringBuilder();
        for (Token t : rpn) {
            result.append(t.getSpell()).append(" ");
        }

        return result.toString();
    }

    private List<Token> readTokensExpression() {
        List<Token> tokens = new ArrayList<>();
        do {
            tokens.add(lexer.next());
        } while (!tokens.get(tokens.size() - 1).getName().equals("END"));
        return tokens;
    }

    private void printMatrix() {
        System.out.println("Operator Precedence Matrix:\n");
        System.out.print(String.format("%8s|", " "));
        for (Map.Entry<String, Map<String, String>> entry : operatingPrecedenceParsingMatrix.entrySet()) {
            if (entry.getKey().equals("LBRACKET") || entry.getKey().equals("RBRACKET"))
                System.out.print(String.format("%8s|", entry.getKey()));
            else
                System.out.print(String.format("%5s|", entry.getKey()));
        }
        System.out.print(String.format("\n%8s|", " ").replace(" ", "-"));
        for (Map.Entry<String, Map<String, String>> entry : operatingPrecedenceParsingMatrix.entrySet()) {
            if (entry.getKey().equals("LBRACKET") || entry.getKey().equals("RBRACKET"))
                System.out.print(String.format("%8s|", " ").replace(" ", "-"));
            else
                System.out.print(String.format("%5s|", " ").replace(" ", "-"));
        }
        System.out.println();
        for (Map.Entry<String, Map<String, String>> extEntry : operatingPrecedenceParsingMatrix.entrySet()) {
            System.out.print(String.format("%8s|", extEntry.getKey()));
            for (Map.Entry<String, String> intEntry : extEntry.getValue().entrySet()) {
                if (intEntry.getKey().equals("LBRACKET") || intEntry.getKey().equals("RBRACKET"))
                    System.out.print(String.format("%8s|", intEntry.getValue()));
                else
                    System.out.print(String.format("%5s|", intEntry.getValue()));
            }
            System.out.print(String.format("\n%8s|", " ").replace(" ", "-"));
            for (Map.Entry<String, Map<String, String>> entry : operatingPrecedenceParsingMatrix.entrySet()) {
                if (entry.getKey().equals("LBRACKET") || entry.getKey().equals("RBRACKET"))
                    System.out.print(String.format("%8s|", " ").replace(" ", "-"));
                else
                    System.out.print(String.format("%5s|", " ").replace(" ", "-"));
            }
            System.out.println();
        }
    }

}
