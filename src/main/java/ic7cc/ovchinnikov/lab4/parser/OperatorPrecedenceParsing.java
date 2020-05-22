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
                    put(Token.IDENT.getName(), " "); put(Token.TRUE.getName(), " "); put(Token.FALSE.getName(), " ");
                    put(Token.NOT.getName(), ".>"); put(Token.AND.getName(), ".>"); put(Token.OR.getName(), ".>");
                    put(Token.LBRACKET.getName(), " "); put(Token.RBRACKET.getName(), ".>"); put(Token.END.getName(), ".>");
                }});
        operatingPrecedenceParsingMatrix.put(Token.TRUE.getName(),
                new LinkedHashMap<>() {{
                    put(Token.IDENT.getName(), " "); put(Token.TRUE.getName(), " "); put(Token.FALSE.getName(), " ");
                    put(Token.NOT.getName(), ".>"); put(Token.AND.getName(), ".>"); put(Token.OR.getName(), ".>");
                    put(Token.LBRACKET.getName(), " "); put(Token.RBRACKET.getName(), ".>"); put(Token.END.getName(), ".>");
                }});
        operatingPrecedenceParsingMatrix.put(Token.FALSE.getName(),
                new LinkedHashMap<>() {{
                    put(Token.IDENT.getName(), " "); put(Token.TRUE.getName(), " "); put(Token.FALSE.getName(), " ");
                    put(Token.NOT.getName(), ".>"); put(Token.AND.getName(), ".>"); put(Token.OR.getName(), ".>");
                    put(Token.LBRACKET.getName(), " "); put(Token.RBRACKET.getName(), ".>"); put(Token.END.getName(), ".>");
                }});
        operatingPrecedenceParsingMatrix.put(Token.NOT.getName(),
                new LinkedHashMap<>() {{
                    put(Token.IDENT.getName(), "<."); put(Token.TRUE.getName(), "<."); put(Token.FALSE.getName(), "<.");
                    put(Token.NOT.getName(), "<."); put(Token.AND.getName(), ".>"); put(Token.OR.getName(), ".>");
                    put(Token.LBRACKET.getName(), "<."); put(Token.RBRACKET.getName(), ".>"); put(Token.END.getName(), ".>");
                }});
        operatingPrecedenceParsingMatrix.put(Token.AND.getName(),
                new LinkedHashMap<>() {{
                    put(Token.IDENT.getName(), "<."); put(Token.TRUE.getName(), "<."); put(Token.FALSE.getName(), "<.");
                    put(Token.NOT.getName(), "<."); put(Token.AND.getName(), ".>"); put(Token.OR.getName(), ".>");
                    put(Token.LBRACKET.getName(), "<."); put(Token.RBRACKET.getName(), ".>"); put(Token.END.getName(), ".>");
                }});
        operatingPrecedenceParsingMatrix.put(Token.OR.getName(),
                new LinkedHashMap<>() {{
                    put(Token.IDENT.getName(), "<."); put(Token.TRUE.getName(), "<."); put(Token.FALSE.getName(), "<.");
                    put(Token.NOT.getName(), "<."); put(Token.AND.getName(), "<."); put(Token.OR.getName(), ".>");
                    put(Token.LBRACKET.getName(), "<."); put(Token.RBRACKET.getName(), ".>"); put(Token.END.getName(), ".>");
                }});
        operatingPrecedenceParsingMatrix.put(Token.LBRACKET.getName(),
                new LinkedHashMap<>() {{
                    put(Token.IDENT.getName(), "<."); put(Token.TRUE.getName(), "<."); put(Token.FALSE.getName(), "<.");
                    put(Token.NOT.getName(), "<."); put(Token.AND.getName(), "<."); put(Token.OR.getName(), "<.");
                    put(Token.LBRACKET.getName(), "<."); put(Token.RBRACKET.getName(), "="); put(Token.END.getName(), " ");
                }});
        operatingPrecedenceParsingMatrix.put(Token.RBRACKET.getName(),
                new LinkedHashMap<>() {{
                    put(Token.IDENT.getName(), " "); put(Token.TRUE.getName(), ".>"); put(Token.FALSE.getName(), ".>");
                    put(Token.NOT.getName(), ".>"); put(Token.AND.getName(), ".>"); put(Token.OR.getName(), ".>");
                    put(Token.LBRACKET.getName(), " "); put(Token.RBRACKET.getName(), ".>"); put(Token.END.getName(), ".>");
                }});
        operatingPrecedenceParsingMatrix.put(Token.END.getName(),
                new LinkedHashMap<>() {{
                    put(Token.IDENT.getName(), "<."); put(Token.TRUE.getName(), "<."); put(Token.FALSE.getName(), "<.");
                    put(Token.NOT.getName(), "<."); put(Token.AND.getName(), "<."); put(Token.OR.getName(), "<.");
                    put(Token.LBRACKET.getName(), "<."); put(Token.RBRACKET.getName(), " "); put(Token.END.getName(), " ");
                }});

        stack = new ArrayDeque<>();
        lexer = new Lexer(path);
    }

    public String build() {
        printMatrix();
        stack.push(Token.END);
        Token token = lexer.next();
        StringBuilder rpn = new StringBuilder("");
        while (stack.size() != 1 || !token.equals(Token.END)) {
            Map<String, String> rules = operatingPrecedenceParsingMatrix.get(stack.peek().getName());
            if (Token.ERROR.getName().equals(token.getName())) {
                System.out.println("ERROR");
                token = lexer.next();
            } else {
                switch (rules.get(token.getName())) {
                    case "<.":
                    case "=":
                        stack.push(token);
                        token = lexer.next();
                        break;
                    case ".>":
                        Token tPlus1;
                        Token t;
                        do {
                            tPlus1 = stack.pop();
                            if (!tPlus1.equals(Token.LBRACKET) && !tPlus1.equals(Token.RBRACKET))
                                rpn.append(tPlus1.getSpell()).append(" ");
                            t = stack.peek();
                        } while (!operatingPrecedenceParsingMatrix.get(t.getName()).get(tPlus1.getName()).equals("<."));
                        break;
                    default:
                        System.out.println("ERROR");
                        token = lexer.next();
                        stack.clear();
                        stack.push(Token.END);
                        break;
                }
            }
        }

        return rpn.toString();
    }
    
    private void printMatrix() {
        System.out.print(String.format("%8s|", " "));
        for (Map.Entry<String, Map<String, String>> entry : operatingPrecedenceParsingMatrix.entrySet()) {
            System.out.print(String.format("%8s|", entry.getKey()));
        }
        System.out.println(String.format("\n%8s|%80s", " ", " ").replace(" ", "-"));
        for (Map.Entry<String, Map<String, String>> extEntry : operatingPrecedenceParsingMatrix.entrySet()) {
            System.out.print(String.format("%8s|", extEntry.getKey()));
            for (Map.Entry<String, String> intEntry : extEntry.getValue().entrySet()) {
                System.out.print(String.format("%8s|", intEntry.getValue()));
            }
            System.out.println(String.format("\n%8s|%80s", " ", " ").replace(" ", "-"));
        }
    }

}
