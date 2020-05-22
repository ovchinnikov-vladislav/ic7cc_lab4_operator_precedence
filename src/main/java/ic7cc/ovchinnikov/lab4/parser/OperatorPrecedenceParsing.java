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
                    put(Token.IDENT.getName(), "e3"); put(Token.TRUE.getName(), "e3"); put(Token.FALSE.getName(), "e3");
                    put(Token.NOT.getName(), "e5"); put(Token.AND.getName(), "e5"); put(Token.OR.getName(), "e5");
                    put(Token.LBRACKET.getName(), "e3"); put(Token.RBRACKET.getName(), ".>"); put("RELATIONSHIP", ".>");
                    put(Token.END.getName(), ".>");
                }});
        operatingPrecedenceParsingMatrix.put(Token.TRUE.getName(),
                new LinkedHashMap<>() {{
                    put(Token.IDENT.getName(), "e3"); put(Token.TRUE.getName(), "e3"); put(Token.FALSE.getName(), "e3");
                    put(Token.NOT.getName(), ".>"); put(Token.AND.getName(), ".>"); put(Token.OR.getName(), ".>");
                    put(Token.LBRACKET.getName(), "e3"); put(Token.RBRACKET.getName(), ".>"); put("RELATIONSHIP", "e5");
                    put(Token.END.getName(), ".>");
                }});
        operatingPrecedenceParsingMatrix.put(Token.FALSE.getName(),
                new LinkedHashMap<>() {{
                    put(Token.IDENT.getName(), "e3"); put(Token.TRUE.getName(), "e3"); put(Token.FALSE.getName(), "e3");
                    put(Token.NOT.getName(), ".>"); put(Token.AND.getName(), ".>"); put(Token.OR.getName(), ".>");
                    put(Token.LBRACKET.getName(), "e3"); put(Token.RBRACKET.getName(), ".>"); put("RELATIONSHIP", "e5");
                    put(Token.END.getName(), ".>");
                }});
        operatingPrecedenceParsingMatrix.put(Token.NOT.getName(),
                new LinkedHashMap<>() {{
                    put(Token.IDENT.getName(), "e5"); put(Token.TRUE.getName(), "<."); put(Token.FALSE.getName(), "<.");
                    put(Token.NOT.getName(), "<."); put(Token.AND.getName(), ".>"); put(Token.OR.getName(), ".>");
                    put(Token.LBRACKET.getName(), "<."); put(Token.RBRACKET.getName(), ".>"); put("RELATIONSHIP", "<.");
                    put(Token.END.getName(), ".>");
                }});
        operatingPrecedenceParsingMatrix.put(Token.AND.getName(),
                new LinkedHashMap<>() {{
                    put(Token.IDENT.getName(), "e5"); put(Token.TRUE.getName(), "<."); put(Token.FALSE.getName(), "<.");
                    put(Token.NOT.getName(), "<."); put(Token.AND.getName(), ".>"); put(Token.OR.getName(), ".>");
                    put(Token.LBRACKET.getName(), "<."); put(Token.RBRACKET.getName(), ".>"); put("RELATIONSHIP", "<.");
                    put(Token.END.getName(), ".>");
                }});
        operatingPrecedenceParsingMatrix.put(Token.OR.getName(),
                new LinkedHashMap<>() {{
                    put(Token.IDENT.getName(), "e5"); put(Token.TRUE.getName(), "<."); put(Token.FALSE.getName(), "<.");
                    put(Token.NOT.getName(), "<."); put(Token.AND.getName(), "<."); put(Token.OR.getName(), ".>");
                    put(Token.LBRACKET.getName(), "<."); put(Token.RBRACKET.getName(), ".>"); put("RELATIONSHIP", "<.");
                    put(Token.END.getName(), ".>");
                }});
        operatingPrecedenceParsingMatrix.put(Token.LBRACKET.getName(),
                new LinkedHashMap<>() {{
                    put(Token.IDENT.getName(), "<."); put(Token.TRUE.getName(), "<."); put(Token.FALSE.getName(), "<.");
                    put(Token.NOT.getName(), "<."); put(Token.AND.getName(), "<."); put(Token.OR.getName(), "<.");
                    put(Token.LBRACKET.getName(), "<."); put(Token.RBRACKET.getName(), "="); put("RELATIONSHIP", "<.");
                    put(Token.END.getName(), "e4");
                }});
        operatingPrecedenceParsingMatrix.put(Token.RBRACKET.getName(),
                new LinkedHashMap<>() {{
                    put(Token.IDENT.getName(), "e3"); put(Token.TRUE.getName(), "e3"); put(Token.FALSE.getName(), "e3");
                    put(Token.NOT.getName(), ".>"); put(Token.AND.getName(), ".>"); put(Token.OR.getName(), ".>");
                    put(Token.LBRACKET.getName(), "e3"); put(Token.RBRACKET.getName(), ".>"); put("RELATIONSHIP", ".>");
                    put(Token.END.getName(), ".>");
                }});
        operatingPrecedenceParsingMatrix.put("RELATIONSHIP",
                new LinkedHashMap<>() {{
                    put(Token.IDENT.getName(), "<."); put(Token.TRUE.getName(), "e5"); put(Token.FALSE.getName(), "e5");
                    put(Token.NOT.getName(), ".>"); put(Token.AND.getName(), ".>"); put(Token.OR.getName(), ".>");
                    put(Token.LBRACKET.getName(), "<."); put(Token.RBRACKET.getName(), ".>"); put("RELATIONSHIP", "e5");
                    put(Token.END.getName(), ".>");
                }});
        operatingPrecedenceParsingMatrix.put(Token.END.getName(),
                new LinkedHashMap<>() {{
                    put(Token.IDENT.getName(), "<."); put(Token.TRUE.getName(), "<."); put(Token.FALSE.getName(), "<.");
                    put(Token.NOT.getName(), "<."); put(Token.AND.getName(), "<."); put(Token.OR.getName(), "<.");
                    put(Token.LBRACKET.getName(), "<."); put(Token.RBRACKET.getName(), "e2"); put("RELATIONSHIP", "<.");
                    put(Token.END.getName(), "e1");
                }});

        stack = new ArrayDeque<>();
        lexer = new Lexer(path);
    }

    public String build() {
        printMatrix();
        stack.push(Token.END);
        List<Token> tokens = readTokensExpression();
        StringBuilder rpn = new StringBuilder("");

        int i = 0;
        Token token = tokens.get(i);
        while (stack.size() != 1 || !token.equals(Token.END)) {
            Map<String, String> rules = operatingPrecedenceParsingMatrix.get(stack.peek().getName());
            if (Token.ERROR.getName().equals(token.getName())) {
                System.out.println("Token error, invalid element");
                break;
            } else {
                switch (rules.get(token.getName())) {
                    case "<.":
                    case "=":
                        stack.push(token);
                        i++;
                        token = tokens.get(i);
                        switch (token.getName()) {
                            case "EQUAL":
                            case "NOT_EQUAL":
                            case "MORE":
                            case "MORE_EQUAL":
                            case "LESS":
                            case "LESS_EQUAL":
                                token = Token.buildRelation(token.getSpell());
                                break;
                        }
                        break;
                    case ".>":
                        Token temp;
                        do {
                            temp = stack.pop();
                            if (!temp.equals(Token.LBRACKET) && !temp.equals(Token.RBRACKET))
                                rpn.append(temp.getSpell()).append(" ");
                        } while (!operatingPrecedenceParsingMatrix.get(stack.peek().getName()).get(temp.getName()).equals("<."));
                        break;
                    default:
                        switch (rules.get(token.getName())) {
                            case "e1":
                                System.out.println("Missing operand");
                                tokens.add(i, Token.TRUE);

                                break;
                            case "e2":
                                System.out.println("Unbalanced right bracket");
                                tokens.remove(token);
                                break;
                            case "e3":
                                System.out.println("Missing operator");
                                tokens.add(Token.AND);
                                break;
                            case "e4":
                                System.out.println("Missing right bracket");
                                stack.pop();
                                break;
                            case "e5":
//                                System.out.println("This operation cannot be applied to this type of literals.");
//                                if (stack.peek().getName().equals("TRUE") || stack.peek().getName().equals("FALSE")) {
//                                    token = Token.AND;
//                                } else if (stack.peek().getName().equals("IDENT")) {
//                                    token = Token.buildRelation(Token.EQUAL.getSpell());
//                                } else if (stack.peek().getName().equals("RELATIONSHIP")) {
//                                    stack.pop();
//                                    stack.push(Token.AND);
//                                }
//                                do {
//                                    temp = stack.pop();
//                                    if (!temp.equals(Token.LBRACKET) && !temp.equals(Token.RBRACKET))
//                                        rpn.append(temp.getSpell()).append(" ");
//                                } while (!operatingPrecedenceParsingMatrix.get(stack.peek().getName()).get(temp.getName()).equals("<."));
                                break;
                        }
                        break;
                }
            }
        }

        return rpn.toString();
    }

    private List<Token> readTokensExpression() {
        List<Token> tokens = new ArrayList<>();
        while (true) {
            tokens.add(lexer.next());
            if (tokens.get(tokens.size() - 1).getName().equals("END"))
                break;
        }
        return tokens;
    }

    private void printMatrix() {
        System.out.println("Operator Precedence Matrix:\n");
        System.out.print(String.format("%12s|", " "));
        for (Map.Entry<String, Map<String, String>> entry : operatingPrecedenceParsingMatrix.entrySet()) {
            if (entry.getKey().equals("RELATIONSHIP"))
                System.out.print(String.format("%12s|", entry.getKey()));
            else if (entry.getKey().equals("LBRACKET") || entry.getKey().equals("RBRACKET"))
                System.out.print(String.format("%8s|", entry.getKey()));
            else
                System.out.print(String.format("%5s|", entry.getKey()));
        }
        System.out.print(String.format("\n%12s|", " ").replace(" ", "-"));
        for (Map.Entry<String, Map<String, String>> entry : operatingPrecedenceParsingMatrix.entrySet()) {
            if (entry.getKey().equals("RELATIONSHIP"))
                System.out.print(String.format("%12s|", " ").replace(" ", "-"));
            else if (entry.getKey().equals("LBRACKET") || entry.getKey().equals("RBRACKET"))
                System.out.print(String.format("%8s|", " ").replace(" ", "-"));
            else
                System.out.print(String.format("%5s|", " ").replace(" ", "-"));
        }
        System.out.println();
        for (Map.Entry<String, Map<String, String>> extEntry : operatingPrecedenceParsingMatrix.entrySet()) {
            System.out.print(String.format("%12s|", extEntry.getKey()));
            for (Map.Entry<String, String> intEntry : extEntry.getValue().entrySet()) {
                if (intEntry.getKey().equals("RELATIONSHIP"))
                    System.out.print(String.format("%12s|", intEntry.getValue()));
                else if (intEntry.getKey().equals("LBRACKET") || intEntry.getKey().equals("RBRACKET"))
                    System.out.print(String.format("%8s|", intEntry.getValue()));
                else
                    System.out.print(String.format("%5s|", intEntry.getValue()));
            }
            System.out.print(String.format("\n%12s|", " ").replace(" ", "-"));
            for (Map.Entry<String, Map<String, String>> entry : operatingPrecedenceParsingMatrix.entrySet()) {
                if (entry.getKey().equals("RELATIONSHIP"))
                    System.out.print(String.format("%12s|", " ").replace(" ", "-"));
                else if (entry.getKey().equals("LBRACKET") || entry.getKey().equals("RBRACKET"))
                    System.out.print(String.format("%8s|", " ").replace(" ", "-"));
                else
                    System.out.print(String.format("%5s|", " ").replace(" ", "-"));
            }
            System.out.println();
        }
    }

}
