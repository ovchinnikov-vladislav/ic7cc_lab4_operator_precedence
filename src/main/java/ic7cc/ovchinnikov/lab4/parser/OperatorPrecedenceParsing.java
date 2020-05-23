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
                    put(Token.NOT.getName(), "e5");
                    put(Token.AND.getName(), ".>");
                    put(Token.OR.getName(), ".>");
                    put(Token.LBRACKET.getName(), "e3");
                    put(Token.RBRACKET.getName(), ".>");
                    put("RELATIONSHIP", ".>");
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
                    put("RELATIONSHIP", "e5");
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
                    put("RELATIONSHIP", "e5");
                    put(Token.END.getName(), ".>");
                }});
        operatingPrecedenceParsingMatrix.put(Token.NOT.getName(),
                new LinkedHashMap<>() {{
                    put(Token.IDENT.getName(), "e5");
                    put(Token.TRUE.getName(), "<.");
                    put(Token.FALSE.getName(), "<.");
                    put(Token.NOT.getName(), "<.");
                    put(Token.AND.getName(), ".>");
                    put(Token.OR.getName(), ".>");
                    put(Token.LBRACKET.getName(), "<.");
                    put(Token.RBRACKET.getName(), ".>");
                    put("RELATIONSHIP", "<.");
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
                    put("RELATIONSHIP", "<.");
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
                    put("RELATIONSHIP", "<.");
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
                    put("RELATIONSHIP", "<.");
                    put(Token.END.getName(), "e4");
                }});
        operatingPrecedenceParsingMatrix.put(Token.RBRACKET.getName(),
                new LinkedHashMap<>() {{
                    put(Token.IDENT.getName(), "e3");
                    put(Token.TRUE.getName(), "e3");
                    put(Token.FALSE.getName(), "e3");
                    put(Token.NOT.getName(), ".>");
                    put(Token.AND.getName(), ".>");
                    put(Token.OR.getName(), ".>");
                    put(Token.LBRACKET.getName(), "e3");
                    put(Token.RBRACKET.getName(), ".>");
                    put("RELATIONSHIP", ".>");
                    put(Token.END.getName(), ".>");
                }});
        operatingPrecedenceParsingMatrix.put("RELATIONSHIP",
                new LinkedHashMap<>() {{
                    put(Token.IDENT.getName(), "<.");
                    put(Token.TRUE.getName(), "e5");
                    put(Token.FALSE.getName(), "e5");
                    put(Token.NOT.getName(), ".>");
                    put(Token.AND.getName(), ".>");
                    put(Token.OR.getName(), ".>");
                    put(Token.LBRACKET.getName(), "<.");
                    put(Token.RBRACKET.getName(), ".>");
                    put("RELATIONSHIP", "e6");
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
                    put("RELATIONSHIP", "<.");
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
                        if (token.getName().equals("RELATIONSHIP")) {
                            if (i + 1 < tokens.size()) {
                                if (tokens.get(i + 1).equals(Token.LBRACKET)) {
                                    System.out.println("Missing operand");
                                    tokens.add(i + 1, Token.buildAtom("a'"));
                                    tokens.add(i + 2, Token.AND);
                                    token = stack.pop();
                                    i--;
                                } else if (tokens.get(i + 1).equals(Token.NOT)) {
                                    System.out.println("This operation cannot be applied to this type of literals.");
                                    tokens.remove(i + 1);
                                } else {
                                    Token t;
                                    do {
                                        t = null;
                                        switch (tokens.get(i + 1).getName()) {
                                            case "EQUAL":
                                            case "NOT_EQUAL":
                                            case "MORE":
                                            case "MORE_EQUAL":
                                            case "LESS":
                                            case "LESS_EQUAL":
                                                t = Token.buildRelation(token.getSpell());
                                                break;
                                        }
                                        if (t != null) {
                                            System.out.println("Cascading relationship operations");
                                            tokens.remove(i + 1);
                                        }
                                    } while (t != null);
                                }
                            } else {
                                System.out.println("Missing operand");
                                tokens.add(i + 1, Token.buildAtom("a'"));
                                token = stack.pop();
                                i--;
                            }
                        }
                        if (token.equals(Token.AND) || token.equals(Token.OR)) {
                            Token t;
                            boolean rightBool = false;
                            if (i + 2 < tokens.size()) {
                                switch (tokens.get(i + 2).getName()) {
                                    case "EQUAL":
                                    case "NOT_EQUAL":
                                    case "MORE":
                                    case "MORE_EQUAL":
                                    case "LESS":
                                    case "LESS_EQUAL":
                                        t = Token.buildRelation(tokens.get(i + 2).getSpell());
                                        rightBool = t.getName().equals("RELATIONSHIP");
                                        break;
                                }
                            } else {
                                System.out.println("Missing operand");
                                tokens.add(i + 1, Token.TRUE);
                                token = stack.pop();
                                i--;
                            }
                            if (!rightBool && i + 1 < tokens.size() && tokens.get(i + 1).getName().equals("IDENT")) {
                                System.out.println("This operation cannot be applied to this type of literals.");
                                tokens.set(i + 1, Token.TRUE);
                            }
                            boolean leftBool = false;
                            if (i - 2 >= 0) {
                                switch (tokens.get(i - 2).getName()) {
                                    case "EQUAL":
                                    case "NOT_EQUAL":
                                    case "MORE":
                                    case "MORE_EQUAL":
                                    case "LESS":
                                    case "LESS_EQUAL":
                                        t = Token.buildRelation(tokens.get(i - 2).getSpell());
                                        leftBool = t.getName().equals("RELATIONSHIP");
                                        break;
                                }
                            }
                            if (!leftBool && i - 1 >= 0 && tokens.get(i - 1).getName().equals("IDENT")) {
                                System.out.println("This operation cannot be applied to this type of literals.");
                                tokens.set(i - 1, Token.TRUE);
                                stack.pop();
                                stack.push(tokens.get(i - 1));
                            }
                            if ((i - 1 >= 0 && token.equals(tokens.get(i - 1))) || (i + 1 < tokens.size() && token.equals(tokens.get(i + 1)))) {
                                System.out.println("Missing operand");
                                tokens.add(i+1, Token.TRUE);
                                i--;
                                token = tokens.get(i);
                                stack.pop();
                            }
                        }
                        if ((token.equals(Token.TRUE) || token.equals(Token.FALSE)) && stack.peek().getName().equals("RELATIONSHIP")) {
                            System.out.println("This operation cannot be applied to this type of literals.");
                            tokens.set(i, Token.buildAtom("a'"));
                            token = tokens.get(i);
                        }
                        if (token.getName().equals(Token.IDENT.getName())) {
                            boolean rightBool = false;
                            boolean leftBool = false;
                            if (i + 1 < tokens.size() && !tokens.get(i + 1).getName().equals("RELATIONSHIP")) {
                                rightBool = true;
                            }
                            if (i - 1 < tokens.size() && !tokens.get(i - 1).getName().equals("RELATIONSHIP")) {
                                leftBool = true;
                            }
                            if (!rightBool && !leftBool) {
                                System.out.println("This operation cannot be applied to this type of literals.");
                                tokens.set(i, Token.TRUE);
                                token = tokens.get(i);
                            }
                        }
                        if (token.equals(Token.RBRACKET)) {
                            if (i + 1 < tokens.size()) {
                                switch (tokens.get(i + 1).getName()) {
                                    case "EQUAL":
                                    case "NOT_EQUAL":
                                    case "MORE":
                                    case "MORE_EQUAL":
                                    case "LESS":
                                    case "LESS_EQUAL":
                                        Token t = Token.buildRelation(tokens.get(i + 1).getSpell());
                                        if (t.getName().equals("RELATIONSHIP")) {
                                            System.out.println("This operation cannot be applied to this type of literals.");
                                            tokens.add(i + 1, Token.AND);
                                            tokens.add(i + 2, Token.buildAtom("a'"));
                                            token = stack.pop();
                                            i--;
                                        }
                                        break;
                                }
                            }
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
                                token = tokens.get(i);
                                break;
                            case "e3":
                                System.out.println("Missing operator");
                                tokens.add(i, Token.AND);
                                token = tokens.get(i);
                                break;
                            case "e4":
                                System.out.println("Missing right bracket");
                                stack.pop();
                                break;
                            case "e5":

                                System.out.println("This operation cannot be applied to this type of literals.");

                                if (token.getName().equals("IDENT")) {
                                    tokens.set(i, Token.TRUE);
                                    token = tokens.get(i);
                                } else if (token.equals(Token.AND) || token.equals(Token.OR)) {
                                    tokens.set(i, Token.EQUAL);
                                    token = tokens.get(i);
                                    if (i + 1 < tokens.size()) {
                                        tokens.set(i + 1, Token.buildAtom("a'"));
                                    }
                                }

                                if (stack.peek().getName().equals("TRUE") || stack.peek().getName().equals("FALSE")) {
                                    token = Token.AND;
                                    tokens.set(i, token);
                                    if (i + 1 < tokens.size() && tokens.get(i + 1).getName().equals("IDENT")) {
                                        tokens.set(i + 1, Token.TRUE);
                                    }
                                } else if (stack.peek().getName().equals("IDENT")) {
                                    token = Token.buildRelation(Token.EQUAL.getSpell());
                                } else if (stack.peek().getName().equals("RELATIONSHIP")) {
                                    stack.pop();
                                    stack.push(Token.AND);
                                }
                                break;
                            case "e6":
                                System.out.println("Cascading relationship operations");
                                tokens.set(i, Token.AND);
                                token = tokens.get(i);
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
