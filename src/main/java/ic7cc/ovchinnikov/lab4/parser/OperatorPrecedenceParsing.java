package ic7cc.ovchinnikov.lab4.parser;

import ic7cc.ovchinnikov.lab4.model.Token;

import java.util.*;

public class OperatorPrecedenceParsing {

    private static final String MARKER = "$";

    private static final Map<Token, Character> token = new HashMap<>();

    static {
        token.put(Token.IDENT)
    }

}
