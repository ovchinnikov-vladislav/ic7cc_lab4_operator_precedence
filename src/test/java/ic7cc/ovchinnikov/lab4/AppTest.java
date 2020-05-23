/*
 * This Java source file was generated by the Gradle 'init' task.
 */
package ic7cc.ovchinnikov.lab4;

import ic7cc.ovchinnikov.lab4.lexer.Lexer;
import ic7cc.ovchinnikov.lab4.model.Token;
import ic7cc.ovchinnikov.lab4.parser.OperatorPrecedenceParsing;
import org.junit.Assert;
import org.junit.Test;

import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Logger;

public class AppTest {

    private static final Logger log = Logger.getLogger(AppTest.class.getName());

    @Test
    public void testLexerSourceCode1() {
        Lexer lexer = new Lexer("test/source_code_1.txt");

        List<Token> tokenList = new LinkedList<>() {{
            add(Token.FALSE); add(Token.AND);
            add(Token.TRUE); add(Token.OR);
            add(Token.FALSE); add(Token.END);
        }};

        List<Token> resultTokens = new LinkedList<>();
        while (!lexer.isEnd()) {
            Token t = lexer.next();
            resultTokens.add(t.getName().equals(Token.IDENT.getName()) ? Token.IDENT : t);
        }
        Assert.assertEquals(tokenList, resultTokens);
    }

    @Test
    public void testLexerSourceCode2() throws IOException {
        Lexer lexer = new Lexer("test/source_code_2.txt");

        List<Token> tokenList = new LinkedList<>() {{
            add(Token.IDENT); add(Token.AND);
            add(Token.IDENT);add(Token.OR);
            add(Token.IDENT);add(Token.END);
        }};

        List<Token> resultTokens = new LinkedList<>();
        while (!lexer.isEnd()) {
            Token t = lexer.next();
            resultTokens.add(t.getName().equals(Token.IDENT.getName()) ? Token.IDENT : t);
        }
        Assert.assertEquals(tokenList, resultTokens);
    }

    @Test
    public void testLexerSourceCode3() throws IOException {
        Lexer lexer = new Lexer("test/source_code_3.txt");

        int i = 0;
        while (!lexer.isEnd()) {
            i++;
            lexer.next();
        }
        Assert.assertEquals(6, i);
    }

    @Test
    public void testLexerSourceCode4() throws IOException {
        Lexer lexer = new Lexer("test/source_code_4.txt");

        int i = 0;
        while (!lexer.isEnd()) {
            i++;
            lexer.next();
        }
        Assert.assertEquals(14, i);
        Assert.assertEquals(Token.END, lexer.next());
    }

    @Test
    public void testLexerSourceCode5() throws IOException {
        Lexer lexer = new Lexer("test/source_code_5.txt");

        int i = 0;
        while (!lexer.isEnd()) {
            i++;
            lexer.next();
        }
        Assert.assertEquals(5, i);
        Assert.assertEquals(Token.END, lexer.next());
    }

    @Test
    public void testParserSourceCode1() {
        OperatorPrecedenceParsing parsing = new OperatorPrecedenceParsing("test/source_code_1.txt");

        String str = parsing.build();
        Assert.assertEquals("false true & false !", str.trim());
    }

    @Test
    public void testParserSourceCode2() throws IOException {
        OperatorPrecedenceParsing parsing = new OperatorPrecedenceParsing("test/source_code_2.txt");

        String str = parsing.build();
        Assert.assertEquals("a b & c !", str.trim());
    }

    @Test
    public void testParserSourceCode3() throws IOException {
        OperatorPrecedenceParsing parsing = new OperatorPrecedenceParsing("test/source_code_3.txt");

        String str = parsing.build();
        Assert.assertEquals("false a ! b !", str.trim());
    }

    @Test
    public void testParserSourceCode4() throws IOException {
        OperatorPrecedenceParsing parsing = new OperatorPrecedenceParsing("test/source_code_4.txt");

        String str = parsing.build();
        Assert.assertEquals("false true false ! & c b ! &", str.trim());
    }

    @Test
    public void testParserSourceCode5() throws IOException {
        OperatorPrecedenceParsing parsing = new OperatorPrecedenceParsing("test/source_code_5.txt");

        String str = parsing.build();
        Assert.assertEquals("false true & false !", str.trim());
    }
}
