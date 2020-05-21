/*
 * This Java source file was generated by the Gradle 'init' task.
 */
package ic7cc.ovchinnikov.lab4;
import ic7cc.ovchinnikov.lab4.model.*;

import java.io.IOException;

public class App {

    public static void main(String[] args) throws IOException {
        Parser parser = new Parser("test/source_code.txt");
        parser.parse();

        parser.printParseTreePNG("result/parse_tree.png");
    }

    public static Grammar grammarG4WithC() {
        Grammar grammar = new Grammar("G4 C", "<program>");
        Terminal lbrace = new Terminal("LBRACE", "{");
        Terminal rbrace = new Terminal("RBRACE", "}");
        Terminal semicolon = new Terminal("SEMICOLON", ";");
        Terminal id = new Terminal("IDENT", "$id");
        Terminal assign = new Terminal("ASSIGN", "=");
        Terminal or = new Terminal("OR", "!");
        Terminal and = new Terminal("AND", "&");
        Terminal not = new Terminal("NOT", "~");
        Terminal trueTerm = new Terminal("true", "true");
        Terminal falseTerm = new Terminal("false", "false");
        grammar.addTerminals(lbrace, rbrace, semicolon, id, assign, or, and, not, trueTerm, falseTerm);

        NonTerminal block = new NonTerminal("<block>");
        NonTerminal operatorsList = new NonTerminal("<operators_list>");
        NonTerminal operator = new NonTerminal("<operator>");
        NonTerminal tail = new NonTerminal("<tail>");
        NonTerminal expression = new NonTerminal("<expression>");
        NonTerminal boolExpression = new NonTerminal("<bool_expression>");
        NonTerminal boolMonomial = new NonTerminal("<bool_monomial>");
        NonTerminal secondaryBoolExpression = new NonTerminal("<secondary_bool_expression>");
        NonTerminal primaryBoolExpression = new NonTerminal("<primary_bool_expression>");
        NonTerminal boolValue = new NonTerminal("<bool_value>");
        NonTerminal boolOperationSign = new NonTerminal("<bool_operation_sign>");
        grammar.addNonTerminals(block, operatorsList, operator, tail, expression, boolExpression, boolMonomial,
                secondaryBoolExpression, primaryBoolExpression, boolValue, boolOperationSign);

        grammar.addProduction(grammar.getStartSymbol(), block.toSymbol());
        grammar.addProduction(block, lbrace.toSymbol(), operatorsList.toSymbol(), rbrace.toSymbol());
        grammar.addProduction(operatorsList, operator.toSymbol(), tail.toSymbol());
        grammar.addProduction(tail, semicolon.toSymbol(), operator.toSymbol(), tail.toSymbol());
        grammar.addProduction(tail, Symbol.EPSILON);
        grammar.addProduction(operator, id.toSymbol(), assign.toSymbol(), expression.toSymbol());
        grammar.addProduction(operator, block.toSymbol());
        grammar.addProduction(expression, boolExpression.toSymbol());
        grammar.addProduction(boolExpression, boolMonomial.toSymbol());
        grammar.addProduction(boolExpression, boolExpression.toSymbol(), or.toSymbol(), boolMonomial.toSymbol());
        grammar.addProduction(boolMonomial, secondaryBoolExpression.toSymbol());
        grammar.addProduction(boolMonomial, boolMonomial.toSymbol(), and.toSymbol(), secondaryBoolExpression.toSymbol());
        grammar.addProduction(secondaryBoolExpression, primaryBoolExpression.toSymbol());
        grammar.addProduction(secondaryBoolExpression, not.toSymbol(), primaryBoolExpression.toSymbol());
        grammar.addProduction(primaryBoolExpression, boolValue.toSymbol());
        grammar.addProduction(primaryBoolExpression, id.toSymbol());
        grammar.addProduction(boolValue, trueTerm.toSymbol());
        grammar.addProduction(boolValue, falseTerm.toSymbol());
        grammar.addProduction(boolOperationSign, not.toSymbol());
        grammar.addProduction(boolOperationSign, and.toSymbol());
        grammar.addProduction(boolOperationSign, or.toSymbol());

        return grammar;
    }
}
