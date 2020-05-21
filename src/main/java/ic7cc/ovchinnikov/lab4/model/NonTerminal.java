package ic7cc.ovchinnikov.lab4.model;

import com.fasterxml.jackson.annotation.*;
import lombok.Getter;

import java.util.Objects;

@Getter
public class NonTerminal implements Comparable<NonTerminal> {

    private final String name;

    @JsonCreator
    public NonTerminal(@JsonProperty("name") String name) {
        this.name = name;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        NonTerminal nonTerminal = (NonTerminal) o;
        return Objects.equals(name, nonTerminal.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name);
    }

    @Override
    public String toString() {
        return "NonTerminal{" +
                "name='" + name + '\'' +
                '}';
    }

    public Symbol toSymbol() {
        return new Symbol(name, null, Symbol.Type.NON_TERM);
    }

    @Override
    public int compareTo(NonTerminal o) {
        return this.name.compareTo(o.name);
    }
}
