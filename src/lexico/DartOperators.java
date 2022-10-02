package lexico;

/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Enum.java to edit this template
 */

/**
 *
 * @author gustavoquintero
 */
public enum DartOperators {
    EQ(61),
    EQEQ(62),
    PLUS(63),
    PLUSPLUS(64),
    MINUS(65),
    MINUSMINUS(66),
    POW(67),
    POWPOW(68),
    AND(69),
    OR(70),
    MINUSEQ(71),
    PLUSEQ(72),
    RESID(73),
    STRINGLITERAL(74),
    SPACE(75),
    MAIN(76),
    OPENCODE(77),
    CLOSECODE(78),
    OPENPARENT(79),
    CLOSEPARENT(80),
    LINEABREAK(81),
    SEMICOLON(82);
    
    public int value;
    private DartOperators(int value){
        this.value = value;
    }
    
    public int getEnum(){
        return value;
    }
}
