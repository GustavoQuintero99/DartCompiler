/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package lexico;

/**
 *
 * @author gustavoquintero
 */
public enum DartReservedWords {
    ASSERT(0),
    BREAK(1),
    CASE(2),
    CATCH(3),
    CLASS(4),
    CONST(5),
    CONTINUE(6),
    DEFAULT(7),
    DO(8),
    ELSE(9),
    ENUM(10),
    EXTENDS(11),
    FALSE(12),
    FINAL(13),
    FINALLY(14),
    FOR(15),
    IF(16),
    IN(17),
    IS(18),
    NEW(19),
    NULL(20),
    RETHROW(21),
    RETURN(22),
    SUPER(23),
    SWITCH(24),
    THIS(25),
    THROW(26),
    TRUE(27),
    TRY(28),
    VAR(29),
    VOID(30),
    WHILE(31),
    WITH(32),
    AWAIT(33),
    YIELD(34),
    ASYNC(35),
    HIDE(36),
    ON(37),
    SHOW(38),
    SYNC(39),
    MAIN(40),
    PRINT(41);
    
    public int value;
    private DartReservedWords(int value){
        this.value = value;
    }
    
    public int getEnum(){
        return value;
    }
    //ASSERT, BREAK, CASE, CATCH, CLASS, CONST, CONTINUE, DEFAULT, DO, ELSE, ENUM, EXTENDS, FALSE, FINAL, FINALLY, FOR, IF, IN, IS, NEW, NULL, RETHROW, RETURN, SUPER, SWITCH, THIS, THROW, TRUE, TRY, VAR, VOID, WHILE, WITH, AWAIT, YIELD, ASYNC, HIDE, ON, SHOW, SYNC
}
