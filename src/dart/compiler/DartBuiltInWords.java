package dart.compiler;

/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Enum.java to edit this template
 */

/**
    *
    * @author gustavoquintero
    */
public enum DartBuiltInWords {
   ABSTRACT(40),
   AS(41),
   COVARIANT(42),
   DEFERRED(43),
   DYNAMIC(44),
   EXPORT(45),
   EXTENSION(46),
   EXTERNAL(47),
   FACTORY(48),
   FUNCTION(49),
   GET(50),
   IMPLEMENTS(51),
   IMPORT(52),
   INTERFACE(53),
   LIBRARY(54),
   MIXIN(55),
   OPERATOR(56),
   PART(57),
   SET(58),
   STATIC(59),
   TYPEDEF(60);
    public int value;
    private DartBuiltInWords(int value){
        this.value = value;
    }
    
    public int getEnum(){
        return value;
    }
    //ABSTRACT, AS, COVARIANT, DEFERRED, DYNAMIC, EXPORT, EXTENSION, EXTERNAL, FACTORY, FUNCTION, GET, IMPLEMENTS, IMPORT, INTERFACE, LIBRARY, MIXIN, OPERATOR, PART, SET, STATIC, TYPEDEF
}
