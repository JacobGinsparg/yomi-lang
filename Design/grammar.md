Grammar
-------

Note that the grammar includes the use of angle brackets, which are normally
used in BNFs to refer to other rules and types. To avoid confusion, we are
using curly braces to refer to rules/types.

    Prefix ::= {Symbol}.

    Direction ::= {1-9}

    DirectionSeq ::= {Direction}[<{Nat}>]
                   | {Direction}[<{Nat}>]{DirectionSeq}

    Button ::= {Symbol}

    ButtonSeq ::= {Button}[<{Nat}>]
                | {Button}+{ButtonSeq}

    Move ::= [{Prefix}][{DirectionSeq}]{ButtonSeq}

    ComboSeq ::= {DirectionSeq} [{ComboSeq}]
               | {Move} [{LinkOrCancel} {ComboSeq}]

    LinkOrCancel ::= ,
                   | ~
