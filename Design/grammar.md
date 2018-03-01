Grammar
-------

    Prefix ::= <Symbol>.

    Direction ::= <1-9>

    DirectionSeq ::= <Direction>[(<Nat>)]
                   | <Direction>[(<Nat>)]<DirectionSeq>

    Button ::= <Symbol>

    ButtonSeq ::= <Button>[(<Nat>)]
                | <Button>+<ButtonSeq>

    Move ::= [<Prefix>][<DirectionSeq>]<ButtonSeq>

    ComboSeq ::= <DirectionSeq> [<ComboSeq>]
               | <Move> [<LinkOrCancel> <ComboSeq>]

    LinkOrCancel ::= ,
                   | >
