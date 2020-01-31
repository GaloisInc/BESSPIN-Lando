abstract InfoLeakageAbs = {
  flags startcat = LeaksAssertion ;

  cat
    LeaksAssertion; InstSpec; InstClass; InstClassOfProc;
    NamedInst; NamedInstOfProc;
    ProcessorSpec; NamedProcessor; InfoSpec; ChannelSpec;
    Bool;

  fun

    ----------------------------------------------------------------------
    -- Assertions About Leakage
    ----------------------------------------------------------------------

    -- A leakage assertion contains a number of parts, some of which are
    -- optional. To indicate which parts occur in what order in each constructor
    -- below, we assign letters to each of these parts. The parts are:
    --
    -- * The instruction or operation that causes the leak (I);
    -- * The processor containing that operation (P);
    -- * The information that is leaked (L);
    -- * The channel by which the information is leaked (C).
    --
    -- Leakage assertions also contain a Boolean modifier to indicate if the
    -- statement is negative, e.g., "does not leak" instead of "leaks".
    LeakAssertionIL : Bool -> InstSpec -> InfoSpec -> LeaksAssertion;
    LeakAssertionILP :
      Bool -> InstSpec -> InfoSpec -> ProcessorSpec -> LeaksAssertion;
    LeakAssertionILC :
      Bool -> InstSpec -> InfoSpec -> ChannelSpec -> LeaksAssertion;
    LeakAssertionILCP :
      Bool -> InstSpec -> InfoSpec -> ChannelSpec -> ProcessorSpec ->
      LeaksAssertion;
    LeakAssertionPILC :
      Bool -> ProcessorSpec -> InstSpec -> InfoSpec -> ChannelSpec ->
      LeaksAssertion;

    -- FIXME: use dependent types to be sure that an InstSpec does not give a
    -- processor if the processor occurs later in the sentence


    ----------------------------------------------------------------------
    -- Specifications of Sets of Instructions / Operations
    ----------------------------------------------------------------------

    -- Different ways of saying all instructions in some class of some processor
    AllISpec : InstClassOfProc -> InstSpec;
    EveryISpec : InstClassOfProc -> InstSpec;
    TheISpec : InstClassOfProc -> InstSpec;

    -- Only instructions in some class; i.e., instructions in this class and
    -- also not instructions not in this class
    OnlyISpec : InstClassOfProc -> InstSpec;

    -- No instructions in some class
    NoISpec : InstClassOfProc -> InstSpec;
    NoPluralISpec : InstClassOfProc -> InstSpec;

    -- A specific named instruction
    TheNamedISpec : NamedInstOfProc -> InstSpec;

    -- A named instruction and no others
    OnlyNamedISpec : NamedInstOfProc -> InstSpec;

    -- An InstClassOfProc is an instruction class of an optional processor
    InstClassHasProc : InstClass -> ProcessorSpec -> InstClassOfProc;
    InstClassNoProc : InstClass -> InstClassOfProc;

    -- The class of instructions in general
    AnyInst : InstClass;

    -- Specific classes of instructions / operations
    ArithInst : InstClass;
    IntegerOp : InstClass;
    FloatingPointOp : InstClass;
    BitwiseOp : InstClass;
    ProcessorFence : InstClass;

    -- A NamedInstOfProc is a named instruction of an optional processor
    NamedInstHasProc : NamedInst -> ProcessorSpec -> NamedInstOfProc;
    NamedInstNoProc : NamedInst -> NamedInstOfProc;

    -- Named instructions; the "Rev_" prefix indicates that the order of name
    -- and "instruction" are reversed, as in "instruction I" instead of "I
    -- instruction"; this only makes sense for certain named instructions
    IntegerMultiplyOp_NamedInst : NamedInst;
    Fmsub_s_NamedInst : NamedInst;
    Rev_Fmsub_s_NamedInst : NamedInst;


    ----------------------------------------------------------------------
    -- Specifications of Processors
    ----------------------------------------------------------------------

    -- The processor is left implicit, i.e., not mentioned
    -- ImplicitPSpec : ProcessorSpec;

    -- Ways of referring to a specific named processor
    TheNamedPSpec : NamedProcessor -> ProcessorSpec;
    MyNamedPSpec : NamedProcessor -> ProcessorSpec;

    -- Specificy named processors
    Rocket_NamedProcessor : NamedProcessor;
    P3_NamedProcessor : NamedProcessor;


    ----------------------------------------------------------------------
    -- Specification of Sorts of Information Leakage
    ----------------------------------------------------------------------

    -- A leak specification states what sort of information is being leaked and
    -- optionall what sort of channel leaks that information
    -- LeakSpecWithChannel : InfoSpec -> ChannelSpec -> LeakSpec;
    -- LeakSpecNoChannel : InfoSpec -> LeakSpec;

    -- The sorts of information that can be leaked
    TimingInfo : InfoSpec;
    InputOperandValues : InfoSpec;

    -- The negations of information specifications
    -- FIXME: make sure to disallow double-negation!
    NoTimingInfo : InfoSpec;
    NoInputOperandValues : InfoSpec;

    -- The sorts of channels through which information can be leaked
    TimingSideChannel : ChannelSpec;
    InformationFlowChannel : ChannelSpec;


    ----------------------------------------------------------------------
    -- Miscellaneous
    ----------------------------------------------------------------------

    -- Booleans: either true or false
    TrueBool : Bool;
    FalseBool : Bool;
}
