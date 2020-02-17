abstract InfoLeakageAbs = {
  flags startcat = LeaksAssertion ;

  cat
    LeaksAssertion; InstSpec; InstClass; NamedInst;
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
    LeakAssertionIPL :
      Bool -> InstSpec -> ProcessorSpec -> InfoSpec -> LeaksAssertion;
    LeakAssertionILP :
      Bool -> InstSpec -> InfoSpec -> ProcessorSpec -> LeaksAssertion;
    LeakAssertionILC :
      Bool -> InstSpec -> InfoSpec -> ChannelSpec -> LeaksAssertion;
    LeakAssertionIPLC :
      Bool -> InstSpec -> ProcessorSpec -> InfoSpec -> ChannelSpec ->
      LeaksAssertion;
    LeakAssertionILCP :
      Bool -> InstSpec -> InfoSpec -> ChannelSpec -> ProcessorSpec ->
      LeaksAssertion;
    LeakAssertionPILC :
      Bool -> ProcessorSpec -> InstSpec -> InfoSpec -> ChannelSpec ->
      LeaksAssertion;


    ----------------------------------------------------------------------
    -- Specifications of Sets of Instructions / Operations
    ----------------------------------------------------------------------

    -- Different ways of saying all instructions in some class of some processor
    AllISpec : InstClass -> InstSpec;
    EveryISpec : InstClass -> InstSpec;
    TheISpec : InstClass -> InstSpec;

    -- Only instructions in some class; i.e., instructions in this class and
    -- also not instructions not in this class
    OnlyISpec : InstClass -> InstSpec;

    -- Only instructions in some class out of some larger class; i.e.,
    -- instructions in the first class and also not in the second class
    OnlyOutOfISpec : InstClass -> InstClass -> InstSpec;
    OnlyNamedOutOfISpec : NamedInst -> InstClass -> InstSpec;

    -- No instructions in some class
    NoISpec : InstClass -> InstSpec;
    NoPluralISpec : InstClass -> InstSpec;

    -- A specific named instruction
    TheNamedISpec : NamedInst -> InstSpec;

    -- A named instruction and no others
    OnlyNamedISpec : NamedInst -> InstSpec;

    -- The class of instructions in general
    AnyInst : InstClass;

    -- Specific classes of instructions / operations
    ArithInst : InstClass;
    IntegerOp : InstClass;
    FloatingPointOp : InstClass;
    BitwiseOp : InstClass;
    ProcessorFence : InstClass;

    A_NamedInst : String -> NamedInst;
    A_NamedOp : String -> NamedInst;


    ----------------------------------------------------------------------
    -- Specifications of Processors
    ----------------------------------------------------------------------

    -- The processor is left implicit, i.e., not mentioned
    -- ImplicitPSpec : ProcessorSpec;

    -- Ways of referring to a specific named processor
    TheNamedPSpec : NamedProcessor -> ProcessorSpec;
    MyNamedPSpec : NamedProcessor -> ProcessorSpec;
    OurNamedPSpec : NamedProcessor -> ProcessorSpec;

    -- Specific named processors
    A_NamedProcessor : String -> NamedProcessor;


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
