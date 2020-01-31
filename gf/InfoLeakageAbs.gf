abstract InfoLeakageAbs = {
  flags startcat = LeaksAssertion ;

  cat
    LeaksAssertion; InstSpec; InstClass; InstClassOfProc;
    NamedInst; NamedInstOfProc;
    ProcessorSpec; NamedProcessor; Bool;
    -- ComponentSpec; OperationSpec; LeakSpec;

  fun

    ----------------------------------------------------------------------
    -- Assertions About Leakage
    ----------------------------------------------------------------------

    -- A timing leaks phrase asserts that an instruction or set of instructions
    -- does or does not leak timing information. Timing leaks phrases contain:
    --
    -- * a Boolean (does or doesn't); and
    -- * a specification of what instructions do or do not leak timing
    --   information
    --
    TimingLeaksAssertion : Bool -> InstSpec -> LeaksAssertion;

    -- A digital leaks phrase contains:
    -- * a Boolean (does or doesn't);
    -- * a specification of what components do or do not leak; and
    -- * a specification of what digital information is or is not leaked
    -- DigitalLeaksAssertion : Bool -> ComponentSpec -> LeakSpec -> LeaksAssertion;


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

    -- Named instructions
    IntegerMultiplyOp_NamedInst : NamedInst;
    Fmsub_s_NamedInst : NamedInst;


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


    ----------------------------------------------------------------------
    -- Miscellaneous
    ----------------------------------------------------------------------

    -- Booleans: either true or false
    TrueBool : Bool;
    FalseBool : Bool;
}
