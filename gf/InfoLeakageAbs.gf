abstract InfoLeakageAbs = {
  flags startcat = LeaksAssertion ;

  cat
    LeaksAssertion; InstSpec; InstClass; Bool;
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
    -- Instruction Specifications
    ----------------------------------------------------------------------

    -- Different ways of saying all instructions in some class
    AllISpec : InstClass -> InstSpec;
    EveryISpec : InstClass -> InstSpec;
    TheISpec : InstClass -> InstSpec;
    NoQuantISpec : InstClass -> InstSpec;

    -- Only instructions in some class; i.e., instructions in this class and
    -- also not instructions not in this class
    OnlyISpec : InstClass -> InstSpec;

    -- No instructions in some class
    NoISpec : InstClass -> InstSpec;

    -- The class of instructions in general
    AnyInst : InstClass;

    -- Specific classes of instructions / operations
    ArithInst : InstClass;
    IntegerOp : InstClass;
    FloatingPointOp : InstClass;
    BitwiseOp : InstClass;
    ProcessorFence : InstClass;

    -- Named instruction
    -- NamedInstruction : String -> InstSpec;


    ----------------------------------------------------------------------
    -- Miscellaneous
    ----------------------------------------------------------------------

    -- Booleans: either true or false
    TrueBool : Bool;
    FalseBool : Bool;
}
