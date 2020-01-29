abstract InfoLeakageAbs = {
  flags startcat = LeaksAssertion ;

  cat
    LeaksAssertion ; InstructionSpec ; Bool ;
    -- ComponentSpec ; OperationSpec ; LeakSpec ;

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
    TimingLeaksAssertion : Bool -> InstructionSpec -> LeaksAssertion;

    -- A digital leaks phrase contains:
    -- * a Boolean (does or doesn't);
    -- * a specification of what components do or do not leak; and
    -- * a specification of what digital information is or is not leaked
    -- DigitalLeaksAssertion : Bool -> ComponentSpec -> LeakSpec -> LeaksAssertion;


    ----------------------------------------------------------------------
    -- Instruction Specifications
    ----------------------------------------------------------------------

    -- All instructions
    AllInstructions : InstructionSpec;

    -- All arithmetic instructions
    ArithInstructions : InstructionSpec;

    -- Named instruction
    -- NamedInstruction : String -> InstructionSpec;


    ----------------------------------------------------------------------
    -- Miscellaneous
    ----------------------------------------------------------------------

    -- Booleans: either true or false
    TrueBool : Bool;
    FalseBool : Bool;
}
