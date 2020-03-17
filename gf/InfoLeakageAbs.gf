abstract InfoLeakageAbs = {
  flags startcat = LeaksAssertion ;

  cat
    LeaksAssertion; InstSpec; InstClass; NamedInst;
    ProcessorSpec; NamedProcessor; InfoSpec; ChannelSpec;
    DataSpec; NamedDatum; ModuleSpec; NamedModule; BoundarySpec;
    Bool;

  fun

    ----------------------------------------------------------------------
    -- Assertions About Leakage
    ----------------------------------------------------------------------

    -- An instruction leakage assertion states that an instruction or operation
    -- on a processor does or does not cause information to be leaked. These
    -- assertions contain a number of parts, some of which are optional. To
    -- indicate which parts occur in what order in each constructor below, we
    -- assign letters to each of these parts. The parts are:
    --
    -- * The instruction or operation that causes the leak (I);
    -- * The processor containing that operation (P);
    -- * The information that is leaked (L);
    -- * The channel by which the information is leaked (C).
    --
    -- Leakage assertions also contain a Boolean modifier to indicate if the
    -- statement is negative, e.g., "does not leak" instead of "leaks".
    --
    -- FIXME: I don't know if channels (C) make sense here...
    ILeakAssertionIL : Bool -> InstSpec -> InfoSpec -> LeaksAssertion;
    ILeakAssertionIPL :
      Bool -> InstSpec -> ProcessorSpec -> InfoSpec -> LeaksAssertion;
    ILeakAssertionILP :
      Bool -> InstSpec -> InfoSpec -> ProcessorSpec -> LeaksAssertion;
    ILeakAssertionILC :
      Bool -> InstSpec -> InfoSpec -> ChannelSpec -> LeaksAssertion;
    ILeakAssertionIPLC :
      Bool -> InstSpec -> ProcessorSpec -> InfoSpec -> ChannelSpec ->
      LeaksAssertion;
    ILeakAssertionILCP :
      Bool -> InstSpec -> InfoSpec -> ChannelSpec -> ProcessorSpec ->
      LeaksAssertion;
    ILeakAssertionPILC :
      Bool -> ProcessorSpec -> InstSpec -> InfoSpec -> ChannelSpec ->
      LeaksAssertion;


    -- A data leakage assertion states that some signal or register in a
    -- functional unit of a processor is or is not leaked. These assertions are
    -- in passive voice, e.g., "The X signal of the Y module is leaked through
    -- Z". The parts of data leakage assertions are:
    --
    -- * The datum, e.g., a named signal or register (D);
    -- * The module or block containing that datum (M);
    -- * The processor containing the functional unit (P);
    -- * The channel by which the information is leaked (C); and
    -- * The functional unit boundary past which the information is leaked (B).
    --
    -- Note that an assertion cannot have both C andB, because a boundary
    -- implies the set of all channels out of a given functional unit.
    DLeakAssertionDMC :
      Bool -> DataSpec -> ModuleSpec -> ChannelSpec -> LeaksAssertion;
    DLeakAssertionDMPC :
      Bool -> DataSpec -> ModuleSpec -> ProcessorSpec -> ChannelSpec ->
      LeaksAssertion;
    DLeakAssertionDMB :
      Bool -> DataSpec -> ModuleSpec -> BoundarySpec -> LeaksAssertion;
    DLeakAssertionDMPB :
      Bool -> DataSpec -> ModuleSpec -> ProcessorSpec -> BoundarySpec ->
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
    NamedOutputChannel : String -> ChannelSpec;


    ----------------------------------------------------------------------
    -- Specification of Data, Modules, and Boundaries
    ----------------------------------------------------------------------

    TheNamedDatum : NamedDatum -> DataSpec;
    NamedRegister : String -> NamedDatum;
    NamedSignal : String -> NamedDatum;

    TheNamedModule : NamedModule -> ModuleSpec;
    MyNamedModule : NamedModule -> ModuleSpec;
    ANamedModule : String -> NamedModule;
    NamedVerilogModule : String -> NamedModule;
    NamedBlock : String -> NamedModule;

    TheModuleCertBoundary : ModuleSpec -> BoundarySpec;
    MyModuleCertBoundary : ModuleSpec -> BoundarySpec;


    ----------------------------------------------------------------------
    -- Miscellaneous
    ----------------------------------------------------------------------

    -- Booleans: either true or false
    TrueBool : Bool;
    FalseBool : Bool;
}
