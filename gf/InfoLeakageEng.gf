--# -path=.:present

concrete InfoLeakageEng of InfoLeakageAbs = open SyntaxEng,ParadigmsEng in {
  lincat
    LeaksAssertion = S;
    InstSpec = NP;
    InstClass = N;
    NamedInst = CN;
    NamedInstOfProc = CN;
    InstClassOfProc = CN;
    ProcessorSpec = NP;
    NamedProcessor = N;
    InfoSpec = NP;
    ChannelSpec = NP;
    Bool = Pol;

  oper
    timing_information : NP =
      mkNP (mkCN (mkN "timing") (mkNP (mkN "information")));

    -- Some generic nouns
    instruction : N = mkN "instruction";
    operation : N = mkN "operation";
    processor : N = mkN "processor";


  lin

    ----------------------------------------------------------------------
    -- Assertions About Leakage
    ----------------------------------------------------------------------

    LeakAssertionIL pol ispec info =
      mkS presentTense simultaneousAnt pol
      (mkCl ispec (mkV2 (mkV "leak")) info);
    LeakAssertionILC pol ispec info chan =
      mkS presentTense simultaneousAnt pol
      (mkCl ispec (mkV3 (mkV "leak") noPrep through_Prep) info chan);
    LeakAssertionILP pol ispec info proc =
      mkS presentTense simultaneousAnt pol
      (mkCl ispec
       (mkVP
        (mkVP (mkV2 (mkV "leak")) info)
        (SyntaxEng.mkAdv in_Prep proc)));
    LeakAssertionILCP pol ispec info chan proc =
      mkS presentTense simultaneousAnt pol
      (mkCl ispec
       (mkVP
        (mkVP (mkV3 (mkV "leak") noPrep through_Prep) info chan)
        (SyntaxEng.mkAdv in_Prep proc)));
    LeakAssertionPILC pol proc ispec info chan =
      mkS
      (SyntaxEng.mkAdv on_Prep proc)
      (mkS presentTense simultaneousAnt pol
       (mkCl ispec (mkV3 (mkV "leak") noPrep through_Prep) info chan));


    ----------------------------------------------------------------------
    -- Instruction Specifications
    ----------------------------------------------------------------------

    AllISpec iclass = mkNP all_Predet (mkNP aPl_Det iclass);
    EveryISpec iclass = mkNP every_Det iclass;
    TheISpec iclass = mkNP thePl_Det iclass;
    OnlyISpec iclass = mkNP only_Predet (mkNP aPl_Det iclass);
    NoISpec iclass = mkNP no_Quant iclass;
    NoPluralISpec iclass = mkNP no_Quant pluralNum iclass;
    TheNamedISpec inst = mkNP theSg_Det inst;
    OnlyNamedISpec inst = mkNP only_Predet (mkNP theSg_Det inst);

    InstClassHasProc iclass proc = mkCN (mkN2 iclass part_Prep) proc;
    InstClassNoProc iclass = mkCN iclass;

    AnyInst = instruction;
    ArithInst = mkN "arithmetic" instruction;
    IntegerOp = mkN "integer" operation;
    FloatingPointOp = mkN "floating" (mkN "point" operation);
    BitwiseOp = mkN "bitwise" operation;
    ProcessorFence = mkN "processor" (mkN "fence");

    NamedInstHasProc inst proc =
      mkCN inst (SyntaxEng.mkAdv part_Prep proc);
    NamedInstNoProc inst = inst;

    A_NamedInst str = mkCN (mkN str.s instruction);
    A_NamedOp str = mkCN (mkN str.s operation);


    ----------------------------------------------------------------------
    -- Specifications of Processors
    ----------------------------------------------------------------------

    TheNamedPSpec named_p = mkNP theSg_Det named_p;
    MyNamedPSpec named_p = mkNP i_Pron named_p;

    -- Specificy named processors
    A_NamedProcessor str = mkN str.s processor;


    ----------------------------------------------------------------------
    -- Specification of Sorts of Timing Leakage
    ----------------------------------------------------------------------

    -- LeakSpecWithChannel info chan =
    --   -- mkVP (mkVP (mkV2 (mkV "leak")) info) (mkAdv through_Prep chan);
    --   mkVP (mkV3 (mkV "leak") noPrep through_Prep) info chan;
    -- LeakSpecNoChannel info = mkVP (mkV2 (mkV "leak")) info;

    TimingInfo = mkNP (mkN "timing" (mkN "information"));
    InputOperandValues = mkNP aPl_Det (mkN "input-operand" (mkN "value"));
    NoTimingInfo = mkNP no_Quant (mkN "timing" (mkN "information"));
    NoInputOperandValues =
      mkNP no_Quant pluralNum (mkN "input-operand" (mkN "value"));

    TimingSideChannel = mkNP aSg_Det (mkN "timing" (mkN "side-channel"));
    InformationFlowChannel =
      mkNP aSg_Det (mkN "information-flow" (mkN "channel"));


    ----------------------------------------------------------------------
    -- Miscellaneous
    ----------------------------------------------------------------------

    TrueBool = positivePol;
    FalseBool = negativePol;
}
