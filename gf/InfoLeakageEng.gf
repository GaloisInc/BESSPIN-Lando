--# -path=.:present

concrete InfoLeakageEng of InfoLeakageAbs = open SyntaxEng,ParadigmsEng in {
  lincat
    LeaksAssertion = S;
    InstSpec = NP;
    InstClass = N;
    InstClassOfProc = CN;
    NamedInst = N;
    NamedInstOfProc = CN;
    ProcessorSpec = NP;
    NamedProcessor = N;
    Bool = Pol;

  oper
    timing_information : NP =
      mkNP (mkCN (mkN "timing") (mkNP (mkN "information")));

    -- Some generic nouns
    instruction : N = mkN "instruction";
    operation : N = mkN "operation";
    processor : N = mkN "processor";

    mk_all_NP : CN -> NP = \cn -> mkNP all_Predet (mkNP aPl_Det cn);


  lin

    ----------------------------------------------------------------------
    -- Assertions About Leakage
    ----------------------------------------------------------------------

    TimingLeaksAssertion pol ispec =
      mkS presentTense simultaneousAnt pol
        (mkCl ispec (mkV2 (mkV "leak")) timing_information);


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

    InstClassHasProc iclass proc = mkCN (mkN2 iclass part_Prep) proc;
    InstClassNoProc iclass = mkCN iclass;

    AnyInst = instruction;
    ArithInst = mkN "arithmetic" instruction;
    IntegerOp = mkN "integer" operation;
    FloatingPointOp = mkN "floating" (mkN "point" operation);
    BitwiseOp = mkN "bitwise" operation;
    ProcessorFence = mkN "processor" (mkN "fence");

    NamedInstHasProc inst proc = mkCN (mkN2 inst part_Prep) proc;
    NamedInstNoProc inst = mkCN inst;

    IntegerMultiplyOp_NamedInst = mkN "integer-multiplication" operation;
    Fmsub_s_NamedInst = mkN "Fmsub_s" instruction;


    ----------------------------------------------------------------------
    -- Specifications of Processors
    ----------------------------------------------------------------------

    TheNamedPSpec named_p = mkNP theSg_Det named_p;
    MyNamedPSpec named_p = mkNP i_Pron named_p;

    -- Specificy named processors
    Rocket_NamedProcessor = mkN "Rocket" processor;


    ----------------------------------------------------------------------
    -- Miscellaneous
    ----------------------------------------------------------------------

    TrueBool = positivePol;
    FalseBool = negativePol;
}
