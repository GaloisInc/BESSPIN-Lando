--# -path=.:present

concrete InfoLeakageEng of InfoLeakageAbs = open SyntaxEng,ParadigmsEng in {
  lincat
    LeaksAssertion = S;
    InstSpec = NP;
    InstClass = N;
    Bool = Pol;

  oper
    timing_information : NP =
      mkNP (mkCN (mkN "timing") (mkNP (mkN "information")));

    -- The generic "instruction" noun
    instruction : N = mkN "instruction";

    -- The generic "operation" noun
    operation : N = mkN "operation";

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
    NoQuantISpec iclass = mkNP aPl_Det iclass;
    OnlyISpec iclass = mkNP only_Predet (mkNP aPl_Det iclass);
    NoISpec iclass = mkNP no_Quant iclass;

    AnyInst = instruction;
    ArithInst = mkN "arithmetic" instruction;
    IntegerOp = mkN "integer" operation;
    FloatingPointOp = mkN "floating" (mkN "point" operation);
    BitwiseOp = mkN "bitwise" operation;
    ProcessorFence = mkN "processor" (mkN "fence");

    
    ----------------------------------------------------------------------
    -- Miscellaneous
    ----------------------------------------------------------------------

    TrueBool = positivePol;
    FalseBool = negativePol;
}
