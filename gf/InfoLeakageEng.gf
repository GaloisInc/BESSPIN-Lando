--# -path=.:present

concrete InfoLeakageEng of InfoLeakageAbs = open SyntaxEng,ParadigmsEng in {
  lincat
    LeaksAssertion = S;
    InstructionSpec = NP;
    Bool = Pol;

  oper
    timing_information : NP =
      mkNP (mkCN (mkN "timing") (mkNP (mkN "information")));

    -- The generic "instruction" noun
    instruction : N = mkN "instruction";

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

    AllInstructions = mk_all_NP instruction;
    ArithInstructions = mk_all_NP (mkCN (mkN "arithmetic" instruction));
    -- NamedInstruction str =
    --   mkNP (mkCN (mkN str.s) (mkNP instruction_N));

    
    ----------------------------------------------------------------------
    -- Miscellaneous
    ----------------------------------------------------------------------

    TrueBool = positivePol;
    FalseBool = negativePol;
}
