--# -path=.:present

concrete InfoLeakageEng of InfoLeakageAbs = open SyntaxEng,ParadigmsEng in {
  lincat
    LeaksAssertion = S;
    InstructionSpec = NP;
    Bool = Pol;

  oper
    timing_information : NP =
      mkNP (mkCN (mkN "timing") (mkNP (mkN "information")));
    instruction_N : N = mkN "instruction";


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

    AllInstructions = mkNP all_Predet (mkNP aPl_Det instruction_N);
    ArithInstructions =
      mkNP (mkCN (mkN "arithmetic") (mkNP instruction_N));
    -- NamedInstruction str =
    --   mkNP (mkCN (mkN str.s) (mkNP instruction_N));

    
    ----------------------------------------------------------------------
    -- Miscellaneous
    ----------------------------------------------------------------------

    TrueBool = positivePol;
    FalseBool = negativePol;
}
