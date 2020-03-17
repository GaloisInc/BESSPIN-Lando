--# -path=.:present

concrete InfoLeakageEng of InfoLeakageAbs = open SyntaxEng,ParadigmsEng in {
  lincat
    LeaksAssertion = S;
    InstSpec = NP;
    InstClass = N;
    NamedInst = CN;
    ProcessorSpec = NP;
    NamedProcessor = N;
    InfoSpec = NP;
    ChannelSpec = NP;
    DataSpec = NP;
    NamedDatum = N;
    ModuleSpec = NP;
    NamedModule = N;
    BoundarySpec = NP;
    Bool = Pol;

  oper
    timing_information : NP =
      mkNP (mkCN (mkN "timing") (mkNP (mkN "information")));

    -- Some generic nouns
    instruction : N = mkN "instruction";
    operation : N = mkN "operation";
    processor : N = mkN "processor";
    cert_boundary : N = mkN "certification" (mkN "boundary");

    -- The transitive form of the very "to leak"
    leak_V2 : V2 = mkV2 (mkV "leak");

    -- The transitive form of the very "to leak" with a "through" clause
    leak_through : V3 = mkV3 (mkV "leak") noPrep through_Prep;


  lin

    ----------------------------------------------------------------------
    -- Assertions About Leakage
    ----------------------------------------------------------------------

    ILeakAssertionIL pol ispec info =
      mkS presentTense simultaneousAnt pol
      (mkCl ispec leak_V2 info);
    ILeakAssertionIPL pol ispec proc info =
      mkS presentTense simultaneousAnt pol
      (mkCl (mkNP ispec (SyntaxEng.mkAdv part_Prep proc))
            leak_V2 info);
    ILeakAssertionILC pol ispec info chan =
      mkS presentTense simultaneousAnt pol
      (mkCl ispec leak_through info chan);
    ILeakAssertionIPLC pol ispec proc info chan =
      mkS presentTense simultaneousAnt pol
      (mkCl (mkNP ispec (SyntaxEng.mkAdv part_Prep proc))
            leak_through info chan);
    ILeakAssertionILP pol ispec info proc =
      mkS presentTense simultaneousAnt pol
      (mkCl ispec
       (mkVP
        (mkVP leak_V2 info)
        (SyntaxEng.mkAdv in_Prep proc)));
    ILeakAssertionILCP pol ispec info chan proc =
      mkS presentTense simultaneousAnt pol
      (mkCl ispec
       (mkVP
        (mkVP leak_through info chan)
        (SyntaxEng.mkAdv in_Prep proc)));
    ILeakAssertionPILC pol proc ispec info chan =
      mkS
      (SyntaxEng.mkAdv on_Prep proc)
      (mkS presentTense simultaneousAnt pol
       (mkCl ispec leak_through info chan));

    DLeakAssertionDMC pol d mod chan =
      mkS presentTense simultaneousAnt pol
      (mkCl (mkNP d (SyntaxEng.mkAdv part_Prep mod))
            (mkVP (passiveVP leak_V2)
                  (SyntaxEng.mkAdv through_Prep chan)));
    DLeakAssertionDMPC pol d mod proc chan =
      mkS presentTense simultaneousAnt pol
      (mkCl (mkNP (mkNP d (SyntaxEng.mkAdv part_Prep mod))
                  (SyntaxEng.mkAdv part_Prep proc))
            (mkVP (passiveVP leak_V2)
                  (SyntaxEng.mkAdv through_Prep chan)));
    DLeakAssertionDMB pol d mod bound =
      mkS presentTense simultaneousAnt pol
      (mkCl (mkNP d (SyntaxEng.mkAdv part_Prep mod))
            (mkVP (passiveVP leak_V2)
                  (SyntaxEng.mkAdv (mkPrep "outside of") bound)));
    DLeakAssertionDMPB pol d mod proc bound =
      mkS presentTense simultaneousAnt pol
      (mkCl (mkNP (mkNP d (SyntaxEng.mkAdv part_Prep mod))
                  (SyntaxEng.mkAdv part_Prep proc))
            (mkVP (passiveVP leak_V2)
                  (SyntaxEng.mkAdv (mkPrep "outside of") bound)));


    ----------------------------------------------------------------------
    -- Instruction Specifications
    ----------------------------------------------------------------------

    AllISpec iclass = mkNP all_Predet (mkNP aPl_Det iclass);
    EveryISpec iclass = mkNP every_Det iclass;
    TheISpec iclass = mkNP thePl_Det iclass;
    OnlyISpec iclass = mkNP only_Predet (mkNP aPl_Det iclass);
    OnlyOutOfISpec iclass1 iclass2 =
      mkNP (mkNP only_Predet (mkNP thePl_Det iclass1))
           (SyntaxEng.mkAdv (mkPrep "out of")
            (mkNP all_Predet (mkNP thePl_Det iclass2)));
    OnlyNamedOutOfISpec inst iclass =
      mkNP (mkNP only_Predet (mkNP theSg_Det inst))
           (SyntaxEng.mkAdv (mkPrep "out of")
            (mkNP all_Predet (mkNP thePl_Det iclass)));
    NoISpec iclass = mkNP no_Quant iclass;
    NoPluralISpec iclass = mkNP no_Quant pluralNum iclass;
    TheNamedISpec inst = mkNP theSg_Det inst;
    OnlyNamedISpec inst = mkNP only_Predet (mkNP theSg_Det inst);

    AnyInst = instruction;
    Named_InstClass str = mkN str.s instruction;
    Named_InstClass_Op str = mkN str.s operation;
    A_NamedInst str = mkCN (mkN str.s instruction);
    A_NamedOp str = mkCN (mkN str.s operation);


    ----------------------------------------------------------------------
    -- Specifications of Processors
    ----------------------------------------------------------------------

    TheNamedPSpec named_p = mkNP theSg_Det named_p;
    MyNamedPSpec named_p = mkNP i_Pron named_p;
    OurNamedPSpec named_p = mkNP we_Pron named_p;

    -- Specificy named processors
    A_NamedProcessor str = mkN str.s processor;


    ----------------------------------------------------------------------
    -- Specification of Sorts of Timing Leakage
    ----------------------------------------------------------------------

    -- LeakSpecWithChannel info chan =
    --   -- mkVP (mkVP leak_V2 info) (SyntaxEng.mkAdv through_Prep chan);
    --   mkVP leak_through info chan;
    -- LeakSpecNoChannel info = mkVP leak_V2 info;

    TimingInfo = mkNP (mkN "timing" (mkN "information"));
    InputOperandValues = mkNP aPl_Det (mkN "input-operand" (mkN "value"));
    NoTimingInfo = mkNP no_Quant (mkN "timing" (mkN "information"));
    NoInputOperandValues =
      mkNP no_Quant pluralNum (mkN "input-operand" (mkN "value"));

    TimingSideChannel = mkNP aSg_Det (mkN "timing" (mkN "side-channel"));
    InformationFlowChannel =
      mkNP aSg_Det (mkN "information-flow" (mkN "channel"));
    NamedOutputChannel str =
      mkNP theSg_Det (mkN str.s (mkN "output" (mkN "channel")));


    ----------------------------------------------------------------------
    -- Specification of Data and Modules
    ----------------------------------------------------------------------

    TheNamedDatum d = mkNP theSg_Det d;
    NamedRegister str = mkN str.s (mkN "register");
    NamedSignal str = mkN str.s (mkN "signal");

    TheNamedModule m = mkNP theSg_Det m;
    MyNamedModule m = mkNP i_Pron m;
    ANamedModule str = mkN str.s (mkN "module");
    NamedVerilogModule str = mkN str.s (mkN "Verilog" (mkN "module"));
    NamedBlock str = mkN str.s (mkN "block");

    TheModuleCertBoundary mod =
      mkNP (mkNP theSg_Det cert_boundary)
           (SyntaxEng.mkAdv possess_Prep mod);
    MyModuleCertBoundary mod =
      mkNP (mkNP i_Pron cert_boundary)
           (SyntaxEng.mkAdv possess_Prep mod);


    ----------------------------------------------------------------------
    -- Miscellaneous
    ----------------------------------------------------------------------

    TrueBool = positivePol;
    FalseBool = negativePol;
}
