{-
Copyright (c) 2020, Galois, Inc.
Author: Eddy Westbrook
This file was built as part of the BESSPIN project.
-}

--# -path=.:present

concrete InfoLeakageClafer of InfoLeakageAbs = open Prelude, Predef, Maybe in {
  -- param Boolean = True | False;
  -- param InstSpecRepr = { pos: String; neg: String };
  -- param MaybeStr = Just String | Nothing;

  lincat
    LeaksAssertion = Str;

    -- Instruction specs have optional positive and negative components; i.e.,
    -- they specify "all instructions in pos and no instructions in neg". Either
    -- of these classes can be missing (but both missing doesn't mean much...)
    InstSpec = PosNeg;

    InstClass = Str;
    NamedInst = Str;
    ProcessorSpec = Str;
    NamedProcessor = Str;

    -- Currently, the only "real" information in an InfoSpec is whether it is
    -- negated, as in "no timing information"
    InfoSpec = Bool;

    ChannelSpec = MaybeS;
    DataSpec = Str;
    NamedDatum = Str;
    ModuleSpec = Str;
    NamedModule = Str;
    BoundarySpec = Str;
    Boolean = Bool;

  oper
    PosNeg : Type = { pos: MaybeS; neg: MaybeS };

    -- Extract out the positive (if the Boolean is true) or negative (if the
    -- Boolean is false) of an InstSpec interpretation
    inst_spec_pos : Bool -> PosNeg -> MaybeS =
      \pol, ispec -> case pol of
        { True => ispec.pos;
          False => ispec.neg
        };

    -- Extract out the negative (if the Boolean is true) or positive (if the
    -- Boolean is false) of an InstSpec interpretation
    inst_spec_neg : Bool -> PosNeg -> MaybeS =
      \pol, ispec -> case pol of
        { True => ispec.neg;
          False => ispec.pos
        };

    -- Build the string prefix ++ str if the MaybeS input is Just
    -- str. Otherwise, return Nothing
    prefix_maybe : Str -> MaybeS -> MaybeS =
      \prefix,maybe_str ->
       if_then_else MaybeS maybe_str.exists
         (JustS (prefix ++ maybe_str.inner))
         NothingS;

    -- Form the conjunction of two MaybeSs that represent constraints, where
    -- Nothing is the vacuously true constraint
    and_maybes : MaybeS -> MaybeS -> MaybeS =
      \c1,c2 ->
        if_then_else MaybeS c1.exists
          (if_then_else MaybeS c2.exists
             (JustS (c1.inner ++ "&&" ++ c2.inner))
             c1)
          c2;

    -- "Run" a MaybeS, returning the empty string for Nothing
    run_maybe : MaybeS -> Str =
      \m -> if_then_else Str m.exists m.inner [];

    -- not : Bool -> Bool =
    --   \b -> case b of { True => False; False => True };
    xorB : Bool -> Bool -> Bool =
      \b1, b2 -> if_then_else Bool b1 b2 (notB b2);

    mk_timing_leakage : Bool -> PosNeg -> MaybeS -> Str =
      \pol,ispec,maybe_proc ->
        "timing_leakage" ++ "[" ++
        run_maybe
          (and_maybes
             (prefix_maybe ("leaking_instructions" ++ "=")
                           (inst_spec_pos pol ispec))
             (and_maybes
                (prefix_maybe ("nonleaking_instructions" ++ "=")
                              (inst_spec_neg pol ispec))
                (prefix_maybe ("soc_under_test" ++ "=") maybe_proc)))
        ++ "]";


    mk_digital_leakage : Bool -> Str -> Str -> MaybeS -> MaybeS -> Str =
      \b,src_sig,src_mod,maybe_dest,maybe_boundary ->
        "digital_information_leakage" ++ "[" ++
        "source.sig" ++ "=" ++ src_sig ++ "&&" ++
        "source.mod" ++ "=" ++ src_mod ++ "&&" ++
        run_maybe
          (and_maybes
             (prefix_maybe ("destination.wire.mod" ++ "=" ++ src_mod ++ "&&" ++
                            "destination.wire.sig" ++ "=") maybe_dest)
             (and_maybes
                (prefix_maybe ("destination.boundary" ++ "=") maybe_boundary)
                (JustS
                 (if_then_else Str b "leak" "!leak"))))
        ++ "]";

  lin

    ----------------------------------------------------------------------
    -- Assertions About Leakage
    ----------------------------------------------------------------------

    ILeakAssertionIL pol ispec info =
      mk_timing_leakage (xorB pol info) ispec NothingS;
    ILeakAssertionIPL pol ispec proc info =
      mk_timing_leakage (xorB pol info) ispec (JustS proc);
    ILeakAssertionILC pol ispec info chan =
      mk_timing_leakage (xorB pol info) ispec NothingS;
      -- error "Instruction timing leakages with channels are not supported";
    ILeakAssertionIPLC pol ispec proc info chan =
      mk_timing_leakage (xorB pol info) ispec (JustS proc);
      -- error "Instruction timing leakages with channels are not supported";
    ILeakAssertionILP pol ispec info proc =
      mk_timing_leakage (xorB pol info) ispec (JustS proc);
    ILeakAssertionILCP pol ispec info chan proc =
      mk_timing_leakage (xorB pol info) ispec (JustS proc);
      -- error "Instruction timing leakages with channels are not supported";
    ILeakAssertionPILC pol proc ispec info chan =
      mk_timing_leakage (xorB pol info) ispec (JustS proc);
      -- error "Instruction timing leakages with channels are not supported";


    DLeakAssertionDMC pol d mod chan =
      mk_digital_leakage pol d mod chan NothingS;
    DLeakAssertionDMPC pol d mod proc chan =
      mk_digital_leakage pol d mod chan NothingS;
    DLeakAssertionDMB pol d mod bound =
      mk_digital_leakage pol d mod NothingS (JustS bound);
    DLeakAssertionDMPB pol d mod proc bound =
      mk_digital_leakage pol d mod NothingS (JustS bound);


    ----------------------------------------------------------------------
    -- Instruction Specifications
    ----------------------------------------------------------------------

    AllISpec iclass = { pos = JustS iclass; neg = NothingS };
    EveryISpec iclass = { pos = JustS iclass; neg = NothingS };
    TheISpec iclass = { pos = JustS iclass; neg = NothingS };
    OnlyISpec iclass =
      { pos = JustS iclass; neg = JustS ("instr" ++ "--" ++ iclass) };
    OnlyOutOfISpec iclass1 iclass2 =
      { pos = JustS iclass1; neg = JustS (iclass2 ++ "--" ++ iclass1) };
    OnlyNamedOutOfISpec inst iclass =
      { pos = JustS inst; neg = JustS (iclass ++ "--" ++ inst) };
    NoISpec iclass = { pos = NothingS; neg = JustS iclass };
    NoPluralISpec iclass = { pos = NothingS; neg = JustS iclass };
    TheNamedISpec inst = { pos = JustS inst; neg = NothingS };
    OnlyNamedISpec inst =
      { pos = JustS inst; neg = JustS ("instr" ++ "--" ++ inst) };

    AnyInst = "instr";
    Named_InstClass str = str.s;
    Named_InstClass_Op str = str.s;
    A_NamedInst str = str.s;
    A_NamedOp str = str.s;


    ----------------------------------------------------------------------
    -- Specifications of Processors
    ----------------------------------------------------------------------

    TheNamedPSpec named_p = named_p;
    MyNamedPSpec named_p = named_p;
    OurNamedPSpec named_p = named_p;
    A_NamedProcessor str = str.s;


    ----------------------------------------------------------------------
    -- Specification of Sorts of Timing Leakage
    ----------------------------------------------------------------------

    -- LeakSpecWithChannel info chan =
    --   -- mkVP (mkVP leak_V2 info) (SyntaxEng.mkAdv through_Prep chan);
    --   mkVP leak_through info chan;
    -- LeakSpecNoChannel info = mkVP leak_V2 info;

    TimingInfo = True;
    InputOperandValues = True;
    NoTimingInfo = False;
    NoInputOperandValues = False;

    -- NOTE: channels are not yet used, so all of these are the empty string
    TimingSideChannel = NothingS;
    InformationFlowChannel = NothingS;
    NamedOutputChannel str = JustS str.s;


    ----------------------------------------------------------------------
    -- Specification of Data and Modules
    ----------------------------------------------------------------------

    TheNamedDatum d = d;
    NamedRegister str = str.s;
    NamedSignal str = str.s;

    TheNamedModule m = m;
    MyNamedModule m = m;
    ANamedModule str = str.s;
    NamedVerilogModule str = str.s;
    NamedBlock str = str.s;

    TheModuleCertBoundary mod = mod;
    MyModuleCertBoundary mod = mod;


    ----------------------------------------------------------------------
    -- Miscellaneous
    ----------------------------------------------------------------------

    TrueBool = True;
    FalseBool = False;
}
