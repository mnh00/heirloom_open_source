       01  BMSELPLI.
           02  FILLER PIC X(12).
           02  ELPLTRNL  COMP PIC S9(4).
           02  ELPLTRNF  PICTURE X.
           02  FILLER REDEFINES ELPLTRNF.
               03  ELPLTRNA  PICTURE X.
           02  ELPLTRNI  PIC X(4).
           02  ELPLXIDL  COMP PIC S9(4).
           02  ELPLXIDF  PICTURE X.
           02  FILLER REDEFINES ELPLXIDF.
               03  ELPLXIDA  PICTURE X.
           02  ELPLXIDI  PIC X(4).
           02  ELPLSIDL  COMP PIC S9(4).
           02  ELPLSIDF  PICTURE X.
           02  FILLER REDEFINES ELPLSIDF.
               03  ELPLSIDA  PICTURE X.
           02  ELPLSIDI  PIC X(4).
           02  ELPLPRML  COMP PIC S9(4).
           02  ELPLPRMF  PICTURE X.
           02  FILLER REDEFINES ELPLPRMF.
               03  ELPLPRMA  PICTURE X.
           02  ELPLPRMI  PIC X(50).
           02  ELPLMSGL  COMP PIC S9(4).
           02  ELPLMSGF  PICTURE X.
           02  FILLER REDEFINES ELPLMSGF.
               03  ELPLMSGA  PICTURE X.
           02  ELPLMSGI  PIC X(79).
       01  BMSELPLO REDEFINES BMSELPLI.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  ELPLTRNO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  ELPLXIDO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  ELPLSIDO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  ELPLPRMO  PIC X(50).
           02  FILLER PICTURE X(3).
           02  ELPLMSGO  PIC X(79).
