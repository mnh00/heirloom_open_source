       01  BMSEJNLI.
           02  FILLER PIC X(12).
           02  EJNLTRNL  COMP PIC S9(4).
           02  EJNLTRNF  PICTURE X.
           02  FILLER REDEFINES EJNLTRNF.
               03  EJNLTRNA  PICTURE X.
           02  EJNLTRNI  PIC X(4).
           02  EJNL1L  COMP PIC S9(4).
           02  EJNL1F  PICTURE X.
           02  FILLER REDEFINES EJNL1F.
               03  EJNL1A  PICTURE X.
           02  EJNL1I  PIC X(40).
           02  EJNL2L  COMP PIC S9(4).
           02  EJNL2F  PICTURE X.
           02  FILLER REDEFINES EJNL2F.
               03  EJNL2A  PICTURE X.
           02  EJNL2I  PIC X(40).
           02  EJNL3L  COMP PIC S9(4).
           02  EJNL3F  PICTURE X.
           02  FILLER REDEFINES EJNL3F.
               03  EJNL3A  PICTURE X.
           02  EJNL3I  PIC X(40).
           02  EJNL4L  COMP PIC S9(4).
           02  EJNL4F  PICTURE X.
           02  FILLER REDEFINES EJNL4F.
               03  EJNL4A  PICTURE X.
           02  EJNL4I  PIC X(40).
           02  EJNLMSGL  COMP PIC S9(4).
           02  EJNLMSGF  PICTURE X.
           02  FILLER REDEFINES EJNLMSGF.
               03  EJNLMSGA  PICTURE X.
           02  EJNLMSGI  PIC X(79).
       01  BMSEJNLO REDEFINES BMSEJNLI.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  EJNLTRNO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  EJNL1O  PIC X(40).
           02  FILLER PICTURE X(3).
           02  EJNL2O  PIC X(40).
           02  FILLER PICTURE X(3).
           02  EJNL3O  PIC X(40).
           02  FILLER PICTURE X(3).
           02  EJNL4O  PIC X(40).
           02  FILLER PICTURE X(3).
           02  EJNLMSGO  PIC X(79).
