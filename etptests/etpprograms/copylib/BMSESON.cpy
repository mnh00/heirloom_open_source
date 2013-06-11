       01  BMSESONI.
           02  FILLER PIC X(12).
           02  ESONTRNL  COMP PIC S9(4).
           02  ESONTRNF  PICTURE X.
           02  FILLER REDEFINES ESONTRNF.
               03  ESONTRNA  PICTURE X.
           02  ESONTRNI  PIC X(4).
           02  ESONUIDL  COMP PIC S9(4).
           02  ESONUIDF  PICTURE X.
           02  FILLER REDEFINES ESONUIDF.
               03  ESONUIDA  PICTURE X.
           02  ESONUIDI  PIC X(8).
           02  ESONPWDL  COMP PIC S9(4).
           02  ESONPWDF  PICTURE X.
           02  FILLER REDEFINES ESONPWDF.
               03  ESONPWDA  PICTURE X.
           02  ESONPWDI  PIC X(8).
           02  ESONNPWL  COMP PIC S9(4).
           02  ESONNPWF  PICTURE X.
           02  FILLER REDEFINES ESONNPWF.
               03  ESONNPWA  PICTURE X.
           02  ESONNPWI  PIC X(8).
           02  ESONMSGL  COMP PIC S9(4).
           02  ESONMSGF  PICTURE X.
           02  FILLER REDEFINES ESONMSGF.
               03  ESONMSGA  PICTURE X.
           02  ESONMSGI  PIC X(79).
       01  BMSESONO REDEFINES BMSESONI.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  ESONTRNO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  ESONUIDO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  ESONPWDO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  ESONNPWO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  ESONMSGO  PIC X(79).
