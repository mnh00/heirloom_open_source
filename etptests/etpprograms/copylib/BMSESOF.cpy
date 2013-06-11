       01  BMSESOFI.
           02  FILLER PIC X(12).
           02  ESOFTRNL  COMP PIC S9(4).
           02  ESOFTRNF  PICTURE X.
           02  FILLER REDEFINES ESOFTRNF.
               03  ESOFTRNA  PICTURE X.
           02  ESOFTRNI  PIC X(4).
           02  ESOFCFML  COMP PIC S9(4).
           02  ESOFCFMF  PICTURE X.
           02  FILLER REDEFINES ESOFCFMF.
               03  ESOFCFMA  PICTURE X.
           02  ESOFCFMI  PIC X(1).
           02  ESOFMSGL  COMP PIC S9(4).
           02  ESOFMSGF  PICTURE X.
           02  FILLER REDEFINES ESOFMSGF.
               03  ESOFMSGA  PICTURE X.
           02  ESOFMSGI  PIC X(79).
       01  BMSESOFO REDEFINES BMSESOFI.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  ESOFTRNO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  ESOFCFMO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  ESOFMSGO  PIC X(79).
