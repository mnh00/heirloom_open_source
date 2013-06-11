       01  BMSEPICI.
           02  FILLER PIC X(12).
           02  EPICTRNL  COMP PIC S9(4).
           02  EPICTRNF  PICTURE X.
           02  FILLER REDEFINES EPICTRNF.
               03  EPICTRNA  PICTURE X.
           02  EPICTRNI  PIC X(4).
           02  EPICZZ9L  COMP PIC S9(4).
           02  EPICZZ9F  PICTURE X.
           02  FILLER REDEFINES EPICZZ9F.
               03  EPICZZ9A  PICTURE X.
           02  EPICZZ9I  PIC X(5).
           02  EPIC$99L  COMP PIC S9(4).
           02  EPIC$99F  PICTURE X.
           02  FILLER REDEFINES EPIC$99F.
               03  EPIC$99A  PICTURE X.
           02  EPIC$99I  PIC 99999999V99.
           02  EPICMSGL  COMP PIC S9(4).
           02  EPICMSGF  PICTURE X.
           02  FILLER REDEFINES EPICMSGF.
               03  EPICMSGA  PICTURE X.
           02  EPICMSGI  PIC X(79).
       01  BMSEPICO REDEFINES BMSEPICI.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  EPICTRNO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  EPICZZ9O  PIC ZZZZ9.
           02  FILLER PICTURE X(3).
           02  EPIC$99O  PIC $$$,$$9.99.
           02  FILLER PICTURE X(3).
           02  EPICMSGO  PIC X(79).
