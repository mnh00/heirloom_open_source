       identification division.

       program-id. ETPETRN.

       author.    Heirloom Computing Inc.
       
      ******************************************************************
      *        Copyright (C) 2010-2013 Heirloom Computing Inc          *
      *                                                                *
      *                        ALL RIGHTS RESERVED                     *
      ******************************************************************
      *                                                                *
      * Property of Heirloom Computing Inc.  This software contains    *
      * confidential and proprietary information of Heirloom Computing *
      * Inc. and is made available solely pursuant to the terms of a   *
      * software license agreement, which governs its use.             *
      *                                                                *
      * No disclosure, reproduction or use of any portion of these     *
      * materials may be made without the express written consent of   *
      * Heirloom Computing Inc.                                        *
      *                                                                *
      ******************************************************************

      * ETPETRN - a non-BMS transaction to be LINKed to from ELPL
      * a non-BMS transaction (application transaction) that 
      * modifies the COMMAREA and returns
      * note:  journal1 must be set up to log info
      * note:  if incoming COMMAREA is 'ABEND' this transaction will ABEND
      *        and generate a Java traceback
      
       environment division.

       data division.

       working-storage section.
        COPY DFHAID. 

       01  ws-program-name                   pic x(8) value 'ETPETRN'.

       01  ws-cwa-ptr                        pointer.

       01  ws-commarea.
           03  ws-parms                      pic x(50).
           
       01  ws-info-msg                       pic x(50).

       01  result-code                       pic s9(5) comp.
       01  m-err                             pic x(80).

       linkage section.
       01  dfhcommarea.
           03                                pic x
               occurs 0 to 32700 times depending on eibcalen.

      ***--------------------------------------------------------------*
      *** ETRN Stuff                                                   *
      ***--------------------------------------------------------------*
       procedure division.
       main-para.
       
      *    this is an application transaction, no BMS map
      *    it should be invoked with 
      *          EXEC CICS LINK TRANSID('ETRN') COMMAREA(myca) END-EXEC
      
      *    print the comm area and modify it
           perform cics-mainline
           
      *    return to the caller
           exec cics return
               commarea(ws-commarea)
           end-exec
           stop run
           .
      ***--------------------------------------------------------------*
       cics-mainline section.
       cics-mainline-para.
      *    only do work if we received a COMMAREA as a parameter
           if eibcalen = length of ws-commarea
               move dfhcommarea(1:eibcalen) to ws-commarea
                
      *        note 2 above:  will ABEND if incoming COMMAREA is 'ABEND'
               if ws-commarea(1:5) = 'ABEND'
                   move "ETRN is being asked to ABEND" to ws-info-msg
                   exec cics 
                       write journalname('journal1')
                       from(ws-info-msg) flength(50)
                       jtypeid('RC') prefix('ETP')
                   end-exec
                   display ws-info-msg on syserr
                   exec cics
                       ABEND ABCODE('DEAD')
                   end-exec
               end-if
                
      *        return spaces and current date in commarea
               move spaces to ws-commarea(20:eibcalen)
               move current-date to ws-commarea(20:34)
               string "COMMAREA in/out:" 
                   ws-commarea(1:34) into ws-info-msg
           else
               move "No COMMAREA supplied" to ws-info-msg
           end-if
           
           exec cics 
               write journalname('journal1')
               from(ws-info-msg) flength(50)
               jtypeid('RC') prefix('ETP')
           end-exec
           display ws-info-msg on syserr
           exit
           .
       end-program.