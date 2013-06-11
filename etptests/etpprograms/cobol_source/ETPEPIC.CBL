       identification division.

       program-id. ETPEPIC.

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
      
      * ETPEPIC - CICS send/receive map with LANG=COBOL PICIN, PICOUT
      * Enter in numbers in the fields, values are displayed on SYSERR
      * and echoed to the screen with decimal or dollar-sign PICTUREs
      * (the var/log/geronimo.out file or, when started in Eclipse,
      *  the Geronimo console)

       environment division.

       data division.

       working-storage section.
        COPY DFHAID. 

       01  ws-program-name                   pic x(8) value 'ETPEPIC'.

      ***--------------------------------------------------------------*
      *** Menu                                                         *
      ***--------------------------------------------------------------*        
       copy BMSEPIC. 

       01 filler.

       01  ws-cwa-ptr                        pointer.

       01  ws-end-msg                        pic x(30)
           value '     CICS session terminated.'.
       01  ws-info-msg                       pic x(50).
       01  ws-tst-zz9                        pic zzzz9.
       01  ws-tst-$99                        pic $$$,$$9.99.
       01  ws-tst-999                        pic 99999.
       01  ws-tst-9v9                        pic 999999V99.
       01  ws-work.
           03  ws-next-tran                  pic x(4).

       01  ws-commarea.
           03  ws-map                        pic x(8).
           03  ws-cfm                        pic x(1).

       01  result-code                       pic s9(5) comp.
       01  m-err                             pic x(80).

       linkage section.
       01  dfhcommarea.
           03                                pic x
               occurs 0 to 32700 times depending on eibcalen.

      ***--------------------------------------------------------------*
      *** CICS Stuff                                                   *
      ***--------------------------------------------------------------*
       procedure division.
       main-para.
           move low-values to ws-next-tran.
       
      *    enter this main due to (1) CICS transfer from another 
      *        transaction or (2) return map from this transaction
           perform ctst-mainline
           
      *    return to CICS because we're (1) waiting to receive our map
      *        or (2) we want to transfer to a different transaction
           if ws-next-tran = low-values
               exec cics return
                   transid(eibtrnid)
                   commarea(ws-commarea)
               end-exec
           else
               move ws-next-tran to eibtrnid
               exec cics return immediate
                   transid(eibtrnid)
               end-exec
           end-if.
           stop run
           .
      ***--------------------------------------------------------------*
       ctst-mainline section.
       ctst-mainline-para.
      *    determine if we are receiving our own map
      *    (shouldn't we check if ws-map = 'BMSEPIC' ??)
           if eibcalen = length of ws-commarea
                move dfhcommarea to ws-commarea
                
      *         receive the map of our screen (if not CLEAR)
                if eibaid not = DFHCLEAR  
                    perform cics-receive-map
                end-if
                
      *         carry out requested operation based on AID key
                evaluate eibaid
                
      *            PF12 Exits the CICS-supported Transactions
                   when DFHPF12
      *                *> PF 12 Key
                       exec cics syncpoint end-exec
                       move eibtrnid to ws-end-msg(1:4)
                       exec cics send text | added text
                           from(ws-end-msg)
                           erase
                       end-exec
                       exec cics return end-exec

      *            PF3 transfers to EMNU menu
                   WHEN DFHPF3
                       move 'EMNU' to ws-next-tran
 
      *            ENTER prints the current values and re-sends the screen
                   when DFHENTER
      *                print current values of fields on the screen
      *                don't use Journals since ETPEJNL tests this
                       string "Current Screen Values"
                           into ws-info-msg
                       display ws-info-msg upon syserr
                       
                       string "EPICZZ9=" EPICZZ9I SPACE
                           "EPIC$99=" EPIC$99I
                           into ws-info-msg
                       display ws-info-msg upon syserr
                       
                       move EPIC$99I to ws-tst-$99
                       move EPICZZ9I to ws-tst-zz9
                       string "ws-tst-zz9=" ws-tst-zz9 SPACE
                           "ws-tst-$99=" ws-tst-$99
                           into ws-info-msg
                       display ws-info-msg upon syserr
                       
                       move EPIC$99I to ws-tst-9v9
                       move EPICZZ9I to ws-tst-999
                       string "ws-tst-9v9=" ws-tst-9v9 SPACE
                           "ws-tst-999=" ws-tst-999
                           into ws-info-msg
                       display ws-info-msg upon syserr
                       
      *                resubmit the screen
                       perform cics-send-ctst
                       
      *            CLEAR key refreshes the screen
                   when DFHCLEAR
      *            *> Clear Key
                       move 'Display reset by clear key'
                           to EPICMSGO
                       move EPICMSGO(1:50) to ws-info-msg
                       exec cics 
                           write journalname('info')
                           from(ws-info-msg) flength(50)
                           jtypeid('RC') prefix('ETP')
                       end-exec
                       perform cics-send-ctst
                   when other
                       move 'Unexpected 3270 attention key'                       
                           to EPICMSGO
                       move EPICMSGO(1:50) to ws-info-msg
                       exec cics 
                           write journalname('info')
                           from(ws-info-msg) flength(50)
                           jtypeid('RC') prefix('ETP')
                       end-exec
                       perform cics-send-ctst
               end-evaluate
           else
               move 0.0 to ws-tst-zz9
               move 0.0 to ws-tst-$99
               perform cics-send-ctst
           end-if
           exit
           .
      ***--------------------------------------------------------------*
       cics-receive-map section.
       cics-receive-map-para.
           evaluate ws-map
               when 'BMSEPIC'
                   exec cics 
                       receive map(ws-map)
                       into(BMSEPICI)
                       nohandle
                   end-exec
                   if eibresp = dfhresp(normal)
                       move low-values to ws-next-tran
                   end-if
           end-evaluate
           exit
           .
      ***--------------------------------------------------------------*
       cics-send-ctst section.
       cics-send-ctst-para.
           move ws-tst-zz9 to EPICZZ9O.
           move ws-tst-$99 to EPIC$99O.

           move 'BMSEPIC' to ws-map

           exec cics send map(ws-map) from(BMSEPICO) erase end-exec
           exit
           .
       end-program.