       identification division.

       program-id. ETPEJNL.

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
      
      * ETPEJNL - CICS Journal functionality
      * Define 4 journals in the 'project.etp_deploy_settings' file
      *    - journal1 -- as a SYSOUT journal
      *    - journal2 -- as a SYSERR journal
      *    - journal3 -- as a log4j journal
      *    - journal4 -- as a TextFile journal to c:\\temp\\journal4.txt
      *                  or /tmp/journal4.txt

       environment division.

       data division.

       working-storage section.
        COPY DFHAID. 

       01  ws-program-name                   pic x(8) value 'ETPEJNL'.

      ***--------------------------------------------------------------*
      *** Menu                                                         *
      ***--------------------------------------------------------------*        
       copy BMSEJNL. 

       01 filler.

       01  ws-cwa-ptr                        pointer.

       01  ws-end-msg                        pic x(30)
           value '     EJNL session terminated.'.
       01  ws-info-msg                       pic x(40).
       01  ws-work.
           03  ws-next-tran                  pic x(4).

       01  ws-commarea.
           03  ws-map                        pic x(8).
           03  ws-parms                      pic x(50).

       01  result-code                       pic s9(5) comp.
       01  m-err                             pic x(80).

       linkage section.
       01  dfhcommarea.
           03                                pic x
               occurs 0 to 32700 times depending on eibcalen.

      ***--------------------------------------------------------------*
      *** EJNL Stuff                                                   *
      ***--------------------------------------------------------------*
       procedure division.
       main-para.
           move low-values to ws-next-tran.
       
      *    enter this main due to (1) EJNL transfer from another 
      *        transaction or (2) return map from this transaction
           perform cics-mainline
           
      *    return to EJNL because we're (1) waiting to receive our map
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
       cics-mainline section.
       cics-mainline-para.
      *    determine if we are receiving our own map
           if eibcalen = length of ws-commarea
                move dfhcommarea to ws-commarea
                
      *         receive the map of our screen (if not CLEAR)
                if eibaid not = DFHCLEAR  
                    perform cics-receive-map
                end-if
                
      *         carry out requested operation based on AID key
                evaluate eibaid
                
      *            PF12 Exits the EJNL-supported Transactions
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
 
      *            ENTER attempts issue that transaction
                   when DFHENTER 
                       if EJNL1I NOT = low-values
                           move EJNL1I to ws-info-msg
                           exec cics 
                               write journalname('journal1')
                               from(ws-info-msg) flength(40)
                               jtypeid('RC') prefix('ETP')
                           end-exec
                       end-if
                       if EJNL2I NOT = low-values
                           move EJNL2I to ws-info-msg
                           exec cics 
                               write journalname('journal2')
                               from(ws-info-msg) flength(40)
                               jtypeid('RC') prefix('ETP')
                           end-exec
                       end-if
                       if EJNL3I NOT = low-values
                           move EJNL3I to ws-info-msg
                           exec cics 
                               write journalname('journal3')
                               from(ws-info-msg) flength(40)
                               jtypeid('RC') prefix('ETP')
                           end-exec
                       end-if
                       if EJNL4I NOT = low-values
                           move EJNL4I to ws-info-msg
                           exec cics 
                               write journalname('journal4')
                               from(ws-info-msg) flength(40)
                               jtypeid('RC') prefix('ETP')
                           end-exec
                       end-if
      *                resend the same screen with Enter
                       perform cics-send-cics
                       
      *            CLEAR key refreshes the screen
                   when DFHCLEAR
      *            *> Clear Key
                       move 'Display reset by clear key'
                           to EJNLMSGO
                       perform cics-send-cics
                   when other
                       move 'Unexpected 3270 attention key'                       
                           to EJNLMSGO
                       perform cics-send-cics
               end-evaluate
           else
               perform cics-send-cics
           end-if
           exit
           .
      ***--------------------------------------------------------------*
       cics-receive-map section.
       cics-receive-map-para.
           exec cics 
               receive map(ws-map)
               into(BMSEJNLI)
               nohandle
           end-exec
           if eibresp = dfhresp(normal)
               move low-values to ws-next-tran
           end-if
           exit
           .
      ***--------------------------------------------------------------*
       cics-send-cics section.
       cics-send-cics-para.
           move low-values to EJNL1O.
           move low-values to EJNL2O.
           move low-values to EJNL3O.
           move low-values to EJNL4O.

           move 'BMSEJNL' to ws-map

           exec cics send map(ws-map) from(BMSEJNLO) erase end-exec
           exit
           .
       end-program.