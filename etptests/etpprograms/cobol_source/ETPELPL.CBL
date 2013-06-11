       identification division.

       program-id. ETPELPL.

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

      * ETPELPL - CICS LINK and CICS RETURN TRANSID to local or 
      * distributed transactions
      * Set-up required in project.etp_deploy_settings for Geronimo
      * to Geronimo (CICS ISC) communication (Program Control tab and 
      * multiple ETP Projects corresponding to companion CICS regions.
      * 1) Use Enter, TRANSID of EMNU and no SYSID 
      *    to issue CICS RETURN TRANSID(EMNU)
      * 2) Use PF6, TRANSID of ETRN and SYSID of this region
      *    to issue CICS LINK TRANSID(ETRN) SYSID(ETP1)
      * 3) Use PF6, TRANSID of ETRN and SYSID of companion region 
      *    to issue CICS LINK TRANSID(ETRN) SYSID(ETP2)
 
       environment division.

       data division.

       working-storage section.
        COPY DFHAID. 

       01  ws-program-name                   pic x(8) value 'ETPELPL'.

      ***--------------------------------------------------------------*
      *** Menu                                                         *
      ***--------------------------------------------------------------*        
       copy BMSELPL. 

       01 filler.

       01  ws-cwa-ptr                        pointer.

       01  ws-end-msg                        pic x(30)
           value '     ELPL session terminated.'.
       01  ws-info-msg                       pic x(30).
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
      *** ELPL Stuff                                                   *
      ***--------------------------------------------------------------*
       procedure division.
       main-para.
           move low-values to ws-next-tran.
       
      *    enter this main due to (1) ELPL transfer from another 
      *        transaction or (2) return map from this transaction
           perform cics-mainline
           
      *    return to ELPL because we're (1) waiting to receive our map
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
                
      *            PF12 Exits the ELPL-supported Transactions
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
 
                   when DFHPF6
      *                *> PF 6 Key -- DPL to etp2/elpl
                       exec cics link
                           transid(ELPLXIDI) 
                           sysid(ELPLSIDI)
                           commarea(ELPLPRMI)
                       end-exec
                       perform cics-send-cics

      *            ENTER attempts issue that transaction
                   when DFHENTER
                       if ELPLXIDI = low-values
                           move 'Must supply a transaction ID' 
                               to ELPLMSGO
                           perform cics-send-cics
                       else
                           move ELPLXIDI to ws-next-tran
                           if ELPLPRMI NOT = low-values
                               move ELPLPRMI to ws-parms
                           else
                               move SPACES to ws-parms
                           end-if
                       end-if
                       
      *            CLEAR key refreshes the screen
                   when DFHCLEAR
      *            *> Clear Key
                       move 'Display reset by clear key'
                           to ELPLMSGO
                       move ELPLMSGO(1:30) to ws-info-msg
                       exec cics 
                           write journalname('info')
                           from(ws-info-msg) flength(30)
                           jtypeid('RC')
                       end-exec
                       perform cics-send-cics
                   when other
                       move 'Unexpected 3270 attention key'                       
                           to ELPLMSGO
                       move ELPLMSGO(1:30) to ws-info-msg
                       exec cics 
                           write journalname('info')
                           from(ws-info-msg) flength(30)
                           jtypeid('RC')
                       end-exec
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
               into(BMSELPLI)
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
           move low-values to ELPLXIDO.
           move low-values to ELPLPRMO.

           move 'BMSELPL' to ws-map
           if eibcalen > 0
               move low-values to ELPLPRMO
               move dfhcommarea(1:eibcalen) to ELPLPRMO
           end-if

           exec cics send map(ws-map) from(BMSELPLO) erase end-exec
           exit
           .
       end-program.