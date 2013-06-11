       identification division.

       program-id. ETPESOF.

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

      * ETPESOF - CICS SIGNOFF
      * When started with JAAS security manager (-Djavax.security.manager
      * and other settings) will invoke JAAS sign-off operation

       environment division.

       data division.

       working-storage section.
        COPY DFHAID.

       01  ws-program-name                   pic x(8) value 'ETPESOF'.

      ***--------------------------------------------------------------*
      *** Menu                                                         *
      ***--------------------------------------------------------------*        
       copy BMSESOF. 

       01 filler.

       01  ws-cwa-ptr                        pointer.

       01  ws-end-msg                        pic x(30)
           value '     CICS session terminated.'.
       01  ws-info-msg                       pic x(30).
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
           perform cesf-mainline
           
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
       cesf-mainline section.
       cesf-mainline-para.
      *    determine if we are receiving our own map
      *    (shouldn't we check if ws-map = 'BMSESOF' ??)
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

      *            PF2 attempts a signon with password change
                   when DFHPF2
                       if ESOFCFMI = low-values
                           move 'Must confirm to sign off' 
                               to ESOFMSGO
                       else
                           exec cics 
                               signoff
                           end-exec
                           if eibresp = dfhresp(normal)
                               move 'Signoff Successful'
                                   to ESOFMSGO
                           end-if
                       end-if
                       perform cics-send-cesf

      *            PF3 transfers to EMNU menu
                   WHEN DFHPF3
                       move 'EMNU' to ws-next-tran
 
      *            ENTER attempts a sign off
                   when DFHENTER
      *            *> Enter Key - see which menu item is selected
                       if ESOFCFMI = low-values
                           move 'Must confirm to sign off' 
                               to ESOFMSGO
                       else
                           exec cics 
                               signoff
                           end-exec
                           if eibresp = dfhresp(normal)
                               move 'Signoff Successful'
                                   to ESOFMSGO
                           end-if
                       end-if
                       perform cics-send-cesf
                       
      *            CLEAR key refreshes the screen
                   when DFHCLEAR
      *            *> Clear Key
                       move 'Display reset by clear key'
                           to ESOFMSGO
                       perform cics-send-cesf
                   when other
                       move 'Unexpected 3270 attention key'                       
                           to ESOFMSGO
                       perform cics-send-cesf
               end-evaluate
           else
               perform cics-send-cesf
           end-if
           exit
           .
      ***--------------------------------------------------------------*
       cics-receive-map section.
       cics-receive-map-para.
           evaluate ws-map
               when 'BMSESOF'
                   exec cics 
                       receive map(ws-map)
                       into(BMSESOFI)
                       nohandle
                   end-exec
                   if eibresp = dfhresp(normal)
                       move low-values to ws-next-tran
                   end-if
           end-evaluate
           exit
           .
      ***--------------------------------------------------------------*
       cics-send-cesf section.
       cics-send-cesf-para.
           move low-values to ESOFCFMO.

           move 'BMSESOF' to ws-map

           exec cics send map(ws-map) from(BMSESOFO) erase end-exec
           exit
           .
       end-program.