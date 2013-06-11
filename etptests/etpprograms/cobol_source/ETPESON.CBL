       identification division.

       program-id. ETPESON.

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

      * ETPESON - CICS SIGNON
      * When started with JAAS security manager (-Djavax.security.manager
      * and other settings) will invoke JAAS sign-on (verify) or
      * change-password operation

       environment division.

       data division.

       working-storage section.
        COPY DFHAID. 

       01  ws-program-name                   pic x(8) value 'ETPESON'.

      ***--------------------------------------------------------------*
      *** Menu                                                         *
      ***--------------------------------------------------------------*        
       copy BMSESON. 

       01 filler.

       01  ws-cwa-ptr                        pointer.

       01  ws-end-msg                        pic x(30)
           value '     CICS session terminated.'.
       01  ws-info-msg                       pic x(30).
       01  ws-work.
           03  ws-next-tran                  pic x(4).

       01  ws-commarea.
           03  ws-map                        pic x(8).
           03  ws-uid                        pic x(8).
           03  ws-pwd                        pic x(8).
           03  ws-npw                        pic x(8).

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
           perform cesn-mainline
           
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
       cesn-mainline section.
       cesn-mainline-para.
      *    determine if we are receiving our own map
      *    (shouldn't we check if ws-map = 'BMSESON' ??)
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
                       if ESONUIDI = low-values
                           move 'Must supply a user ID' to ESONMSGO
                       else if ESONPWDI = low-values
              move 'Must supply existing password to change a password' 
                           to ESONMSGO 
                       else if ESONNPWI = low-values
                            move 'Must supply a new password' 
                                to ESONMSGO
                       else
                           exec cics 
                               signon userid(ESONUIDI) 
                               password(ESONPWDI)
                               newpassword(ESONNPWI) 
                           end-exec
                           if eibresp = dfhresp(normal)
                               move 'Password Changed'
                                   to ESONMSGO
                           end-if
                       end-if end-if end-if
                       perform cics-send-cesn

      *            PF3 transfers to EMNU menu
                   WHEN DFHPF3
                       move 'EMNU' to ws-next-tran
 
      *            ENTER attempts a sign on
                   when DFHENTER
      *            *> Enter Key - see which menu item is selected
                       if ESONUIDI = low-values
                           move 'Must supply a user ID' 
                               to ESONMSGO
                       else if ESONPWDI = low-values
                           move 'Must supply password' 
                               to ESONMSGO
                       else
                           exec cics 
                               signon userid(ESONUIDI) 
                               password(ESONPWDI)
                               newpassword(ESONNPWI) 
                           end-exec
                           if eibresp = dfhresp(normal)
                               move 'Signon Successful'
                                   to ESONMSGO
                           end-if
                       end-if end-if
                       perform cics-send-cesn
                       
      *            CLEAR key refreshes the screen
                   when DFHCLEAR
      *            *> Clear Key
                       move 'Display reset by clear key'
                           to ESONMSGO
                       perform cics-send-cesn
                   when other
                       move 'Unexpected 3270 attention key'                       
                           to ESONMSGO
                       perform cics-send-cesn
               end-evaluate
           else
               perform cics-send-cesn
           end-if
           exit
           .
      ***--------------------------------------------------------------*
       cics-receive-map section.
       cics-receive-map-para.
           evaluate ws-map
               when 'BMSESON'
                   exec cics 
                       receive map(ws-map)
                       into(BMSESONI)
                       nohandle
                   end-exec
                   if eibresp = dfhresp(normal)
                       move low-values to ws-next-tran
                   end-if
           end-evaluate
           exit
           .
      ***--------------------------------------------------------------*
       cics-send-cesn section.
       cics-send-cesn-para.
           move low-values to ESONUIDO.
           move low-values to ESONPWDO.
           move low-values to ESONNPWO.

           move 'BMSESON' to ws-map

           exec cics send map(ws-map) from(BMSESONO) erase end-exec
           exit
           .
       end-program.