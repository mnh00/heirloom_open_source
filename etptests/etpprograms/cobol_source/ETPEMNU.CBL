       identification division.

       program-id. ETPEMNU.

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

      
      * ETPEMNU - CICS send/receive map functionality
      * Displays a menu, allows PF key or 'X' to select other transactions

       environment division.

       data division.

       working-storage section.
        COPY DFHAID. 

       01  ws-program-name                   pic x(8) value 'ETPEMNU'.

      ***--------------------------------------------------------------*
      *** Menu                                                         *
      ***--------------------------------------------------------------*        
       copy BMSEMNU.

       01 filler.

       01  ws-cwa-ptr                        pointer.

       01  ws-end-msg                        pic x(30)
           value '     ETSQ session terminated.'.
       01  ws-info-msg                       pic x(30).
       01  ws-work.
           03  ws-next-tran                  pic x(4).
           03  ws-x                          pic s9(9)  comp.
           03  ws-z                          pic s9(9)  comp.
           03  ws-result-code                pic s9(9) comp.
           03  ws-numeric                    pic s9(9)  comp.
           03  ws-numeric-disp.
               05 ws-numeric-digit           pic 9 occurs 8 times.
           03  ws-numeric-z9                 pic z9.
           03  ws-attr-askip                 pic x value '0'.
           03  ws-attr-mdt                   pic x value 'A'.
           03  ws-numeric-zzzz9m             pic zzzz9-.

           03  ws-swap-flag                  pic s9(9) comp.
           03  ws-swap-area                  pic x(53).
           03  ws-sub1                       pic 9(2).
           03  ws-sub2                       pic 9(2).
           03  ws-perform-cnt                pic 9(2).

       01  ws-commarea.
           03  ws-map                        pic x(8).
           03  ws-wid                        pic 9(4) blank when zero.
           03  ws-cid                        pic 9(4) blank when zero.
           03  ws-did                        pic 9(2) blank when zero.
           03  ws-xid                        pic 9(2) blank when zero.
           03  ws-wct                        pic 9(2) blank when zero.
           03  ws-dbms                       pic x(1).
               88 ws-use-db-88              value 'D'.
               88 ws-use-vsam-files-88       value 'V'.
           03  ws-siz                        pic x(1).
               88 ws-load-large-88           value 'L'.
               88 ws-load-small-88           value 'S'.
           03  ws-rem                        pic x(1).
               88 ws-load-remove-88          value 'Y'.
           03  ws-lod                        pic x(1).
               88 ws-load-yes-88             value 'Y'.
           03  ws-dbg                        pic x(1).
               88 ws-load-debug-88           value 'Y'.

       01  result-code                       pic s9(5) comp.
       01  m-err                             pic x(80).

       linkage section.
       01  dfhcommarea.
           03                                pic x
               occurs 0 to 32700 times depending on eibcalen.


      ***--------------------------------------------------------------*
      *** ETSQ Stuff                                                   *
      ***--------------------------------------------------------------*
       procedure division.
       main-para.
           move low-values to ws-next-tran.
           perform emnu-mainline
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
       emnu-mainline section.
       emnu-mainline-para.
      *     call 'CBL_DEBUGBREAK'
           if eibcalen = length of ws-commarea
               move dfhcommarea to ws-commarea
               if eibaid not = DFHCLEAR  
      *            *> Clear Key
                   perform cics-receive-map
               end-if
      *         display eibaid upon syserr
               evaluate eibaid
                    
                   when DFHPF12
      *                *> PF 12 Key
                       exec cics syncpoint end-exec
                       move eibtrnid to ws-end-msg(1:4)
                       exec cics send text | added text
                           from(ws-end-msg)
                           erase
                       end-exec
                       exec cics return end-exec
                   when DFHPF1  
                       move 'ESON' to ws-next-tran
                   when DFHPF2
                       move 'ESOF' to ws-next-tran
                   when DFHPF3
                       move 'EPIC' to ws-next-tran
                   when DFHPF4
                       move 'EJNL' to ws-next-tran
                   when  DFHPF5
                       move 'ETSQ' to ws-next-tran
                   when DFHPF6
                       move 'ETDQ' to ws-next-tran
                   when DFHPF7
                       move 'ELPL' to ws-next-tran
                   when DFHPF8
                       move 'EDPL' to ws-next-tran
                   when DFHPF9
                       move 'ESQL' to ws-next-tran
                   when dfhenter
      *            *> Enter Key - see which menu item is selected
                   if eibresp = dfhresp(normal)
                       move low-values to ws-next-tran
                       if MNUESONI not = low-values
                           move 'ESON' to ws-next-tran
                       end-if
                       if MNUESOFI not = low-values
                           move 'ESOF' to ws-next-tran
                       end-if
                       if MNUEPICI not = low-values
                           move 'EPIC' to ws-next-tran
                       end-if
                       if MNUEJNLI not = low-values
                           move 'EJNL' to ws-next-tran
                       end-if
                       if MNUETSQI not = low-values
                           move 'ETSQ' to ws-next-tran
                       end-if
                       if MNUETDQI not = low-values
                           move 'ETDQ' to ws-next-tran
                       end-if
                       if MNUELPLI not = low-values
                           move 'ELPL' to ws-next-tran
                       end-if
                       if MNUEDPLI not = low-values
                           move 'EDPL' to ws-next-tran
                       end-if
                       if MNUESQLI not = low-values
                           move 'ESQL' to ws-next-tran
                       end-if
                       if MNUQUITI not = low-values
                           exec cics syncpoint end-exec
                           move eibtrnid to ws-end-msg(1:4)
                           exec cics send text
                               from(ws-end-msg)
                               erase
                           end-exec
                           exec cics return end-exec
                       end-if
                   end-if
                   when dfhclear
      *            *> Clear Key
                       move 'Display reset by clear key'
                         to MNUMSGO
                       perform cics-send-approprate
                   when other
                       move 'Unexpected 3270 attention key'                       
                         to MNUMSGO
                       perform cics-send-approprate
               end-evaluate
           else
               perform cics-send-emnu
           end-if
           exit
           .
      ***--------------------------------------------------------------*
       cics-receive-map section.
       cics-receive-map-para.
           evaluate ws-map
               when 'BMSEMNU'
                   exec cics receive map(ws-map)
                       into(BMSEMNUI)
                       nohandle
                   end-exec
                   if eibresp = dfhresp(normal)
                       move low-values to ws-next-tran
                       if MNUESONI not = low-values
                           move 'ESON' to ws-next-tran
                       end-if
                       if MNUESOFI not = low-values
                           move 'ESOF' to ws-next-tran
                       end-if
                       if MNUEPICI not = low-values
                           move 'EPIC' to ws-next-tran
                       end-if
                       if MNUEJNLI not = low-values
                           move 'EJNL' to ws-next-tran
                       end-if
                       if MNUETSQI not = low-values
                           move 'ETSQ' to ws-next-tran
                       end-if
                       if MNUETDQI not = low-values
                           move 'ETDQ' to ws-next-tran
                       end-if
                       if MNUELPLI not = low-values
                           move 'ELPL' to ws-next-tran
                       end-if
                       if MNUEDPLI not = low-values
                           move 'EDPL' to ws-next-tran
                       end-if
                       if MNUESQLI not = low-values
                           move 'ESQL' to ws-next-tran
                       end-if
                       if MNUQUITI not = low-values
                           move 'QUIT' to ws-next-tran
                       end-if
                   end-if
           end-evaluate
           exit
           .
      ***--------------------------------------------------------------*
       edit-to-number section.
       edit-to-number-para.
           move 0 to ws-numeric
           perform varying ws-z from 1 by 1
               until ws-z > length of ws-numeric-disp
               if ws-numeric-digit(ws-z) is numeric
                   compute ws-numeric
                      = (ws-numeric * 10)
                     + ws-numeric-digit(ws-z)
               end-if
           end-perform
           exit
           .
      ***--------------------------------------------------------------*
       cics-send-approprate section.
       cics-send-approprate-para.
           evaluate ws-map
               when 'BMSESON'
                   perform cics-send-emnu
               when 'BMSESOF'
                   perform cics-send-emnu
               when other
                   perform cics-send-emnu
           end-evaluate
           exit
           .
      ***--------------------------------------------------------------*
       cics-send-emnu section.
       cics-send-emnu-para.
           move MNUMSGO to MNUMSGO
           move low-values to MNUESOFO.
           move low-values to MNUESONO.
           move low-values to MNUEPICO.
           move low-values to MNUEJNLO.
           move low-values to MNUETSQO.
           move low-values to MNUETDQO.
           move low-values to MNUELPLO.
           move low-values to MNUEDPLO.
           move low-values to MNUESQLO.
           move low-values to MNUQUITO.

           move 'BMSEMNU' to ws-map

           exec cics send map(ws-map) from(BMSEMNUO) erase end-exec
           exit
           .
           