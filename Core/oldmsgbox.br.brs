00010 ! Replace S:\Core\OldMsgBox.br
00020 ! ______________________________________________________________________
00030   def library fnoldmsgbox(mat response$,&cap$,mat msgline$,mtype)
00040     library 'S:\Core\Library': fnwin3b
00050 ! mtype=0 means splash    - returns no response                                 ! mostly for "please wait..." and "printing..."                                 ! (anywhere no response is required - no buttons are displyed either)
00060 ! mtype=1 means OK only   - returns no response !:
          ! mtype=2 means Yes or No - returns "Y" or "N" !:
          ! mtype=3 means Yes, No, Cancel - returns "Y" or "N" or "" !:
          ! mtype=4 means Yes, No, Back, Cancel - returns "Y", "N","","XIT"
00070 ! response$(1)= code you're looking for 2-5 are reserved for future use
00080     let msgbox_win=104
00090     close #msgbox_win: ioerr L100
00100 L100: let endrow=12
00110     for j=1 to udim(msgline$) !:
            let msgbox_width=max(msgbox_width,len(msgline$(j))+2) !:
          next j
00120     let msgbox_height=udim(msgline$)+2
00130     let mbr$=str$(int((24-msgbox_height)/2)+msgbox_height+1)
00140     let fnwin3b(msgbox_win,cap$,msgbox_height,msgbox_width,0,0,5,0)
00150     let mglinerow=2 !:
          for j=1 to udim(msgline$) !:
            let tmp_fld$=str$(mglinerow+j-1)&",2,Cc "&str$(msgbox_width-1)&",N" !:
            print #msgbox_win,fields tmp_fld$: msgline$(j) !:
          next j
00160     if mtype=1 then !:
            print fields mbr$&",38,Cc 4,B,1": "Ok" !:
            input fields mbr$&",09,C 1,AE,N": pause$
00170 L170: if mtype=2 then !:
            print fields mbr$&",35,Cc 4,B,21": "Yes" !:
            print fields mbr$&",40,Cc 4,B,22": "No" !:
            input fields str$(val(mbr$)-1)&",09,Cu 1,AE,N": response$(1)
00180     if mtype=2 and cmdkey=22 then let response$(1)="N"
00190     if mtype=2 and cmdkey=21 then let response$(1)="Y"
00200     if mtype=2 and response$(1)<>"Y" and response$(1)<>"N" then gosub PRNBELL : goto L170
00210 L210: if mtype=3 then !:
            print fields mbr$&",29,Cc 4,B,21": "Yes" !:
            print fields mbr$&",34,Cc 4,B,22": "No" !:
            print fields mbr$&",39,C 12,B,99": "Cancel (Esc)" !:
            input fields "24,79,Cu 1,AE,N": response$(1)
00220     if mtype=4 then !:
            print fields mbr$&",24,Cc 4,B,21": "Yes" !:
            print fields mbr$&",29,Cc 4,B,22": "No" !:
            print fields mbr$&",34,C  9,B,02": "Back (F2)" !:
            print fields mbr$&",44,C 12,B,99": "Cancel (Esc)" !:
            input fields "24,79,Cu 1,AE,N": response$(1)
00230     if mtype=41 then !:
            print fields mbr$&",36,C  8,B,01": "Ok  (F1)" !:
            input fields "24,79,Cu 1,AE,N": response$(1)
00240     if (mtype=3 or mtype=4) and cmdkey=22 then let response$(1)="N"
00250     if (mtype=3 or mtype=4) and cmdkey=21 then let response$(1)="Y"
00260     if (mtype=3 or mtype=4) and cmdkey=99 then let response$(1)=""
00270     if mtype=4 and cmdkey=2 then let response$(1)="BACK"
00280     if mtype=4 and (cmdkey=99 or cmdkey=5) then let response$(1)="CANCEL"
00290     if mtype=3 and response$(1)<>"Y" and response$(1)<>"N" and response$(1)<>"" then gosub PRNBELL : goto L210
00300     if mtype=4 and response$(1)<>"Y" and response$(1)<>"N" and response$(1)<>"BACK" and response$(1)<>"CANCEL" then gosub PRNBELL : goto L210
00310     close #104: ioerr L320
00320 L320: fnend 
00330 ! __________
00340 PRNBELL: ! print bell
00350   print fields "24,1,C 7,N": bell$
00360   return 
00370 ! ______________________________________________________________________
