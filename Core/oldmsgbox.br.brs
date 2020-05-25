! Replace S:\Core\OldMsgBox.br
 
def library fnoldmsgbox(mat response$,&cap$,mat msgline$,mtype)
		library 'S:\Core\Library': fnwin3b
! mtype=0 means splash    - returns no response                                 ! mostly for "please wait..." and "printing..."                                 ! (anywhere no response is required - no buttons are displyed either)
! mtype=1 means OK only   - returns no response : _
		! mtype=2 means Yes or No - returns "Y" or "N" : _
		! mtype=3 means Yes, No, Cancel - returns "Y" or "N" or "" : _
		! mtype=4 means Yes, No, Back, Cancel - returns "Y", "N","","Xit"
! response$(1)= code you're looking for 2-5 are reserved for future use
		msgbox_win=104
		close #msgbox_win: ioerr L100
L100: endrow=12
		for j=1 to udim(msgline$) : _
			msgbox_width=max(msgbox_width,len(msgline$(j))+2) : _
		next j
		msgbox_height=udim(msgline$)+2
		mbr$=str$(int((24-msgbox_height)/2)+msgbox_height+1)
		fnwin3b(msgbox_win,cap$,msgbox_height,msgbox_width,0,0,5,0)
		mglinerow=2 : _
		for j=1 to udim(msgline$) : _
			tmp_fld$=str$(mglinerow+j-1)&",2,Cc "&str$(msgbox_width-1)&",N" : _
			pr #msgbox_win,fields tmp_fld$: msgline$(j) : _
		next j
		if mtype=1 then : _
			pr f mbr$&",38,Cc 4,B,1": "Ok" : _
			input fields mbr$&",09,C 1,AE,N": pause$
L170: if mtype=2 then : _
			pr f mbr$&",35,Cc 4,B,21": "Yes" : _
			pr f mbr$&",40,Cc 4,B,22": "No" : _
			input fields str$(val(mbr$)-1)&",09,Cu 1,AE,N": response$(1)
		if mtype=2 and cmdkey=22 then response$(1)="N"
		if mtype=2 and cmdkey=21 then response$(1)="Y"
		if mtype=2 and response$(1)<>"Y" and response$(1)<>"N" then gosub PRNBELL : goto L170
L210: if mtype=3 then : _
			pr f mbr$&",29,Cc 4,B,21": "Yes" : _
			pr f mbr$&",34,Cc 4,B,22": "No" : _
			pr f mbr$&",39,C 12,B,99": "Cancel (Esc)" : _
			input fields "24,79,Cu 1,AE,N": response$(1)
		if mtype=4 then : _
			pr f mbr$&",24,Cc 4,B,21": "Yes" : _
			pr f mbr$&",29,Cc 4,B,22": "No" : _
			pr f mbr$&",34,C  9,B,02": "Back (F2)" : _
			pr f mbr$&",44,C 12,B,99": "Cancel (Esc)" : _
			input fields "24,79,Cu 1,AE,N": response$(1)
		if mtype=41 then : _
			pr f mbr$&",36,C  8,B,01": "Ok  (F1)" : _
			input fields "24,79,Cu 1,AE,N": response$(1)
		if (mtype=3 or mtype=4) and cmdkey=22 then response$(1)="N"
		if (mtype=3 or mtype=4) and cmdkey=21 then response$(1)="Y"
		if (mtype=3 or mtype=4) and cmdkey=99 then response$(1)=""
		if mtype=4 and cmdkey=2 then response$(1)="BACK"
		if mtype=4 and (cmdkey=99 or cmdkey=5) then response$(1)="CANCEL"
		if mtype=3 and response$(1)<>"Y" and response$(1)<>"N" and response$(1)<>"" then gosub PRNBELL : goto L210
		if mtype=4 and response$(1)<>"Y" and response$(1)<>"N" and response$(1)<>"BACK" and response$(1)<>"CANCEL" then gosub PRNBELL : goto L210
		close #104: ioerr L320
L320: fnend
! __________
PRNBELL: ! pr bell
	pr f "24,1,C 7,N": bell$
return
 
