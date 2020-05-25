! Replace S:\Core\Label\fnLabel
! pr label library, finds out how to format the labels and then 
! prints them using PrintAce so bar codes pr right
def library fnlabel(mat linestyle$)
	library 'S:\Core\Library': fnerror
	library 'S:\Core\Library': fnwait
	library 'S:\Core\Library': fnopenprn,fncloseprn
	library 'S:\Core\Library': fnAcs,fnLbl,fnTos,fnButton,fnFra,fnTxt,fnCmdSet,fncomboa
	library 'S:\Core\Library': fnXit
	library 'S:\Core\Library': fnreg_read,fnreg_write
	library 'S:\Core\Library': fnpa_finis,fnpa_newpage,fnpa_open
	library 'S:\Core\Library': fnpa_fontsize
	library 'S:\Core\Library': fngethandle
	library 'S:\Core\Library': fnfree
	on error goto Ertn

	dim labeltext$(5)*120,wabel$(10,3,5)*120
	dim opt$(3)*43
	dim resp$(10)*80
	fn_test_mat_lab$
	! wabel$(10,3,5) wabel$(x,y,z) wabel$(left/right,Up/Down,LabelLine)
	! LabelText$(x)= each line of label text, 4 lines per label 
	! linestyle$(x) indicates which hex code to use on that line of pr 
	! (this linestyle feature is not yet written 
	! if uprc$(linestyle$(5))="BAR" then label_pos1=01 : label_pos2=75 : label_pos3=150 else label_pos1=02 : label_pos2=30 : label_pos3=63
	! top_marg=2

	Screen1: ! r: top level main loop
		gosub ASK_LABEL_FORMAT
		if ckey=5 then goto LABEL_XIT
		if label_format=1 then 
			gosub L1
		else if label_format=2 then 
			gosub ASK_LABEL_FORMAT_2_LSTART
			if ckey=5 then goto Screen1
			gosub ASK_L2_MARGINS
			if ckey=5 then goto Screen1
			gosub L2
		end if 
	goto LABEL_XIT ! /r

	ASK_LABEL_FORMAT: ! r:
		fnTos(sn$="labellib1")
		fnLbl(1,1,"Label Format:",15,1)
		opt$(1)="Avery 8160 (30/Page Ink Jet)" 
		opt$(2)="Avery 5160 (30/Page Laser)" 
		opt$(3)="Universal Data Processing (1-Up Dot-Matrix)" 
		fncomboa("labellib1."&wsid$,1,17,mat opt$,"Choose either dot-matrix or laser type labels",32) 
		resp$(1)=opt$(1)
		fnCmdSet(2)
		fnAcs(sn$,unused,mat resp$,ckey)
		if ckey=5 then goto Xit_ASK_LABEL
		if resp$(1)=opt$(3) then 
			label_format=1 
		else 
			label_format=2
		end if
		XIT_ASK_LABEL: !
	return  ! /r

	ASK_LABEL_FORMAT_2_LSTART: ! r:
		fnTos(sn$="labellib2")
		fnFra(1,2,10,28,"Select First Label to Print","If you only have a partial sheet of labels, you can select the starting place of the first label") 
		myline=1: mypos=1
		for j=1 to 30
			fnButton(myline,mypos,"  "&cnvrt$("pic(zz)",j)&"  ",j+20,"Starts with label "&str$(j),0,6,1)
			if j/3=int(j/3) then myline+=1
			mypos+=10
			if mypos>30 then mypos=1
		next j
		fnCmdSet(1)
		fnAcs(sn$,unused,mat resp$,ckey)
		if ckey=5 then goto Xit_ASK_LABEL_2
		lstart=ckey-21 ! because the other routine starts out adding one.
		labx=int((lstart+2)/3) 
		laby=lstart-((labx-1)*3)
		XIT_ASK_LABEL_2: ! 
	return ! /r
	ASK_L2_MARGINS: ! r:
		fnreg_read('top_marg',top_marg$) : top_marg=val(top_marg$) conv ignore
		fnreg_read('label_pos1',label_pos1$) : label_pos1=val(label_pos1$) conv ignore
		fnreg_read('label_pos2',label_pos2$) : label_pos2=val(label_pos2$) conv ignore
		fnreg_read('label_pos3',label_pos3$) : label_pos3=val(label_pos3$) conv ignore
		if top_marg=0 and label_pos1+label_pos2+label_pos3=0 then 
			if uprc$(linestyle$(5))="BAR" then 
				label_pos1=01 : label_pos2=75 : label_pos3=150
			else 
				label_pos1=02 : label_pos2=30 : label_pos3=63
			end if 
			top_marg=2
		end if  ! top_marg=0 and label_pos1+label_pos2+label_pos3=0
		sn$="labellib3" 
		fnTos(sn$)
		fnLbl(1,1,"Top Margin (lines):",24,1)
		fnTxt(1,26,3,3,1,'20',0,"Increase or decrease the top margin to move the pr up or down on the labels") 
		resp$(1)=str$(top_marg)
		fnLbl(2,1,"Left Column Position:",24,1)
		fnTxt(2,26,3,3,1,'20',0,"Increase or decrease the position to move the left label right or left") 
		resp$(2)=str$(label_pos1)
		fnLbl(3,1,"Center Column Position:",24,1)
		fnTxt(3,26,3,3,1,'20',0,"Increase or decrease the position to move the center label right or left") 
		resp$(3)=str$(label_pos2)
		fnLbl(4,1,"Right Column Position:",24,1)
		fnTxt(4,26,3,3,1,'20',0,"Increase or decrease the position to move the right label right or left") 
		resp$(4)=str$(label_pos3)
		fnCmdSet(2)
		fnAcs(sn$,unused,mat resp$,ckey)
		top_marg=val(resp$(1)) 
		label_pos1=val(resp$(2)) 
		label_pos2=val(resp$(3)) 
		label_pos3=val(resp$(4))
		if ckey<>5 then 
			fnreg_write('top_marg',str$(top_marg))
			fnreg_write('label_pos1',str$(label_pos1))
			fnreg_write('label_pos2',str$(label_pos2))
			fnreg_write('label_pos3',str$(label_pos3))
		end if 
	return ! /r
	LABEL_DONE: ! r:
		if uprc$(linestyle$(5))="BAR" then goto RELEASE_PRINT ! bar code to PrintAce
		fncloseprn
		close #hLabelTemp: 
		hLabelTemp=0
	return  ! /r
	L2: ! r:
		gosub OPEN_LABEL_WSID
		L2_NEXT: ! 
		mat wabel$=("") : fn_test_mat_lab$
		do
			read #hLabelTemp,using "Form POS 1,5*C 120": mat labeltext$ eof L2_DONE
			! wabel$(10,3,4) wabel$(x,y,z) wabel$(Up/Down,left/right,LabelLine)
			laby=laby+1
			if laby>3 then laby=1 : labx=labx+1
			if labx>10 or labx<1 then labx=1
			for j=1 to 5
				wabel$(labx,laby,j)=labeltext$(j)
			next j
			if laby=>3 and labx=>10 then 
				laby=0 : labx=0
				gosub L2_PRINT
				goto L2_NEXT
			end if  ! laby=>3 and labx=>10
		loop
		L2_DONE: ! 
		gosub L2_PRINT
	goto LABEL_DONE ! /r

	L2_PRINT: ! r:
		if uprc$(linestyle$(5))="BAR" then goto BARCODE_PRINT
		fnopenprn
		fnwait("Printing: Please wait...",1)
		if top_marg>0 then 
			pr #255,using "Form POS 1,C 1,SKIP "&str$(top_marg): ''
		end if
		for x=1 to 10
			for z=1 to 5
				for y=1 to 3
				!	If UPRC$(LINESTYLE$(Z))="BAR" AND wabel$(X,Y,Z)<>"" Then 
				!		pRINTEDABARCODE=1 
				!		fnBARCODE(wabel$(X,Y,Z),LABEL_POS(Y))
				!	end if
				!	If wabel$(X,Y,Z)<>"" Then Let FNBARCODE(wabel$(X,Y,Z),LABEL_POS(Y))
				next y

				if printedabarcode=1 then 
					pr #255: '' 
					printedabarcode=0 
					goto L1310
				end if
				! If LINESTYLE$(Z)<>"" Then Let FNSETLINESTYLE(LINESTYLE$(Z))
				pr #255,using L1300: wabel$(x,1,z)(1:25),wabel$(x,2,z)(1:25),wabel$(x,3,z)(1:25)
				L1300:  form pos label_pos1,c 25,pos label_pos2,c 25,pos label_pos3,c 25
				L1310: !
			next z
			if x<10 then pr #255: '' : pr #255: ''
			if x=10 then pr #255: newpage
		next x
	return ! /r



	BARCODE_PRINT: ! r:
		addy=4
		gosub VBOPENPRINT
		if top_marg>0 then 
			pr #20: 'Call Print.AddText(" ",'&str$(label_pos3)&','&str$(top_marg*(addy)+ymargin)&')'
		end if
		for x=1 to 10
			for z=1 to 5
				if z=5 then
					lyne+=addy
					for j=1 to 3
						v=val(wabel$(x,j,z)) conv L1560
						bc$=trim$(wabel$(x,j,z))
						if j=1 then label_pos =0
						if j=2 then label_pos =2.75
						if j=3 then label_pos =5.25
						if trim$(bc$)<>"" then pr #20: 'Call Print.DisplayBarCode('&str$(label_pos)&','&str$(x+.1)&',"'&bc$&'")'
					next j
				else
					L1560:  !
					pr #20: 'Call Print.AddText("'&trim$(wabel$(x,1,z))&'",'&str$(label_pos1)&','&str$(lyne+=addy+ymargin)&')' 
					pr #20: 'Call Print.AddText("'&trim$(wabel$(x,2,z))&'",'&str$(label_pos2)&','&str$(lyne+ymargin)&')' 
					pr #20: 'Call Print.AddText("'&trim$(wabel$(x,3,z))&'",'&str$(label_pos3)&','&str$(lyne+ymargin)&')'
					!      Form POS LABEL_POS1,C 25,POS LABEL_POS2,C 25,POS LABEL_POS3,C 25
				end if
			next z
			if x<10 then lyne+=addy*1.9
			if x=10 then let fnpa_newpage
		next x
	return ! /r

	L1: ! r:
		fnopenprn
		fnwait("Printing: Please wait...",1)
		gosub OPEN_LABEL_WSID
		do
			read #hLabelTemp,using "Form POS 1,5*C 120": mat labeltext$ eof L1_DONE
			for z=1 to 5
				!   If UPRC$(LINESTYLE$(Z))="BAR" AND LABELTEXT$(Z)<>"" Then 
				!   fnBARCODE(LABELTEXT$(Z),LABEL_POS1) : pr #255: '' 
				!   Goto 1400
				!  end if
				!   If LINESTYLE$(Z)<>"" Then Let FNSETLINESTYLE(LINESTYLE$(Z))
				pr #255,using 'form pos 2,c 70': labeltext$(z)(1:70)
			next z
			pr #255: ''
		loop
		L1_DONE: ! 
	goto LABEL_DONE ! /r

	OPEN_LABEL_WSID: ! r:
		open #hLabelTemp:=fngethandle: "Name="&env$('temp')&"\Label.dat,RecL=600,Use",internal,outIn ioerr XNOW
	return  ! /r

	LABEL_XIT: ! r:
		close #hLabelTemp,free: ioerr ignore
		fnfree(env$('temp')&'\Label.dat')
		hLabelTemp=0
		! fnFree(env$('temp')&"\Label.dat")
	goto XNOW ! /r
	XNOW: ! 
fnend 

VBOPENPRINT: ! r:
	fnpa_open ! if file(20)=-1 then
	fnpa_fontsize(12)
	lyne=4
return ! /r

RELEASE_PRINT: ! r:
	close #1: ioerr ignore
	close #3: ioerr ignore
	fnpa_finis
goto Xit
Xit: fnXit
def fn_test_mat_lab$
	if udim(wabel$,1)<>10 or udim(wabel$,2)<>3 or udim(wabel$,3)<>5 then 
		mat wabel$(10,3,5)
	end if  ! udim(wabel$,1)<>10 or udim(wabel$,2)<>3 or udim(wabel$,3)<>5
fnend  ! fn_test_mat_lab$
! r:  ertn - fails to label_xit instead of Xit
ERTN: fnerror(program$,err,line,act$,"label_xit")
	if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
	if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT ! if env$("ACSDeveloper")<>"" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
	pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr '' : pause : goto ERTN_EXEC_ACT
ERTN_EXEC_ACT: execute act$ : goto ERTN
! /r
