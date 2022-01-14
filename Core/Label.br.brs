! add a label to a queue that will be printed when fnLabel is called
dim gLabelFileName$*128

def library fnAddLabel(mat in_labeltext$)
	if ~setup then fn_setup

	gLabelFileName$='[Temp]\Label_s[session].dat'

	! on error goto Ertn   (  failure should probably skip a label, not all  )
	dim labeltext$(5)*120

	if udim(in_labeltext$)<>5 then
		pr "fnAddLabel - You should send no more than 5 array items to fnAddLabel."
		pause
	end if
	for j=1 to min(5,udim(in_labeltext$))
		labeltext$(j)=in_labeltext$(j)(1:min(len(in_labeltext$(j)),120))
	next j
	open #tmp=fnH: "Name="&gLabelFileName$&",RecL=600,Use",internal,output
	write #tmp,using "form pos 1,5*C 120": mat labeltext$
	close #tmp:
	mat labeltext$=("")

fnend




! pr label library, finds out how to format the labels and then
! prints them using PrintAce so bar codes pr right
def library fnLabel(mat linestyle$)
	if ~setup then fn_setup
	on error goto Ertn

	dim labeltext$(5)*120,wabel$(10,3,5)*120
	dim opt$(3)*43
	dim resp$(10)*80
	fn_testMatLab$
	! wabel$(10,3,5) wabel$(x,y,z) wabel$(left/right,Up/Down,LabelLine)
	! LabelText$(x)= each line of label text, 4 lines per label
	! linestyle$(x) indicates which hex code to use on that line of pr
	! (this linestyle feature is not yet written
	! if uprc$(linestyle$(5))="BAR" then labelPos1=01 : labelPos2=75 : labelPos3=150 else labelPos1=02 : labelPos2=30 : labelPos3=63
	! top_marg=2

	Screen1: ! r: top level main loop
		gosub AskLabelFormat
		if ckey=5 then goto LabelXit
		if Labelformat=1 then
			gosub L1
		else if Labelformat=2 then
			gosub AskLabelFormat2Lstart
			if ckey=5 then goto Screen1
			gosub AskL2MARGINS
			if ckey=5 then goto Screen1
			gosub L2
		end if
	goto LabelXit ! /r

	AskLabelFormat: ! r:
		fnTos
		fnLbl(1,1,"Label Format:",15,1)
		opt$(1)="Avery 8160 (30/Page Ink Jet)"
		opt$(2)="Avery 5160 (30/Page Laser)"
		opt$(3)="Universal Data Processing (1-Up Dot-Matrix)"
		fncomboa("labellib1",1,17,mat opt$,"Choose either dot-matrix or laser type labels",32)
		fnreg_read('label format',resp$(1),opt$(1))
		! resp$(1)=opt$(1)
		fnCmdSet(2)
		ckey=fnAcs(mat resp$)
		if ckey<>5 then
			fnreg_write('label format',resp$(1))
			if resp$(1)=opt$(3) then
				Labelformat=1
			else
				Labelformat=2
			end if
		end if
	return  ! /r

	AskLabelFormat2Lstart: ! r:
		fnTos
		fnFra(1,2,10,28,"Select First Label to Print","If you only have a partial sheet of labels, you can select the starting place of the first label")
		myline=1: mypos=1
		for j=1 to 30
			fnButton(myline,mypos,"  "&cnvrt$("pic(zz)",j)&"  ",j+20,"Starts with label "&str$(j),0,6,1)
			if j/3=int(j/3) then myline+=1
			mypos+=10
			if mypos>30 then mypos=1
		next j
		fnCmdSet(1)
		ckey=fnAcs(mat resp$)
		if ckey<>5 then
			lstart=ckey-21 ! because the other routine starts out adding one.
			labx=int((lstart+2)/3)
			laby=lstart-((labx-1)*3)
		end if
	return ! /r
	AskL2MARGINS: ! r:
		top_marg =fnreg_read('top_marg',top_marg$)
		labelPos1=fnreg_read('labelPos1',labelPos1$)
		labelPos2=fnreg_read('labelPos2',labelPos2$)
		labelPos3=fnreg_read('labelPos3',labelPos3$)
		if top_marg=0 and labelPos1+labelPos2+labelPos3=0 then
			if uprc$(linestyle$(5))="BAR" then
				labelPos1=01 : labelPos2=75 : labelPos3=150
			else
				labelPos1=02 : labelPos2=30 : labelPos3=63
			end if
			top_marg=2
		end if
		fnTos
		fnLbl(1,1,"Top Margin (lines):",24,1)
		fnTxt(1,26,3,3,1,'20',0,"Increase or decrease the top margin to move the pr up or down on the labels")
		resp$(1)=str$(top_marg)
		fnLbl(2,1,"Left Column Position:",24,1)
		fnTxt(2,26,3,3,1,'20',0,"Increase or decrease the position to move the left label right or left")
		resp$(2)=str$(labelPos1)
		fnLbl(3,1,"Center Column Position:",24,1)
		fnTxt(3,26,3,3,1,'20',0,"Increase or decrease the position to move the center label right or left")
		resp$(3)=str$(labelPos2)
		fnLbl(4,1,"Right Column Position:",24,1)
		fnTxt(4,26,3,3,1,'20',0,"Increase or decrease the position to move the right label right or left")
		resp$(4)=str$(labelPos3)
		fnCmdSet(2)
		ckey=fnAcs(mat resp$)
		top_marg =val(resp$(1))
		labelPos1=val(resp$(2))
		labelPos2=val(resp$(3))
		labelPos3=val(resp$(4))
		if ckey<>5 then
			fnreg_write('top_marg' ,str$(top_marg ))
			fnreg_write('labelPos1',str$(labelPos1))
			fnreg_write('labelPos2',str$(labelPos2))
			fnreg_write('labelPos3',str$(labelPos3))
		end if
	return ! /r
	LabelDone: ! r:
		if uprc$(linestyle$(5))="BAR" then goto ReleasePrint ! bar code to PrintAce
		fncloseprn
		close #hLabelTemp:
		hLabelTemp=0
	return  ! /r
	L2: ! r:
		gosub OepnLabelFile
		L2Next: !
		mat wabel$=("") : fn_testMatLab$
		do
			read #hLabelTemp,using "form pos 1,5*C 120": mat labeltext$ eof L2Done
			! wabel$(10,3,4) wabel$(x,y,z) wabel$(Up/Down,left/right,LabelLine)
			laby=laby+1
			if laby>3 then laby=1 : labx=labx+1
			if labx>10 or labx<1 then labx=1
			for j=1 to 5
				wabel$(labx,laby,j)=labeltext$(j)
			next j
			if laby=>3 and labx=>10 then
				laby=0 : labx=0
				gosub L2Print
				goto L2Next
			end if  ! laby=>3 and labx=>10
		loop
		L2Done: !
		gosub L2Print
	goto LabelDone ! /r

	L2Print: ! r:
		if uprc$(linestyle$(5))="BAR" then goto BarCodePrint
		fnopenprn
		fnwait("Printing: Please wait...",1)
		if top_marg>0 then
			pr #255,using "form pos 1,c 1,skip "&str$(top_marg): ''
		end if
		for x=1 to 10
			for z=1 to 5
				for y=1 to 3
				!	if uprc$(linestyle$(z))="bar" and wabel$(x,y,z)<>"" then
				!		printedabarcode=1
				!		fnbarcode(wabel$(x,y,z),labelpos(y))
				!	end if
				!	If wabel$(X,Y,Z)<>"" Then Let FNBARCODE(wabel$(X,Y,Z),labelPos(Y))
				next y

				if printedabarcode=1 then
					pr #255: ''
					printedabarcode=0
					goto L1310
				end if
				! if linestyle$(z)<>"" then let fnsetlinestyle(linestyle$(z))
				pr #255,using L1300: wabel$(x,1,z)(1:25),wabel$(x,2,z)(1:25),wabel$(x,3,z)(1:25)
				L1300:  form pos labelPos1,c 25,pos labelPos2,c 25,pos labelPos3,c 25
				L1310: !
			next z
			if x<10 then pr #255: '' : pr #255: ''
			if x=10 then pr #255: newpage
		next x
	return ! /r


	BarCodePrint: ! r:
		addy=4
		gosub VbOpenPrint
		if top_marg>0 then
			pr #20: 'Call Print.AddText(" ",'&str$(labelPos3)&','&str$(top_marg*(addy)+ymargin)&')'
		end if
		for x=1 to 10
			for z=1 to 5
				if z=5 then
					lyne+=addy
					for j=1 to 3
						v=val(wabel$(x,j,z)) conv L1560
						bc$=trim$(wabel$(x,j,z))
						if j=1 then labelPos =0
						if j=2 then labelPos =2.75
						if j=3 then labelPos =5.25
						if trim$(bc$)<>"" then pr #20: 'Call Print.DisplayBarCode('&str$(labelPos)&','&str$(x+.1)&',"'&bc$&'")'
					next j
				else
					L1560:  !
					pr #20: 'Call Print.AddText("'&trim$(wabel$(x,1,z))&'",'&str$(labelPos1)&','&str$(lyne+=addy+ymargin)&')'
					pr #20: 'Call Print.AddText("'&trim$(wabel$(x,2,z))&'",'&str$(labelPos2)&','&str$(lyne+ymargin)&')'
					pr #20: 'Call Print.AddText("'&trim$(wabel$(x,3,z))&'",'&str$(labelPos3)&','&str$(lyne+ymargin)&')'
					!      form pos labelPos1,C 25,pos labelPos2,C 25,pos labelPos3,C 25
				end if
			next z
			if x<10 then lyne+=addy*1.9
			if x=10 then let fnpa_newpage
		next x
	return ! /r

	L1: ! r:
		fnopenprn
		fnwait("Printing: Please wait...",1)
		gosub OepnLabelFile
		do
			read #hLabelTemp,using "form pos 1,5*C 120": mat labeltext$ eof L1Done
			for z=1 to 5
				!   If UPRC$(LINESTYLE$(Z))="BAR" AND LABELTEXT$(Z)<>"" Then
				!   fnBARCODE(LABELTEXT$(Z),labelPos1) : pr #255: ''
				!   Goto 1400
				!  end if
				!   If LINESTYLE$(Z)<>"" Then Let FNSETLINESTYLE(LINESTYLE$(Z))
				pr #255,using 'form pos 2,c 70': labeltext$(z)(1:70)
			next z
			pr #255: ''
		loop
		L1Done: !
	goto LabelDone ! /r

	OepnLabelFile: ! r:
		open #hLabelTemp=fnH: "Name="&gLabelFileName$&",RecL=600,Use",internal,outIn ioerr Xnow
	return  ! /r

	LabelXit: ! r:
		close #hLabelTemp,free: ioerr ignore
		fnfree(gLabelFileName$)
		hLabelTemp=0
	goto Xnow ! /r
	Xnow: !
fnend

VbOpenPrint: ! r:
	fnpa_open ! if file(20)=-1 then
	fnpa_fontsize(12)
	lyne=4
return ! /r

ReleasePrint: ! r:
	close #1: ioerr ignore
	close #3: ioerr ignore
	fnpa_finis
goto Xit
Xit: fnXit
def fn_testMatLab$
	if udim(wabel$,1)<>10 or udim(wabel$,2)<>3 or udim(wabel$,3)<>5 then
		mat wabel$(10,3,5)
	end if
fnend
include: fn_setup
