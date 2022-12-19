
! add a label to a queue that will be printed when fnLabel is called
def library fnAddLabel(mat alTextIn$; ___,hTmp,j)
	if ~setup then fn_setup
	if udim(alTextIn$)<>5 then pr 'fnAddLabel - You should send 5 array items to fnAddLabel.' : pause
	dim alText$(5)*120
	mat alText$=('')
	for j=1 to min(5,udim(alTextIn$))
		alText$(j)=alTextIn$(j)(1:min(len(alTextIn$(j)),120))
	next j
	write #hTmp=fn_openLabelFile,using 'form pos 1,5*C 120': mat alText$
	close #hTmp:
	gLabelCount+=1
fnend
def fn_openLabelFile(; ___,returnN)
	dim gLabelFileName$*128
	if gLabelFileName$='' then gLabelFileName$='[Temp]\Label_s[session].dat'
	open #returnN=fnH: 'Name='&gLabelFileName$&',RecL=600,Use',internal,outIn
	fn_openLabelFile=returnN
fnend

! pr label library, finds out how to format the labels and then
! prints them using PrintAce so bar codes pr right
def library fnLabel(mat linestyle$; ___,returnN)
	if ~setup then fn_setup
	on error goto Ertn

	dim labelText$(5)*120
	dim resp$(10)*80
	dim wabel$(10,3,5)*120
	! fn_testMatLab(mat wabel$)
	! wabel$(10,3,5) wabel$(x,y,z) wabel$(left/right,Up/Down,LabelLine)
	! labelText$(x)= each line of label text, 4 lines per label
	! linestyle$(x) indicates which hex code to use on that line of pr
	! (this linestyle feature is not yet written
	! if uprc$(linestyle$(5))='BAR' then labelPos1=01 : labelPos2=75 : labelPos3=150 else labelPos1=02 : labelPos2=30 : labelPos3=63
	! top_marg=2

	Screen1: ! r: top level main loop
		! r: AskLabelFormat
		fnTos : lc=0
		fnLbl(lc+=1,1,'('&str$(gLabelCount)&' labels queued)',15,1)
		lc+=1
		fnLbl(lc+=1,1,'Label Format:',15,1)
		dim opt$(3)*43
		opt$(1)='Avery 8160 (30/Page Ink Jet)'
		opt$(2)='Avery 5160 (30/Page Laser)'
		opt$(3)='Universal Data Processing (1-Up Dot-Matrix)'
		fnComboA('labellib1',lc,17,mat opt$,'Choose either dot-matrix or laser type labels',32)
		fnreg_read('label format',resp$(1),opt$(1))
		! resp$(1)=opt$(1)
		fnCmdSet(2)
		ckey=fnAcs(mat resp$)
		if ckey<>5 then
			fnreg_write('label format',resp$(1))
			if resp$(1)=opt$(3) then
				labelFormat=1 ! 1-Up Dot-Matrix
			else
				labelFormat=2 ! 3x10/Page
			end if
		end if
		! /r

		if ckey=5 then goto LabelXit
		if labelFormat=1 then
			gosub Label_1UpDotMatrix
		else if labelFormat=2 then
			if fn_askL3x10(labX,labY)=5 then
				goto Screen1
			end if
			! fn_AskL3x10margins
			! if ckey=5 then goto Screen1
			gosub Label_3x10
		end if
	goto LabelXit ! /r
	Label_1UpDotMatrix: ! r: 1-Up Dot-Matrix
		fnOpenPrn
		! fnwait('Printing: Please wait...',1)
		hLabelTemp=fn_openLabelFile
		do
			read #hLabelTemp,using 'form pos 1,5*C 120': mat labelText$ eof L1Done
			for z=1 to 5
				!   If UPRC$(LINESTYLE$(Z))='BAR' AND labelText$(Z)<>'' Then
				!   fnBARCODE(labelText$(Z),labelPos1) : pr #255: ''
				!   Goto 1400
				!  end if
				!   If LINESTYLE$(Z)<>'' Then FNSETLINESTYLE(LINESTYLE$(Z))
				pr #255,using 'form pos 2,c 70': labelText$(z)(1:70)
			next z
			pr #255: ''
		loop
		L1Done: !
	goto LabelFinis ! /r
	Label_3x10: ! r: 3x10/Page
		fn_readL3x10margins(top_marg,labelPos1,labelPos2,labelPos3)
		hLabelTemp=fn_openLabelFile
		L3x10nextPage: !
		mat wabel$=('') ! fn_testMatLab(mat wabel$)
		do
			read #hLabelTemp,using 'form pos 1,5*C 120': mat labelText$ eof L3x10Finis
			! wabel$(10,3,4) wabel$(x,y,z) wabel$(Up/Down,left/right,LabelLine)
			labY+=1
			if labY>3 then labY=1 : labX+=1
			if labX>10 or labX<1 then labX=1
			for j=1 to 5
				wabel$(labX,labY,j)=labelText$(j)
			next j
			if labY=>3 and labX=>10 then
				labY=0 : labX=0
				gosub L3x10print
				goto L3x10nextPage
			end if  ! labY=>3 and labX=>10
		loop
		L3x10Finis: !
		gosub L3x10print
	goto LabelFinis ! /r
		L3x10print: ! r:
			if uprc$(linestyle$(5))='BAR' then
				goto BarCodePrint
			end if
			fnOpenPrn
			! fnwait('Printing: Please wait...',1)
			if top_marg>0 then
				pr #255,using 'form pos 1,c 1,skip '&str$(top_marg): ''
			end if
			for x=1 to 10
				for z=1 to 5
					if printedabarcode=1 then
						pr #255: ''
						printedabarcode=0
					else
						! if linestyle$(z)<>'' then fnsetlinestyle(linestyle$(z))
						pr #255,using L1300: wabel$(x,1,z)(1:25),wabel$(x,2,z)(1:25),wabel$(x,3,z)(1:25)
						L1300:  form pos labelPos1,c 25,pos labelPos2,c 25,pos labelPos3,c 25
					end if
				next z
				if x<10 then pr #255: '' : pr #255: ''
				if x=10 then pr #255: newpage
			next x
		return ! /r
			BarCodePrint: ! r:
				addy=4
				fnpa_open
				fnpa_fontsize(12)
				lyne=4
				if top_marg>0 then
					! pr #20: 'Call Print.AddText(' ','&str$(labelPos3)&','&str$(top_marg*(addy)+ymargin)&')'
					fnpa_txt(' ',labelPos3,top_marg*(addy)+ymargin)
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
								fnpa_barcode(labelPos,x+.1,bc$)
							next j
						else
							L1560:  !

							! pr #20: 'Call Print.AddText("'&trim$(wabel$(x,1,z))&'",'&str$(labelPos1)&','&str$(lyne+=addy+ymargin)&')'
							! pr #20: 'Call Print.AddText("'&trim$(wabel$(x,2,z))&'",'&str$(labelPos2)&','&str$(lyne+ymargin)&')'
							! pr #20: 'Call Print.AddText("'&trim$(wabel$(x,3,z))&'",'&str$(labelPos3)&','&str$(lyne+ymargin)&')'

							fnpa_txt(trim$(wabel$(x,1,z)),labelPos1,lyne+=addy+ymargin)
							fnpa_txt(trim$(wabel$(x,2,z)),labelPos2,lyne+ymargin)
							fnpa_txt(trim$(wabel$(x,3,z)),labelPos3,lyne+ymargin)

							!      form pos labelPos1,C 25,pos labelPos2,C 25,pos labelPos3,C 25
						end if
					next z
					if x<10 then lyne+=addy*1.9
					if x=10 then fnpa_newpage
				next x
			return ! /r
	LabelFinis: ! r: this is the end of both the Label_1UpDotMatrix and Label_3x10 sub-routines
		returnN=1 ! marks succesful completion of label production
		if uprc$(linestyle$(5))='BAR' then
			close #1: ioerr ignore
			close #3: ioerr ignore
			fnpa_finis
			goto Xit
		end if
		fnClosePrn
		close #hLabelTemp: ioerr ignore
		hLabelTemp=0
	return  ! /r
	LabelXit: ! r:
		close #hLabelTemp,free: ioerr ignore
		fnfree(gLabelFileName$)
		gLabelCount=0
		hLabelTemp=0
		fnLabel=returnN
	! /r
fnend
	def fn_askL3x10(&labX,&labY; ___,returnN,ckey,j,myline,mypos,lStart)
		! re-uses locally dimmed mat resp$ but does not need to share
		AskLabelFormat2LstartToS: !
		fnTos
		fnFra(1,2,10,28,'Select First Label to Print','If you only have a partial sheet of labels, you can select the starting place of the first label')
		myline=1: mypos=1
		for j=1 to 30
			fnButton(myline,mypos,'  '&cnvrt$('pic(zz)',j)&'  ',j+20,'Starts with label '&str$(j),0,6,1)
			if j/3=int(j/3) then myline+=1
			mypos+=10
			if mypos>30 then mypos=1
		next j
		fnCmdKey('Set Margins',76)
		fnCmdKey('Label 1',75,1,0,'Just hit Enter to start with the first label')
		fnCmdKey('&Cancel',5,0,1)
		! fnCmdSet(1)
		returnN=fnAcs(mat resp$)
		if returnN=76 then
			fn_AskL3x10margins
			goto AskLabelFormat2LstartToS
		else if returnN=75 then
			returnN=21
		end if
		if returnN<>5 then
			lStart=returnN-20 ! user selection
			lStart-=1 ! because the other routine starts out incrementing by one.
			labX=int((lStart+2)/3)
			labY=lStart-((labX-1)*3)
			! pr 'labX='&str$(labX)
			! pr 'labY='&str$(labY) 
			! pause
			end if
		fn_askL3x10=returnN
	fnend
		def fn_AskL3x10margins
			AskL3x10margins: !
			fn_readL3x10margins(top_marg,labelPos1,labelPos2,labelPos3)
			fnTos
			fnLbl(1,1,'Top Margin (lines):',24,1)    	: fnTxt(1,26,3,3,1,'20',0,'number of blank lines before first label line')  	: resp$(1)=top_marg$
			fnLbl(2,1,'Left Column Position:',24,1)  	: fnTxt(2,26,3,3,1,'20',0,'character position to begin left most label')    	: resp$(2)=labelPos1$
			fnLbl(3,1,'Center Column Position:',24,1)	: fnTxt(3,26,3,3,1,'20',0,'character position (left/right) of center label')	: resp$(3)=labelPos2$
			fnLbl(4,1,'Right Column Position:',24,1) 	: fnTxt(4,26,3,3,1,'20',0,'position of the right column of labels')          	: resp$(4)=labelPos3$
			fnCmdSet(2)
			ckey=fnAcs(mat resp$)
			top_marg =val(top_marg$ =resp$(1)) conv AskL3x10margins
			labelPos1=val(labelPos1$=resp$(2)) conv AskL3x10margins
			labelPos2=val(labelPos2$=resp$(3)) conv AskL3x10margins
			labelPos3=val(labelPos3$=resp$(4)) conv AskL3x10margins
			if ckey<>5 then
				fnreg_write('top_marg' ,top_marg$ )
				fnreg_write('labelPos1',labelPos1$)
				fnreg_write('labelPos2',labelPos2$)
				fnreg_write('labelPos3',labelPos3$)
			end if
		fnend

	def fn_readL3x10margins(&top_marg,&labelPos1,&labelPos2,&labelPos3)
		top_marg =fnreg_read('top_marg' ,top_marg$ , '2',1)
		labelPos1=fnreg_read('labelPos1',labelPos1$, '2',1)
		labelPos2=fnreg_read('labelPos2',labelPos2$,'30',2)
		labelPos3=fnreg_read('labelPos3',labelPos3$,'63',2)
	fnend
	! def fn_testMatLab(mat wabel$)
	! 	if udim(wabel$,1)<>10 or udim(wabel$,2)<>3 or udim(wabel$,3)<>5 then
	! 		mat wabel$(10,3,5)
	! 	end if
	! fnend
Xit: fnXit
include: fn_setup
