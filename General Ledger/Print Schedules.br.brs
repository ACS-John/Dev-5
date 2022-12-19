! formerly S:\acsGL\glSchPrt
! pr schedules
 
	autoLibrary
	on error goto Ertn
 
	dim dollar$*1,k$*3,by(13),bp(13),byt(13)
	dim gl2$*12,d2$*50,by2(13),bp2(13)
	dim sn$*78,ft$*78,gl$(80)*12,prtsch(0),d$*50,dol$*80,scheduleno(50)
	dim text$*45,resp$(45)*50
 
	fnTop(program$)
	pedat=fnactpd
	fnfscode
	fnpriorcd
	if fnGlAskFormatPriorCdPeriod=5 then goto Xit
	fnfscode
	fnpriorcd
	open #20: "Name=[Q]\GLmstr\Company.h[cno],Shr",i,i,r: read #20,using "form pos 296,N 2",rec=1: lmu : close #20:
	open #hAcGlSchs1:=1: "Name=[Q]\GLmstr\ACGLSCHS.h[cno],KFName=[Q]\GLmstr\schindex.h[cno],Shr",i,i,k ioerr DONE
	open #3: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr",i,i,k
	if fnProcess=1 then
		prtall=1
	else
		if ~fn_selectSchedules(mat prtsch) then goto Xit
	end if
	fnOpenPrn
	for prtSchItem=1 to udim(mat prtsch)
		if prtall=1 or prtsch(prtSchItem)<>0 then
			if prtall=1 then
				read #hAcGlSchs1,using L270: sn,sn$,ft$,dp,rs,cm eof DONE
				L270: form pos 1,n 3,2*c 78,3*n 1
			else
				k$=lpad$(str$(prtsch(prtSchItem)),3)
				read #hAcGlSchs1,using L270,key=k$: sn,sn$,ft$,dp,rs,cm nokey NextSchedule
			end if
			! pr 'SN=';sn : pause
			if dp=1 then dollar$="$" else dollar$=" "
			gosub PrintHeadings
			open #hSchedule=fnH: "Name=[Q]\GLmstr\schedule"&str$(sn)&".h[cno],KFName=[Q]\GLmstr\schedule_idx"&str$(sn)&".h[cno],Shr",i,outIn,k
			do
				read #hSchedule,using "form pos 1,c 12": gl$ eof EoSchedule
				! pr '"'&gl$&'"' : pause
				if gl$="  0     0  0" then goto NextGLinSchedule
				if j1><51 then goto L380
				gosub PrintPageFooter
				gosub PrintHeadings
				L380: read #3,using L450,key=gl$: d$,bb,cb,mat by,mat bp nokey NextGLinSchedule
				if cno<>99 then goto L450
				L400: read #3,using L410: gl2$,d2$,bb2,cb2,mat by2,mat bp2 eof L450
				L410: form pos 1,c 12,pos 13,c 50,pos 81,41*pd 6.2
				if gl2$=gl$(j) then goto L430 else goto L450
				L430: bb+=bb2 : cb+=cb2 : mat by=by+by2 : mat bp=bp+bp2
				goto L400
				L450: form pos 13,c 50,pos 81,41*pd 6.2
				if fnfscode=0 or (fnfscode=pedat and fnpriorcd=1) then goto L530 ! CURRENT OR PRIOR
				if fnfscode<0 or fnfscode>12 then let fnfscode=1
				if fnpriorcd=1 then cb=by(fnfscode) else cb=bp(fnfscode)
				if fnpriorcd=2 then goto L520
				if fnfscode>1 then bb=by(fnfscode-1) else bb=0
				goto L530
				L520: if fnfscode>1 then bb=bp(fnfscode-1) else bb=0
				L530: curmo=cb-bb
				if rs=1 then cb=-cb
				if rs=1 then curmo=-curmo
				if rs><1 then goto L580
				for rv=1 to 13 : by(rv)=-by(rv) : next rv
				L580: !
				if cm=1 then
					pr #255,using L660: d$,dollar$,curmo,dollar$,cb pageoflow PgOf
					j1=j1+1
					dollar$=" "
					L660: form pos 1,c 50,pos 51,c 1,pic(--,---,---.##),pos 67,c 1,pic(--,---,---.##)
				else if cm=2 then
					if lmu<2 or lmu>13 then goto L700
					for l=lmu to 2 step -1 : by(l)=by(l)-by(l-1) : next l
					L700: !
					if dp=1 then
					  dol$="form pos 1,C 32,14*PIC(---,---,--$.##)"
					else
					  dol$="form pos 1,C 32,14*PIC(---,---,---.##)"
					end if
					pr #255,using dol$: d$(1:30),mat by,cb pageoflow PgOf
					mat byt=byt+by
				else if cm=3 then
					if dp=1 then
					  dol$="form pos 1,C 42,13*PIC(---,---,--$.##)"
					else
					  dol$="form pos 1,C 42,13*PIC(---,---,---.##)"
					end if
					pr #255,using dol$: d$(1:40),mat by pageoflow PgOf
					mat byt=byt+by
				else
					pr #255,using L610: d$,dollar$,cb pageoflow PgOf
					L610: form pos 1,c 50,pos 67,c 1,pic(--,---,---.##)
					dollar$=" "
				end if
				gosub AccumulateTotals
				NextGLinSchedule: !
			loop
			EoSchedule: !
			j1=0
			gosub PrintTotals
			gosub PrintPageFooter
		end if
		NextSchedule: !
	next prtSchItem
	goto DONE
PrintTotals: ! r:
	if dp=1 then dollar$="$" else dollar$=" "
	if cm=0 then
		pr #255,using L610: "    Total",dollar$,ytdtot
		pr #255,using 'form pos 67,c 14': "=============="
	else if cm=1 then
		pr #255,using L920: "______________","______________"
		L920: form pos 51,c 14,pos 67,c 14
		pr #255,using L660: "    Total",dollar$,cmtot,dollar$,ytdtot
		pr #255,using L920: "==============","=============="
	else if cm=2 then
		dol$="form pos 1,C 32,14*'  ------------'"
		pr #255,using dol$: ""
		if dp=1 then
			dol$="form pos 1,C 32,14*PIC(---,---,--$.##)"
		else
			dol$="form pos 1,C 32,14*PIC(---,---,---.##)"
		end if
		pr #255,using dol$: "  Totals",mat byt,ytdtot pageoflow PgOf
		dol$="form pos 1,C 32,14*'  ============'"
		pr #255,using dol$: ""
	else if cm=3 then
		dol$="form pos 1,C 42,13*'  ------------'"
		pr #255,using dol$: ""
		if dp=1 then
			dol$="form pos 1,C 42,13*PIC(---,---,--$.##)"
		else
			dol$="form pos 1,C 42,13*PIC(---,---,---.##)"
		end if
		pr #255,using dol$: "  Totals",mat byt pageoflow PgOf
		dol$="form pos 1,C 42,13*'  ============'"
		pr #255,using dol$: ""
	else
		pr #255,using 'form pos 67,c 14': "______________"
	end if
	cmtot=0
	ytdtot=0
	mat byt=(0)
return ! /r
PrintPageFooter: ! r:
	fnPgLen(pglen)
	sk=pglen-krec(255): fl=len(rtrm$(ft$))
	pr #255,using L1190: rtrm$(ft$)
	L1190: form skip sk,pos tabnote,c fl,skip 1
	if eofcode<>1 then
		pr #255: newpage
	end if
return ! /r
PgOf: ! r:
	gosub PrintPageFooter
	gosub PrintHeadings
continue ! /r
PrintHeadings: ! r:
	pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs22 \b "&rtrm$("Schedule "&str$(sn))&" - "&trim$(sn$)&"}"
	pr #255: "\qc  {\f181 \fs16 \b "&rtrm$(fnpedat$)&"}"
	pr #255: "\ql "
	pr #255: ""
	if cm=1 then
		pr #255,using L1380: lpad$(rtrm$(fncch$),20),"Year To Date"
		L1380: form pos 45,c 20,pos 69,c 12,skip 2
	else if cm=2 then
		pr #255: "Description                           Period 1      Period 2      Period 3      Period 4      Period 5      Period 6      Period 7      Period 8      Period 9     Period 10     Period 11     Period 12     Period 13  Year To Date"
		pr #255: "______________________________    ____________  ____________  ____________  ____________  ____________  ____________  ____________  ____________  ____________  ____________  ____________  ____________  ____________  ____________"
	else if cm=3 then
		pr #255: "Description                                     Period 1      Period 2      Period 3      Period 4      Period 5      Period 6      Period 7      Period 8      Period 9     Period 10     Period 11     Period 12     Period 13"
		pr #255: "________________________________________    ____________  ____________  ____________  ____________  ____________  ____________  ____________  ____________  ____________  ____________  ____________  ____________  ____________"
	end if
return ! /r
DONE: ! r:
	fnfscode(pedat)
	fnpriorcd(1)
	fnClosePrn
goto Xit ! /r
AccumulateTotals: ! r:
	ytdtot=ytdtot+cb
	if cm=1 then
		cmtot+=curmo
	end if
return ! /r
Xit: fnXit
def fn_selectSchedules(mat prtsch)
	selectSchedulesReturn=0
	restore #hAcGlSchs1,key>="   ": nokey SelectSchedulesXit
	ln=1 : totallisted=0
	fnTos(sn$="GLschprt")
	fnLbl(1,15,"Select Schedules to Be Printed")
	do
		read #hAcGlSchs1,using L270: sn,sn$,ft$,dp,rs,cm eof L1720
		ln=ln+1
		text$=cnvrt$("pic(zzz)",sn)&"  "&sn$(1:40)
		fnChk(ln,1,text$,0)
		totallisted+=1
		scheduleno(totallisted)=val(text$(1:3))
		! if ln>21 and 1>1 then goto L1720 ! quit if more than two columns
		! if ln>21 then ln=1  : colpos+=52
	loop
	L1720: !
	if totallisted then
		fnCmdKey("&Next",1,1,0,"Allows you to enter transactions.")
	end if
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without printing.")
	ckey=fnAcs(mat resp$)
	if ckey<>5 then
		mat prtsch(totallisted)
		for j=1 to totallisted
			if resp$(j)='True' then
				prtsch(j)=scheduleno(j)
			end if
		next j
		selectSchedulesReturn=totallisted
	end if
	SelectSchedulesXit: !
	fn_selectSchedules=selectSchedulesReturn
fnend
include: ertn
 
