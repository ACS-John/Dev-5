! uw$         unique word -
! mat lbl$    array of field labels
! mat fln     array of field lengths
! fin         open file handle
! mat p$      array of
def library fnHamster(uw$*20,mat lbl$,mat fln,fin,mat p$; mat flTyp$,mat sln,mat mask,mat startPos,mat incontrol$,mat mxl)
	! r: setup
	library 'S:\Core\Library': fnerror,fnTos,fnflexinit1,fnCmdKey,fnAcs2,fnflexadd1,fnLbl,fnTxt,fncomboa,fncombof,fnpause,fnChk,fngethandle
	on error goto ERTN

	dim sln2(199)
	dim fltyp2$(199)*2
	dim mask2(199),startPos2(199)
	dim option$(199)*256
	dim control$(60,26)*256
	dim p2$(100)*1024 ! used to hold mat P$ + 1 more response for Add Loop
	dim keyorder(199) ! contains a 0 if not a key, else contains it's sequence in the order of fields used to make the key
	dim cmask$(199) ! Flexgrid Column Mask
	dim flxItem$(199)*2048,flxhdr$(199)*80 ! flexgrid item and header
	dim key$*80,blank$(20)*80 ! dynamically built key
	dim resp$(256)*1024
	! /r
	! r: prepare arrays
		mat flxItem$(199) : mat flxhdr$(199) : mat sln2(199) : mat fltyp2$(199)
		mat mask2(199) : mat startPos2(199) : mat option$(199) : mat control$(60,26)
		row_select=1 : opt_cancel=5 : opt_add=4 : opt_edit=3
		opt_delete=7 : right=1
		itemCount=udim(p$)
		mat hComboF(itemCount)
		!
		if udim(incontrol$,1)<>0 then
			mat control$(udim(incontrol$,1),udim(incontrol$,2))
			mat control$=incontrol$
		end if
		if udim(startPos)<>itemCount then
			startPos2(1)=1
		else
			startPos2(1)=startPos(1)
		end if
		for j=1 to itemCount
			if udim(mat control$,1)=>j and lwrc$(control$(j,1))='combof' and control$(j,7)<>'' then ! it is a combof that has an index
				fltyp2$(j)="c"
				open #hComboF(j):=fngethandle: 'name='&control$(j,2)&',kfname='&control$(j,7)&',Shr',internal,input,keyed
			else if udim(flTyp$)<>itemCount then
				fltyp2$(j)="g"
			else if j>udim(flTyp$) then
				fltyp2$(j)="g"
			else if flTyp$(j)="" then
				fltyp2$(j)="g"
			else
				fltyp2$(j)=lwrc$(flTyp$(j))
			end if
			if udim(sln)<>itemCount then
				sln2(j)=fln(j)
			else if sln(j)=0 then
				sln2(j)=fln(j)
			else
				sln2(j)=sln(j)
			end if
			if udim(mask)<>itemCount then
				mask2(j)=0
			else
				mask2(j)=mask(j)
			end if
			if mask2(j)=1 then fln(j)=8
			if j=1 then goto SKIP_startPos
			if udim(mat startPos)=itemCount then
				startPos2(j)=startPos(j)
			else if udim(mat startPos)<>itemCount and udim(mat startPos)<>0 then
				startPos2(j)=startPos2(j-1)+int(sln2(j-1))
			else if udim(mat startPos)<>itemCount and udim(mat startPos)<>0 then
				startPos2(j)=startPos2(j-1)+int(sln2(j-1))
			else if udim(mat startPos)=itemCount then
				startPos2(j)=startPos2(j-1)+int(sln2(j-1))
			else if udim(startPos)=0 then
				startPos2(j)=startPos2(j-1)+int(sln2(j-1))
			else if startPos(j)=0 then
				startPos2(j)=startPos2(j-1)+int(sln2(j-1))
			end if
			if udim(mat startPos)=>j and startPos(j)<>0 then
				startPos2(j)=startPos(j)
			end if
			SKIP_startPos: !
		next j
		mat mask2(itemCount) : mat sln2(itemCount) : mat fltyp2$(itemCount) : mat startPos2(itemCount) : mat keyorder(itemCount)
	! /r
	! Gosub KEYORDER_BUILD
	! r: Build Flex Headers and Flex Mask
		mat flxhdr$(itemCount+1) : fhc=0 : flxhdr$(fhc+=1)='Rec'
		for j=2 to itemCount+1
			if mask2(j-1)<20000 then flxhdr$(fhc+=1)=lbl$(j-1)
			controlX=j-1
			testmask=mask2(controlX)
			if testmask=>1000 and testmask<2000 then testmask-=1000
			if controlX<=udim(mat control$,1) and lwrc$(control$(controlX,1))='combof' and control$(controlX,7)<>'' then
			else if testmask=>1 and testmask<=29 then
				cmask$(fhc)=str$(testmask)
			else if testmask=>30 and testmask<=31 then
				cmask$(fhc)=str$(testmask)
			else if testmask=32 then
				cmask$(fhc)=str$(10)
			else if testmask=>33 and testmask<=39 then
				cmask$(fhc)=str$(testmask)
			else if testmask=>40 and testmask<=49 then
				cmask$(fhc)=str$(testmask)
			else if testmask=>50 and testmask<=53 then
				cmask$(fhc)=str$(testmask)
			else
				cmask$(fhc)='80'
			end if
			!
		next j
		mat flxhdr$(fhc) : mat flxItem$(fhc) : mat cmask$(fhc)
		! /r
	! /r
	! /r
	goto MENU1


	! KEYORDER_BUILD: ! r: unused
	! uses: FIN, mat startPos2
	! returns: mat keyorder
	! this section is not used currently
	! if later we want to add an option to force keys to be unique,
	! than I'll probably want to resurect and test this section
			j=0 : mat keyorder=(0) : bowman=0
			do while kps(fin,j+=1)>0
				for j=1 to udim(startPos2)
					if startPos2=kps(fin,j) then keyorder(j)=bowman+=1
				next j
			loop
	return ! /r
	MENU1: ! r:
		fnTos
		fnflexinit1(uw$&"2b",1,1,20,108,mat flxhdr$,mat cmask$,row_select)
		for j1=1 to lrec(fin)
			prec=j1
			gosub READ_P ! Read #FIN,Using FRM$,Rec=J1: MAT P$ noRec (just past fnflexadd1)
			if pnorec<>1 then
				fic=0 : flxItem$(fic+=1)=str$(rec(fin))
				for j2=2 to itemCount+1
					controlX=j2-1
					if mask2(controlX)<20000 then
						dim hcfDesc$*128,hcfKey$*128
						hcfDesc$='' ! p$(controlX)
						if controlX<=udim(mat control$,1) and lwrc$(control$(controlX,1))='combof' and control$(controlX,7)<>'' then
							hcfKey$=rpad$(trim$(p$(controlX))(1:kln(hComboF(controlX))),kln(hComboF(controlX)))
							read #hComboF(controlX),using 'form pos '&control$(controlX,5)&',c '&control$(controlX,6),key=hcfKey$: hcfDesc$ nokey ignore
							hcfDesc$=rtrm$(hcfDesc$)
						end if
						flxItem$(fic+=1)=p$(controlX)&' '&hcfDesc$
						!           if hcfDesc$<>'' then pr 'flxItem$('&str$(fic)&')="'&flxItem$(fic)&'" hcfDesc$="'&hcfDesc$&'"' : pause
					end if
				next j2
				fnflexadd1(mat flxItem$)
			end if
		next j1
		for hComboFitem=1 to hComboFcount
			if hComboF(hComboFitem) then
				close #hComboF(hComboFitem): ioerr ignore
				hComboF(hComboFitem)=0
			end if
		next hComboFitem
		fnLbl(21,20," ") ! move command buttons down one line so search box ok
		fnCmdKey("Edi&t",opt_edit,1)
		fnCmdKey("&Add",opt_add)
		fnCmdKey("&Delete",opt_delete)
		fnCmdKey("E&xit",opt_cancel,0,1)
		fnAcs2(mat resp$,menu1_opt)
		prec=val(resp$(1)) conv MENU1
		if prec=0 and menu1_opt=opt_edit then let menu1_opt=opt_add
		if menu1_opt=opt_cancel then
			goto XIT
		else if menu1_opt=opt_add then
			goto TO_ADD
		else if menu1_opt=opt_edit then
			goto TO_EDIT
		else if menu1_opt=opt_delete then
			delete #fin,rec=prec:
		end if
	goto MENU1 ! /r
	TO_EDIT: ! r: ADD and EDIT routines
			gosub READ_P
	TO_ADD: !
			if menu1_opt=opt_add then mat p$=("")
			if itemCount>30 then
				j2=int(itemCount/2) : myflen=0
				for j=1 to j2
					myflen=max(myflen,fln(j))
				next j
			end if
	! \Print MYFLEN : fnPAUSE ! XXX
			mylen=0
			for j=1 to itemCount
				mylen=max(mylen,len(lbl$(j)))
			next j
			mat p2$(alana=udim(p$)+1) : mat p2$(1:udim(p$))=p$(1:udim(p$))
			fnTos
			mypos=mylen+3 : lc=ic=0 : col=1 : colpos=1
			for j=1 to itemCount
				if itemCount>30 and j>(itemCount/2) and col=1 then
					lc=0 : colpos=mypos+myflen+4 : col+=1
					mypos=colpos+mylen+2
				end if
				if mask2(ic+1)=>20000 then ic+=1 : goto SKIP_LABEL_AND_CONTROL
				fnLbl(lc+=1,colpos,lbl$(ic+=1)&":",mylen,right)
				if mask2(ic)>10000 then
					disable=1
					mask2(ic)-=10000
				else
					disable=0
				end if
				if j<udim(mxl) then maxlen=mxl(j) else maxlen=0
				if j>udim(control$,1) or trim$(control$(j,1))="" or lwrc$(control$(j,1))="txt" then
					if fln(j)>40 and (maxlen=0 or maxlen>40) then
						maxlen=fln(j)
						fln(j)=40
					end if
					fnTxt(lc,mypos,fln(j),maxlen,0,str$(mask2(ic)),disable) ! p$(j)
				else if lwrc$(control$(j,1))="comboa" then
					mat option$(999)
					L1160: !
					cj+=1
					if cj<udim(control$,2)-1 and trim$(control$(j,cj))<>"" then
						option$(cj)=control$(j,cj+1)
						goto L1160
					else
						mat option$(cj-1)
					end if
					fnComboa(uw$&"A"&str$(j),lc,mypos,mat option$) ! p$(j)
				else if lwrc$(control$(j,1))="combof" then
					fnCombof(uw$&"F"&str$(j),lc,mypos,val(control$(j,4))+val(control$(j,6))+3,control$(j,2),val(control$(j,3)),val(control$(j,4)),val(control$(j,5)),val(control$(j,6)),control$(j,7),val(control$(j,8)))
				end if
				! done adding control and label
				if disable=1 then mask2(ic)+=10000
				SKIP_LABEL_AND_CONTROL: !
			next j
			fnLbl(lc+1,20," ") ! move command buttons down one line so search box ok
			if menu1_opt=opt_add then
				fnChk(lc+=1,mypos,'Add Loop',right)
				p2$(alana)='False'
			end if
			if addloop$='' then p2$(alana)='False' else p2$(alana)=addloop$
			fnCmdKey("&Save",1,1)
			fnCmdKey("&Cancel",opt_cancel,0,1)
			!
			fnAcs2(mat p2$,ck)
			mat p$(1:udim(p$))=p2$(1:udim(p$))
			if ck<>opt_cancel then gosub REWR_P
			addloop$=p2$(alana)
			if lwrc$(addloop$)=lwrc$('True') then goto TO_ADD else goto MENU1
	! /r

	READ_P: ! r:
		! Pnorec (returned value)= 0 = ok    = 1 = noRec error encountered
		! Peof (returned value)  = 0 = ok    = 1 = EOF   error encountered
		! PRec (sent value)= record number to read
		pnorec=0 : peof=0
		! Read 1st Item
		j=1
		dim tmp$*512
		if fltyp2$(j)="c" or fltyp2$(j)="cr" then
			tmp$="Form Pos "&str$(startPos2(j))&",c "&str$(sln2(j))
			read #fin,using tmp$,rec=prec,reserve: p$(j) noRec PNOREC eof PEOF
		else if fltyp2$(j)="g" then
			tmp$="Form Pos "&str$(startPos2(j))&",g "&str$(sln2(j))
			read #fin,using tmp$,rec=prec,reserve: p$(j) noRec PNOREC eof PEOF
		else if fltyp2$(j)="n" or fltyp2$(j)="pd" then
			tmp$="Form Pos "&str$(startPos2(j))&","&fltyp2$(j)&" "&str$(sln2(j))
			read #fin,using tmp$,rec=prec,reserve: t noRec PNOREC eof PEOF
			p$(j)=str$(t)
		else if fltyp2$(j)="pd" and ord(p$(j))=15 then
			p$(j)=""
		end if
		! Read 2nd to Last Item
		for j=2 to itemCount-1
			if fltyp2$(j)="c" or fltyp2$(j)="cr" then
				tmp$="Form Pos "&str$(startPos2(j))&",c "&str$(sln2(j))
				reread #fin,using tmp$,reserve: p$(j) noRec PNOREC eof PEOF
			else if fltyp2$(j)="g" then
				tmp$="Form Pos "&str$(startPos2(j))&",g "&str$(sln2(j))
				reread #fin,using tmp$,reserve: p$(j) noRec PNOREC eof PEOF
			else if fltyp2$(j)="n" or fltyp2$(j)="pd" then
				tmp$="Form Pos "&str$(startPos2(j))&","&fltyp2$(j)&" "&str$(sln2(j))
				reread #fin,using tmp$,reserve: t noRec PNOREC eof PEOF
				p$(j)=str$(t)
			else if fltyp2$(j)="pd" and ord(p$(j))=15 then
				p$(j)=""
			end if
		next j
		! read Last Item
		j=itemCount
		if fltyp2$(j)="c" or fltyp2$(j)="cr" then
			tmp$="Form Pos "&str$(startPos2(j))&",c "&str$(sln2(j))
			reread #fin,using tmp$,release: p$(j) noRec PNOREC eof PEOF
		else if fltyp2$(j)="g" then
			tmp$="Form Pos "&str$(startPos2(j))&",g "&str$(sln2(j))
			reread #fin,using tmp$,release: p$(j) noRec PNOREC eof PEOF
		else if fltyp2$(j)="n" or fltyp2$(j)="pd" then
			tmp$="Form Pos "&str$(startPos2(j))&","&fltyp2$(j)&" "&str$(sln2(j))
			reread #fin,using tmp$,release: t noRec PNOREC eof PEOF
			p$(j)=str$(t)
		else if fltyp2$(j)="pd" and ord(p$(j))=15 then
			p$(j)=""
		end if
		goto READ_P_XIT
		PNOREC: !
			pnorec=1
		goto READ_P_XIT
		PEOF: !
			peof=1
		goto READ_P_XIT
		READ_P_XIT: !
	return ! /r
	RightKeyWrongRecord: ! r:
		do
			read #fin:
		loop until rec(fin)=prec
	return ! /r

	REWR_P: ! r:
		! spos=1
		if menu1_opt=opt_add then
			prec=lrec(fin)+1
			keyForm$=fn_setKeyForm$(mat blank$,key$,fin)
			write #fin,using keyForm$,reserve: mat blank$
			! pr 'write using KeyFormS,Reserve: Mat Blank$   - keyForm$='&keyForm$
			read #fin,key=key$: nokey SPECIAL_NOKEY
		else
			keyForm$=fn_setKeyForm$(mat blank$,key$,fin)
			reread #fin,using keyForm$: mat blank$
			j=0 : key$=''
			do while kps(fin,j+=1)>0
				key$=key$&blank$(j)
			loop
			read #fin,key=key$: nokey SPECIAL_NOKEY
			if rec(fin)<>prec then
				gosub RightKeyWrongRecord
			end if
		end if
		for j=1 to itemCount
			if j<=udim(control$,1) and lwrc$(control$(j,1))="combof" then
				p$(j)=p$(j)(1:val(control$(j,4)))
			end if
			crflag=0
			if fltyp2$(j)="cr" then
				p$(j)=lpad$(trim$(p$(j)),sln2(j))
				fltyp2$(j)="c"
				crflag=1
			end if
			if lwrc$(fltyp2$(j))<>"pd" then p$(j)=p$(j)(1:sln2(j))
			if fltyp2$(j)="c" or fltyp2$(j)="g" or fltyp2$(j)="cr" then
				tmp$="Form Pos "&str$(startPos2(j))&","&fltyp2$(j)&" "
				tmp$=tmp$&str$(sln2(j))
				rewrite #fin,using tmp$,same,reserve: p$(j)
				! pr 'Rewr$ - '&TMP$&"   P$("&STR$(J)&")="&P$(J)
			end if
			if crflag=1 then fltyp2$(j)="cr" : crflag=0
			if fltyp2$(j)="n" or fltyp2$(j)="pd" then
				tmp$="Form Pos "&str$(startPos2(j))&","&fltyp2$(j)&" "
				tmp$=tmp$&str$(sln2(j)) : t=val(p$(j))
				rewrite #fin,using tmp$,same,reserve: t
			end if
		next j
		release #fin:
		! REWR_P_XIT: !
	return ! /r
	SPECIAL_NOKEY: ! r:
		! pr 'Special Nokey routine' ! XXX
		key$=""
		dim keyForm$*1024
		read #fin,using keyForm$,rec=prec: mat blank$
		for j=1 to udim(blank$) : key$=key$&blank$(j) : next j
	continue  ! not Return  ! not Retry ! /r
	XIT: !
fnend

def fn_setKeyForm$*1024(mat blank$,&key$,fin; ___,return$*1024)
	return$='Form ' : key$='' : j=0
	do while kps(fin,j+=1)>0
		return$(inf:inf)='Pos '&str$(kps(fin,j))&','
		return$(inf:inf)='C '&str$(kln(fin,j))&','
		blank$(j)=rpt$(chr$(48),kln(fin,j))
		key$(inf:inf)=blank$(j)
	loop
	return$=return$(1:len(return$)-1) ! remove the trailing comma
	mat blank$(j-1)
	! pr 'return$='&return$ ! XXX
	fn_setKeyForm$=return$
fnend
include: ertn
