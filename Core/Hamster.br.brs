! uw$         unique word -
! mat lbl$    array of field labels
! mat fln     array of field lengths
! hIn         open file handle
! mat p$      array of
def library fnHamster2b(uw$*128,mat lbl$,mat fln,hIn,mat p$; mat flTyp$,mat sln,mat mask$,mat startPos,mat incontrol$,mat mxl)
	autoLibrary
	on error goto Ertn
	fnHamster2b=fn_hamster(uw$,mat lbl$,mat fln,hIn,mat p$, mat flTyp$,mat sln,mat mask$,mat startPos,mat incontrol$,mat mxl)
fnend

def library fnHamster(uw$*128,mat lbl$,mat fln,hIn,mat p$; mat flTyp$,mat sln,mat mask,mat startPos,mat incontrol$,mat mxl, ___,maskCount,x)
	autoLibrary
	on error goto Ertn

	dim h1Mask$(0)
	maskCount=udim(mat mask)
	mat h1Mask$(maskCount)
	for x=1 to maskCount
		h1Mask$(x)=str$(mask(x))
	nex x

	fnHamster=fn_hamster(uw$,mat lbl$,mat fln,hIn,mat p$, mat flTyp$,mat sln,mat mask$,mat startPos,mat incontrol$,mat mxl)
fnend

def fn_hamster(uw$*128,mat lbl$,mat fln,hIn,mat p$; mat flTyp$,mat sln,mat mask$,mat startPos,mat incontrol$,mat mxl)
	! r: setup:  prepare arrays, etc
		dim keyorder(199) ! contains a 0 if not a key, else contains it's sequence in the order of fields used to make the key
		dim cmask$(199) ! Flexgrid Column Mask
		dim key$*512 ! dynamically built key
		dim resp$(256)*1024
		dim flxItem$(199)*2048
		mat flxItem$(199)
		dim flxhdr$(199)*80 ! flexgrid item and header
		mat flxhdr$(199)

		dim mask2N(199)
		mat mask2N(199)
		dim option$(199)*256
		mat option$(199)
		dim control$(60,26)*256
		mat control$(60,26)
		row_select=1 : ck_cancel=5 : ck_add=4 : ck_edit=3
		ck_delete=7 : right=1
		itemCount=udim(mat p$)
		mat hComboF(itemCount)

		if udim(mat incontrol$,1)<>0 then
			mat control$(udim(mat incontrol$,1),udim(mat incontrol$,2))
			mat control$=incontrol$
		end if
		
		dim startPos2(199)
		mat startPos2(199)
		if udim(mat startPos)<>itemCount then
			startPos2(1)=1
		else
			startPos2(1)=startPos(1)
		end if
		
		dim fltyp2$(199)*2
		mat fltyp2$(199)
		dim sln2(199)
		mat sln2(199)
		dim mask2$(199)
		mat mask2$(199)
		for j=1 to itemCount
			if udim(mat control$,1)=>j and lwrc$(control$(j,1))='combof' and control$(j,7)<>'' then ! it is a combof that has an index
				fltyp2$(j)='c'
				open #hComboF(j)=fnH: 'name='&control$(j,2)&',kfname='&control$(j,7)&',Shr',i,i,k
			else if udim(mat flTyp$)<>itemCount then
				fltyp2$(j)='g'
			else if j>udim(mat flTyp$) then
				fltyp2$(j)='g'
			else if flTyp$(j)='' then
				fltyp2$(j)='g'
			else
				fltyp2$(j)=lwrc$(flTyp$(j))
			end if
			
			if udim(mat sln)<>itemCount then
				sln2(j)=fln(j)
			else if sln(j)=0 then
				sln2(j)=fln(j)
			else
				sln2(j)=sln(j)
			end if
			
			if udim(mat mask$)<>itemCount then
				mask2$(j)=''
				mask2N(j)=0
			else
				mask2$(j)=mask$(j)
				mask2N(j)=val(mask$(j)) conv ignore
			end if
			if mask2N(j)=1 then fln(j)=8
			
			if j>1 then
				if udim(mat startPos)=itemCount then
					startPos2(j)=startPos(j)
				else if udim(mat startPos)<>itemCount and udim(mat startPos)<>0 then
					startPos2(j)=startPos2(j-1)+int(sln2(j-1))
				else if udim(mat startPos)<>itemCount and udim(mat startPos)<>0 then
					startPos2(j)=startPos2(j-1)+int(sln2(j-1))
				else if udim(mat startPos)=itemCount then
					startPos2(j)=startPos2(j-1)+int(sln2(j-1))
				else if udim(mat startPos)=0 then
					startPos2(j)=startPos2(j-1)+int(sln2(j-1))
				else if startPos(j)=0 then
					startPos2(j)=startPos2(j-1)+int(sln2(j-1))
				end if
				if udim(mat startPos)=>j and startPos(j)<>0 then
					startPos2(j)=startPos(j)
				end if
			end if
			
		next j
		mat mask2$(itemCount)
		mat mask2N(itemCount)
		mat sln2(itemCount)
		mat fltyp2$(itemCount)
		mat startPos2(itemCount)
		mat keyorder(itemCount)
	! /r
	! Gosub KEYORDER_BUILD
	! r: Build Flex Headers and Flex Mask
		mat flxhdr$(itemCount+1) : fhc=0 : flxhdr$(fhc+=1)='Rec'
		for j=2 to itemCount+1
			if mask2N(j-1)<20000 then flxhdr$(fhc+=1)=lbl$(j-1)
			controlX=j-1
			testmask=mask2N(controlX)
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
				! cmask$(fhc)=str$(testmask)
				cmask$(fhc)='glaccount' ! str$(testmask)
				pr 'mask2$(controlX)='&mask2$(controlX)
				pr 'hamster - building flex header as '&cmask$(fhc)&' for provided testMask of '&str$(testmask)&'.' : pause
			else
				cmask$(fhc)='80'
			end if
			
		next j
		mat flxhdr$(fhc) : mat flxItem$(fhc) : mat cmask$(fhc)
		! /r
	goto Menu1

! 	! KEYORDER_BUILD: ! r: unused
! 	! uses: hIn, mat startPos2
! 	! returns: mat keyorder
! 	! this section is not used currently
! 	! if later we want to add an option to force keys to be unique,
! 	! than I'll probably want to resurect and test this section
! 			j=0 : mat keyorder=(0) : bowman=0
! 			do while kps(hIn,j+=1)>0
! 				for j=1 to udim(mat startPos2)
! 					if startPos2=kps(hIn,j) then keyorder(j)=bowman+=1
! 				next j
! 			loop
! 	return ! /r
	Menu1: ! r:
		fnTos
		fnFlexInit1(uw$&'2b',1,1,20,108,mat flxhdr$,mat cmask$,row_select)
		for j1=1 to lrec(hIn)
			pRec=j1
			gosub ReadP ! Read #hIn,Using FRM$,Rec=J1: MAT P$ noRec (just past fnFlexAdd1)
			if pnorec<>1 then
				fic=0 : flxItem$(fic+=1)=str$(rec(hIn))
				for j2=2 to itemCount+1
					controlX=j2-1
					if mask2N(controlX)<20000 then
						dim hcfDesc$*128,hcfKey$*128
						hcfDesc$='' ! p$(controlX)
						if controlX<=udim(mat control$,1) and lwrc$(control$(controlX,1))='combof' and control$(controlX,7)<>'' then
							hcfKey$=rpad$(trim$(p$(controlX))(1:kln(hComboF(controlX))),kln(hComboF(controlX)))
							read #hComboF(controlX),using 'form pos '&control$(controlX,5)&',c '&control$(controlX,6),key=hcfKey$: hcfDesc$ nokey ignore
							hcfDesc$=rtrm$(hcfDesc$)
							if mask2$(controlX)='glaccount' then
								flxItem$(fic+=1)=fnRgl$(p$(controlX), 60,1)
							else 
								flxItem$(fic+=1)=p$(controlX)&' '&hcfDesc$
							end if
						else
							flxItem$(fic+=1)=p$(controlX)&' '&hcfDesc$
						end if
						!           if hcfDesc$<>'' then pr 'flxItem$('&str$(fic)&')="'&flxItem$(fic)&'" hcfDesc$="'&hcfDesc$&'"' : pause
					end if
							fnRgl$('', 0,0) ! close the left open gl desc file
				next j2
				fnFlexAdd1(mat flxItem$)
			end if
		next j1
		for hComboFitem=1 to hComboFcount
			if hComboF(hComboFitem) then
				close #hComboF(hComboFitem): ioerr ignore
				hComboF(hComboFitem)=0
			end if
		next hComboFitem
		fnLbl(21,20,' ') ! move command buttons down one line so search box ok
		fnCmdKey('Edi&t',ck_edit,1)
		fnCmdKey('&Add',ck_add)
		fnCmdKey('&Delete',ck_delete)
		fnCmdKey('E&xit',ck_cancel,0,1)
		ckey=fnAcs(mat resp$,ckey)
		pRec=val(resp$(1)) conv Menu1
		if pRec=0 and ckey=ck_edit then ckey=ck_add
		if ckey=ck_cancel then
			goto Xit
		else if ckey=ck_add then
			goto ToAdd
		else if ckey=ck_edit then
			goto ToEdit
		else if ckey=ck_delete then
			delete #hIn,rec=pRec:
		end if
	goto Menu1 ! /r
	ToEdit: ! r: ADD and EDIT routines
			gosub ReadP
	ToAdd: !
			if ckey=ck_add then mat p$=('')
			if itemCount>30 then
				j2=int(itemCount/2) : myflen=0
				for j=1 to j2
					myflen=max(myflen,fln(j))
				next j
			end if
	! Print MYFLEN : fnPAUSE ! XXX
			mylen=0
			for j=1 to itemCount
				mylen=max(mylen,len(lbl$(j)))
			next j
			dim p2$(0)*2048 ! used to hold mat P$ + 1 more response for Add Loop
			mat p2$(alana=udim(mat p$)+1) : mat p2$(1:udim(mat p$))=p$(1:udim(mat p$))
			fnTos
			mypos=mylen+3 : lc=ic=0 : col=1 : colpos=1
			for j=1 to itemCount ! r:
				if itemCount>30 and j>30 and col=1 then ! if itemCount>30 and j>(itemCount/2) and col=1 then ! form Col 2
					lc=0 : colpos=mypos+myflen+35 : col+=1
					mypos=colpos+mylen+2
				else if itemCount>60 and j>60 and col=2 then ! Col 3
					lc=0 : colpos=mypos+myflen+30 : col+=1
					mypos=colpos+mylen+2
				end if
				
				
				
				
				if mask2N(ic+1)=>20000 then ic+=1 : goto SkipLabelAndControl
				fnLbl(lc+=1,colpos,lbl$(ic+=1)&':',mylen,right)
				if mask2N(ic)>10000 then
					disable=1
					mask2N(ic)-=10000
				else
					disable=0
				end if
				if j<udim(mat mxl) then maxlen=mxl(j) else maxlen=0
				if j>udim(mat control$,1) or trim$(control$(j,1))='' or lwrc$(control$(j,1))='txt' then
					if fln(j)>40 and (maxlen=0 or maxlen>40) then
						maxlen=fln(j)
						fln(j)=40
					end if
					fnTxt(lc,mypos,fln(j),maxlen,0,str$(mask2N(ic)),disable) ! p$(j)
				else if lwrc$(control$(j,1))='comboa' then
					mat option$(999)
					L1160: !
					cj+=1
					if cj<udim(mat control$,2)-1 and trim$(control$(j,cj))<>'' then
						option$(cj)=control$(j,cj+1)
						goto L1160
					else
						mat option$(cj-1)
					end if
					fnComboA(uw$&'A'&str$(j),lc,mypos,mat option$) ! p$(j)
				else if lwrc$(control$(j,1))='combof' then
					if mask2$(j)='glaccount' then
						fnQgl(lc,mypos)	!  fnQgl(myline,mypos; container,x,forceGLsysIfPossible,qgllength)
						p2$(j)=fnRgl$(p2$(j))
					else 
						! fnComboF(sfn$*100       ,lyne,ps,  width                                   ,df$*200        ,psk               ,lnk                 ,psd                ,lnd                ;if$*200       ,limlis              ,unused_userOrReplace,ttt$*200,contain,tabcon,keyFormat$)
						fnComboF(uw$&'F'&str$(j),lc,mypos,val(control$(j,4))+val(control$(j,6))+3,control$(j,2),val(control$(j,3)),val(control$(j,4)),val(control$(j,5)),val(control$(j,6)),control$(j,7),val(control$(j,8)))
					end if
				end if
				! done adding control and label
				if disable=1 then mask2N(ic)+=10000
				SkipLabelAndControl: !
			next j ! /r
			fnLbl(lc+1,20,' ') ! move command buttons down one line so search box ok
			if ckey=ck_add then
				fnChk(lc+=1,mypos,'Add Loop',right)
				p2$(alana)='False'
			end if
			if addloop$='' then p2$(alana)='False' else p2$(alana)=addloop$
			fnCmdKey('&Save',1,1)
			fnCmdKey('&Cancel',ck_cancel,0,1)

			p2ckey=fnAcs(mat p2$)
			mat p$(1:udim(mat p$))=p2$(1:udim(mat p$))
			if p2ckey<>ck_cancel then fn_rewriteP(hIn,pRec,mat fltyp2$,mat control$,ckey)
			addloop$=p2$(alana)
			if lwrc$(addloop$)=lwrc$('True') then goto ToAdd else goto Menu1
	! /r

	ReadP: ! r:
		! Pnorec (returned value)= 0 = ok    = 1 = noRec error encountered
		! Peof (returned value)  = 0 = ok    = 1 = EOF   error encountered
		! pRec (sent value)= record number to read
		pnorec=0 : peof=0
		! Read 1st Item
		j=1
		dim tmp$*512
		if fltyp2$(j)='c' or fltyp2$(j)='cr' then
			tmp$='form pos '&str$(startPos2(j))&',c '&str$(sln2(j))
			read #hIn,using tmp$,rec=pRec,reserve: p$(j) noRec PNOREC eof PEOF
		else if fltyp2$(j)='g' then
			tmp$='form pos '&str$(startPos2(j))&',g '&str$(sln2(j))
			read #hIn,using tmp$,rec=pRec,reserve: p$(j) noRec PNOREC eof PEOF
		else if fltyp2$(j)='n' or fltyp2$(j)='pd' then
			tmp$='form pos '&str$(startPos2(j))&','&fltyp2$(j)&' '&str$(sln2(j))
			read #hIn,using tmp$,rec=pRec,reserve: xt noRec PNOREC eof PEOF
			p$(j)=str$(xt)
		else if fltyp2$(j)='pd' and ord(p$(j))=15 then
			p$(j)=''
		end if
		! Read 2nd to Last Item
		for j=2 to itemCount-1
			if fltyp2$(j)='c' or fltyp2$(j)='cr' then
				tmp$='form pos '&str$(startPos2(j))&',c '&str$(sln2(j))
				reread #hIn,using tmp$,reserve: p$(j) noRec PNOREC eof PEOF
			else if fltyp2$(j)='g' then
				tmp$='form pos '&str$(startPos2(j))&',g '&str$(sln2(j))
				reread #hIn,using tmp$,reserve: p$(j) noRec PNOREC eof PEOF
			else if fltyp2$(j)='n' or fltyp2$(j)='pd' then
				tmp$='form pos '&str$(startPos2(j))&','&fltyp2$(j)&' '&str$(sln2(j))
				reread #hIn,using tmp$,reserve: xt noRec PNOREC eof PEOF
				p$(j)=str$(xt)
			else if fltyp2$(j)='pd' and ord(p$(j))=15 then
				p$(j)=''
			end if
		next j
		! read Last Item
		j=itemCount
		if fltyp2$(j)='c' or fltyp2$(j)='cr' then
			tmp$='form pos '&str$(startPos2(j))&',c '&str$(sln2(j))
			reread #hIn,using tmp$,release: p$(j) noRec PNOREC eof PEOF
		else if fltyp2$(j)='g' then
			tmp$='form pos '&str$(startPos2(j))&',g '&str$(sln2(j))
			reread #hIn,using tmp$,release: p$(j) noRec PNOREC eof PEOF
		else if fltyp2$(j)='n' or fltyp2$(j)='pd' then
			tmp$='form pos '&str$(startPos2(j))&','&fltyp2$(j)&' '&str$(sln2(j))
			reread #hIn,using tmp$,release: xt noRec PNOREC eof PEOF
			p$(j)=str$(xt)
		else if fltyp2$(j)='pd' and ord(p$(j))=15 then
			p$(j)=''
		end if
		goto ReadP_XIT
		PNOREC: !
			pnorec=1
		goto ReadP_XIT
		PEOF: !
			peof=1
		goto ReadP_XIT
		ReadP_XIT: !
	return ! /r

	Xit: !
fnend
	def fn_rewriteP(hIn,&pRec,mat fltyp2$,mat control$,ckey; ___,tmp$*512,crflag,keyForm$*1024)
		! inherrits local: ck_add
	! RewriteP: ! r:
		! spos=1
		if ckey=ck_add then
			pRec=lrec(hIn)+1
			dim blank$(20)*256
			keyForm$=fn_keyForm$(mat blank$,key$,hIn)
			write #hIn,using keyForm$,reserve: mat blank$
			! pr 'write using KeyFormS,Reserve: Mat Blank$   - keyForm$='&keyForm$
			read #hIn,key=key$: nokey SpecialNoKey
		else
			keyForm$=fn_keyForm$(mat blank$,key$,hIn)
			reread #hIn,using keyForm$: mat blank$
			j=0 : key$=''
			do while kps(hIn,j+=1)>0
				key$&=blank$(j)
			loop
			read #hIn,key=key$: nokey SpecialNoKey
			fn_rightKeyWrongRecord(hIn,pRec)
		end if
		
		for j=1 to itemCount ! r:
			if j<=udim(mat control$,1) and lwrc$(control$(j,1))='combof' then
				p$(j)=p$(j)(1:val(control$(j,4)))
				! pause
				if mask$(j)='glaccount' then
					! pr 'ere i am jh p$(j)='''&p$(j)&'''' : pause
					p$(j)=fnAgl$(p$(j))
				end if
				
			end if
			crflag=0
			if fltyp2$(j)='cr' then
				crflag=1
				p$(j)=lpad$(trim$(p$(j)),sln2(j))
				fltyp2$(j)='c'
			end if
			if lwrc$(fltyp2$(j))<>'pd' then p$(j)=p$(j)(1:sln2(j))
			if fltyp2$(j)='c' or fltyp2$(j)='g' or fltyp2$(j)='cr' then
				tmp$='form pos '&str$(startPos2(j))&','&fltyp2$(j)&' '
				tmp$=tmp$&str$(sln2(j))
				rewrite #hIn,using tmp$,same,reserve: p$(j)
				! pr 'Rewr$ - '&TMP$&'   P$('&STR$(J)&')='&P$(J)
			end if
			if crflag=1 then fltyp2$(j)='cr' : crflag=0
			if fltyp2$(j)='n' or fltyp2$(j)='pd' then
				tmp$='form pos '&str$(startPos2(j))&','&fltyp2$(j)&' '
				tmp$=tmp$&str$(sln2(j)) : xt=val(p$(j))
				rewrite #hIn,using tmp$,same,reserve: xt
			end if
		next j ! /r
		
		release #hIn:
		! RewriteP_XIT: !
	! return ! /r
	
	fnend
		SpecialNoKey: ! r: supporting clause for fn_rewriteP
			! pr 'Special Nokey routine' ! XXX
			key$=''
			read #hIn,using keyForm$,rec=pRec: mat blank$
			for j=1 to udim(mat blank$)
				key$&=blank$(j)
			next j
		continue  ! not Return  ! not Retry ! /r

def fn_rightKeyWrongRecord(hIn,pRec)
	if rec(hIn)<>pRec then
		do
			read #hIn:
		loop until rec(hIn)=pRec
	end if
fnend
def fn_keyForm$*1024(mat kfBlank$,&key$,hIn; ___,return$*1024,j)
	return$='form ' : key$=''
	do while kps(hIn,j+=1)>0
		return$&='pos '&str$(kps(hIn,j))&',C '&str$(kln(hIn,j))&','
		kfBlank$(j)=rpt$(chr$(48),kln(hIn,j))
		key$&=kfBlank$(j)
	loop
	return$=return$(1:len(return$)-1) ! remove the trailing comma
	mat kfBlank$(j-1)
	! pr 'return$='&return$ ! XXX
	fn_keyForm$=return$
fnend

include: fn_open
include: ertn
