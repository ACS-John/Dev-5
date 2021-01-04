! Replace S:\acsGL\Unpost
! Remove Transactions (for a date range)
! r: setup library and dims
autoLibrary
on error goto Ertn
 
dim k(10,8),p$*30,ta(2),cap$*128,t$*12
dim n(2),l$*12
! /r
fnTop(program$,cap$="Remove Entries")
MENU1: ! r:
	fnTos
	lc=0 : mylen=47 : mypos=mylen+2
	fnLbl(lc+=1,1,"Starting Date to Remove:",mylen,1)
	fnTxt(lc,mypos,0,0,0,'ccyymmdd')
	resp$(1)="" ! STR$(fndate_mmddyy_to_ccyymmdd(BEGDAT))
	fnLbl(lc+=1,1,"Ending Date to Remove:",mylen,1)
	fnTxt(lc,mypos,0,0,0,'ccyymmdd')
	resp$(2)="" ! STR$(fndate_mmddyy_to_ccyymmdd(ENDDAT))
	lc+=1
	fnChk(lc+=1,50,'Process History instead of Current Transactions',1)
	resp$(3)="False"
	lc+=1
	fnChk(lc+=1,50,'Remove Duplicates Only',1)
	resp$(4)='False'
	fnCmdSet(2)
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	begdat=val(resp$(1))
	enddat=val(resp$(2))
	if resp$(3)='True' then code$='H' else code$='C'
	if resp$(4)='True' then del_dupe_only=1 else del_dupe_only=0
	if enddat<begdat or (enddat=0 and begdat=0) then pr bell; : goto MENU1
! /r
! r: get ready to run
	fnStatus('date range: '&str$(begdat)&' - '&str$(enddat))
	if del_dupe_only then let fnStatus('only deleting duplicate entries')
	open #1: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr",internal,outIn,keyed
	if uprc$(code$)="H" then
		fnStatus('Processing history instead of current transactions')
		if del_dupe_only then
			fnIndex("[Q]\GLmstr\AcTrans.h[cno]","[Q]\GLmstr\tmp70.h[cno]","1,70")
		end if  ! del_dupe_only
		open #h_trans=fnH: "Name=[Q]\GLmstr\AcTrans.h[cno],KFName=[Q]\GLmstr\AcTrIdx.h[cno],Shr",internal,outIn,keyed  ! 3
		if del_dupe_only then
			open #h_trans_dupe=fnH: "Name=[Q]\GLmstr\AcTrans.h[cno],KFName=[Q]\GLmstr\tmp70.h[cno],Shr",internal,input,keyed
		end if  ! del_dupe_only
	else
		fnStatus('Processing current transactions only')
		if del_dupe_only then
			fnIndex("[Q]\GLmstr\GLTrans.h[cno]","[Q]\GLmstr\tmp70.h[cno]","1,70")
		end if  ! del_dupe_only
		open #h_trans=fnH: "Name=[Q]\GLmstr\GLTrans.h[cno],Shr",internal,outIn,relative  ! 2
		if del_dupe_only then
			open #h_trans_dupe=fnH: "Name=[Q]\GLmstr\GLTrans.h[cno],KFName=[Q]\GLmstr\tmp70.h[cno],Shr",internal,input,keyed
		end if  ! del_dupe_only
	end if
! /r
READ_H_TRANS: ! r: main loop
	read #h_trans,using 'Form POS 1,C 12,N 6,PD 6.2,N 2,N 2,C 12,C 30': t$,s,k,mat n,l$,p$ eof EO_H_TRANS
	reread #h_trans,using 'Form POS 1,C 70': hd_key_one$
	if fndate_mmddyy_to_ccyymmdd(s)<begdat or fndate_mmddyy_to_ccyymmdd(s)>enddat then goto READ_H_TRANS
! if val(t$(1:3))=0 and val(t$(4:9))=0 and val(t$(10:12))=0 then goto READ_H_TRANS
	if t$(3:3)=" " then t$(3:3)="0"
	if t$(12:12)=" " then t$(12:12)="0"
	read #1,using 'Form POS 81,2*PD 6.2',key=t$: bb,cb nokey DEL_H_TRANS ! delete any transactions without a matching master record.
	cb=cb-k
	if uprc$(code$)="H" then bb=bb-k
	rewrite #1,using 'Form POS 81,2*PD 6.2',key=t$: bb,cb
DEL_H_TRANS: !
! rec_to_delete=rec(h_trans)
! if trim$(l$)='4905' and t$='  6   507  1' then pause
	if ~del_dupe_only or fn_has_dupe(h_trans_dupe,rec(h_trans),'Form pos 1,C 70') then
		fnStatus('deleting transaction: '&hd_key_one$)
		delete #h_trans: ioerr ignore
	end if  ! ~del_dupe_only or fn_has_dupe
	goto READ_H_TRANS

EO_H_TRANS: ! /r
	fnStatus('Reassigning Transaction Addresses...') ! r:
	restore #1,key>="            ": eof ignore
	do
		read #1,using 'Form POS 333,2*PD 3': mat ta eof L470
		rewrite #1,using 'Form POS 333,2*PD 3': 0,0
	loop
L470: !
	lr2=lrec(2)
	if uprc$(code$)<>"H" then rewrite #h_trans,using 'Form POS 71,PD 3',rec=1: lr2
	for j=1 to lr2
		read #h_trans,using 'Form POS 1,C 12,POS 71,PD 3',rec=j: k$,nta noRec L580
		if k$="  0     0  0" then goto L580
		read #1,using 'Form POS 333,2*PD 3',key=k$: mat ta nokey L580
		if ta(1)=0 then ta(1)=j
		if ta(2)>0 then
			rewrite #h_trans,using 'Form POS 71,PD 3',rec=ta(2): j
		end if
		ta(2)=j
		rewrite #1,using 'Form POS 333,2*PD 3',key=k$: mat ta
		if uprc$(code$)<>"H" then rewrite #h_trans,using 'Form POS 71,PD 3',rec=j: 0
L580: !
	next j
! /r
	fnStatusPause
	goto Xit
Xit: fnXit
def fn_has_dupe(h_trans_dupe,hd_rec,hd_form$)
	! hd_key$ must be formatted properly and contain ENTIRE key not just part, h_trans_dupe must be keyed and it's record pointer will be changed
	! hd_form$ - form for key only
	dim hd_key_one$*128,hd_key_two$*128
	hd_return=0
	hd_key_two$=''
	! restore #h_trans_dupe:
	! release #h_trans:
	! read #h_trans_dupe,using hd_form$,rec=hd_rec,release: hd_key_one$ noRec HD_XIT
	read #h_trans_dupe,using hd_form$,key=hd_key_one$: hd_key_one$ noRec HD_XIT
	read #h_trans_dupe,using hd_form$: hd_key_two$ eof HD_EOF
	HD_EOF: !
	if hd_key_one$=hd_key_two$ then hd_return=1
	HD_XIT: !
	fn_has_dupe=hd_return
fnend  ! fn_has_dupe
include: ertn
