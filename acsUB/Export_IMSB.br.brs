! Replace S:\acsUB\ubCass1
! -- Creates Address Certification File:
 
 
	autoLibrary
	on error goto Ertn
 
	dim z$,nam$*30,sta$*30,sta2$*30,csz$*30,opt1$(7),txt$*80,cap$*128
 
	fnTop(program$,cap$="Export File for IMSB")
	fncno(cno)
 
! r: a screen
! /r
	open #h_out:=fnH: "Name=SAVE:text documents (*.txt) |*.txt,RecL=112,EOL=CRLF,Replace",display,output ioerr Xit
F_OUT_FIXED: form pos 1,c 10,c 30,c 30,c 23,c 2,c 5,pos 112,c 1
!  pr file$(h_tmp);" ";lrec(h_tmp)
!  save_name$=os_filename$(file$(h_tmp))
!  close #h_tmp,free:
 
! open #h_out:=2: "Name="&dv$&"ubCass1.dat,RecL=112,EOL=None,Replace",external,output ! ioerr l170
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed
F_CUSTOMER: form pos 1,c 10,pos 41,3*c 30,pos 213,pd 4,pos 296,pd 4,pos 385,pd 3,pos 1854,pd 5.2
	open #h_adrbil:=3: "Name=[Q]\UBmstr\ubAdrBil.h[cno],KFName=[Q]\UBmstr\AdrIndex.h[cno],Shr",internal,outIn,keyed
F_ADRBIL: form pos 11,4*c 30
	pr #h_out,using F_OUT_FIXED: 'Account','Name','Street Address','City','State','Zip Code'
! r:  main loop
	do
		read #1,using F_CUSTOMER: z$,nam$,sta$,csz$,fb1,lbd,altcode eof Xit
		if altcode=2 then goto L350 ! alternate address coded as not being used at this time.
		read #h_adrbil,using F_ADRBIL,key=z$: nam$,sta$,sta2$,csz$ nokey L300
		if trim$(csz$)="" then csz$=sta2$ : sta2$=""
L300: !
		csz$=trim$(csz$)
		if csz$(len(csz$)-5:len(csz$)-5) =" " then goto L330
		if csz$(len(csz$)-5:len(csz$)-5) < "0" or csz$(len(csz$)-5:len(csz$)-5)>"9" then
			csz$=csz$(1:len(csz$)-5) &" "&csz$((len(csz$)-4):len(csz$))
		end if
L330: !
		fncsz(csz$,city$,state$,zip$)
		zip$=trim$(zip$)(1:5)
L350: !
		pr #h_out,using F_OUT_FIXED: z$,nam$,sta$,city$,state$,zip$,chr$(10)
	loop
! /r
Xit: fnXit
include: ertn
