! Replace S:\acsUB\ubCass1
! -- Creates Address Certification File:
 
 
	autoLibrary
	on error goto Ertn
 
	dim z$,nam$*30,sta$*30,sta2$*30,csz$*30,opt1$(7),txt$*80,cap$*128
 
	fnTop("S:\acsUB\ubCass1",cap$="Create Certification File")
	fncno(cno)
 
! r: a screen
! fnTos(sn$:="ubCass1")
! respc=0
! fnLbl(1,1,"Destination Path for Cass file:",33,1)
! opt1$(1)="A:\"
! opt1$(2)="B:\"
! opt1$(3)="C:\"
! opt1$(4)="E:\"
! opt1$(5)="F:\"
! opt1$(6)="G:\"
! opt1$(7)="H:\"
! fncomboa("AB",1,35,mat opt1$)
! resp$(respc+=1)=opt1$(1)
! fnCmdSet(2)
! L170: fnAcs(mat resp$,ckey)
! if ckey=5 then goto Xit
! dv$=resp$(1)
! /r
	open #h_out:=fngethandle: "Name=SAVE:ubCass1.dat,RecL=112,EOL=None,Replace",external,output ioerr Xit
!  pr file$(h_tmp);" ";lrec(h_tmp)
!  save_name$=os_filename$(file$(h_tmp))
!  close #h_tmp,free:
 
! open #h_out:=2: "Name="&dv$&"ubCass1.dat,RecL=112,EOL=None,Replace",external,output ! ioerr l170
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed
	open #3: "Name=[Q]\UBmstr\ubAdrBil.h[cno],KFName=[Q]\UBmstr\AdrIndex.h[cno],Shr",internal,outIn,keyed
! r:  main loop
	do
		read #1,using L250: z$,nam$,sta$,csz$,fb1,lbd,altcode eof Xit
L250: form pos 1,c 10,pos 41,3*c 30,pos 213,pd 4,pos 296,pd 4,pos 385,pd 3,pos 1854,pd 5.2
		if altcode=2 then goto L350 ! alternate address coded as not being used at this time.
		read #3,using L280,key=z$: nam$,sta$,sta2$,csz$ nokey L300
L280: form pos 11,4*c 30
		if trim$(csz$)="" then csz$=sta2$ : sta2$=""
L300: csz$=trim$(csz$)
		if csz$(len(csz$)-5:len(csz$)-5) =" " then goto L330
		if csz$(len(csz$)-5:len(csz$)-5) < "0" or csz$(len(csz$)-5:len(csz$)-5)>"9" then csz$=csz$(1:len(csz$)-5) &" "&csz$((len(csz$)-4):len(csz$))
L330: fncsz(csz$,city$,state$,zip$)
		zip$=trim$(zip$)(1:5)
L350: write #h_out,using F_OUT_FIXED: z$,nam$,sta$,city$,state$,zip$,chr$(10)
F_OUT_FIXED: form pos 1,c 10,c 30,c 30,c 23,c 2,c 5,pos 112,c 1
	loop
! /r
Xit: fnXit
include: Ertn
