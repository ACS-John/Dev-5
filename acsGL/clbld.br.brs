! Replace S:\acsGL\CLBld
! Create Checkbook System Files
 
	autoLibrary
	fnTop(program$,cap$="Create Checkbook System Files")
	on error goto Ertn
 
	dim cnam$*40,dat$*20,de$*50,vn$(4)*30 ,ta(2),cap$*128,io2$(2),wrd2$(2)*38
	dim resp$(4)*50,option$(2)*25,contact$*30,email$*50,myact$*20
 
	fncno(cno,cnam$)
 
	dv$="A"
MENU1: !
	fnTos(sn$="ClBld") : _
	mylen=50: mypos=mylen+3 : right=1
	fnLbl(1,1,"Insert Blank Formatted Diskette In Selected Drive:",mylen,right)
	fnTxt(1,mypos,1,0,right,"",0,"The information needs to be placed on a diskette.  If you do not have a diskette drive, use your C: drive and transfer the information to a CD.",0 ) : _
	resp$(1)= dv$
	option$(1)="Build G/L Master File" : _
	option$(2)="Build Payee File"
	fncomboa("TypeOfFile",3,25,mat option$,"You must indicate the type of entry you will be entering.",25)
	resp$(2)=str$(sel)
	fnCmdKey("&Next",1,1,0,"Allows you to enter transactions.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without transferring files.")
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	dv$=resp$(1)
	if trim$(resp$(2))="Build G/L Master File" then ti1=1 else ti1=2
	dv$=dv$&":"
	on ti1 goto L330,END1
 
L330: open #1: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr",internal,input,keyed
	open #2: "Name="&dv$&"GLmstr.h[cno],SIZE=0,RecL=62,Replace",internal,output
L350: read #1,using L360: gl$,de$ eof END1
L360: form pos 1,c 12,c 50
	write #2,using L360: gl$,de$
	goto L350
 
END1: close #1: ioerr L410
L410: close #2: ioerr L420
L420: open #2: "Name="&dv$&"PAYMSTR.h[cno],SIZE=0,RecL=276,Replace",internal,output
	open #payeegl:=fngethandle: "Name=[Q]\CLmstr\PayeeGLBreakdown.h[cno],Version=1,KFName=[Q]\CLmstr\Payeeglbkdidx.h[cno],Use,RecL=56,KPs=1,KLn=8,Shr",internal,outIn,keyed
	open #1: "Name=[Q]\GLmstr\paymstr.h[cno],KFName=[Q]\GLmstr\payidx1.h[cno],Shr",internal,input,keyed  ! Ioerr 580
READ_GL1099: !
	read #1,using 'Form Pos 1,C 8,4*c 30,x 5,n 2,c 11,x 6,c 12,c 30,c 50,c 12,c 20': vn$,mat vn$,typ,ss$,ph$,contact$,email$,fax$,myact$ eof EOF_GL1099
	write #2,using 'Form Pos 1,C 8,4*c 30,x 5,n 2,c 11,x 6,c 12,c 30,c 50,c 12,c 20': vn$,mat vn$,typ,ss$,ph$,contact$,email$,fax$,myact$
	goto READ_GL1099
 
EOF_GL1099: !
	close #1:
	open #1: "Name=[Q]\GLmstr\PRmstr.h[cno],KFName=[Q]\GLmstr\PRIndex.h[cno],Shr",internal,input,keyed ioerr L410
	vn$(3)=""
L540: read #1,using 'Form POS 1,N 4,3*C 25,C 11,PD 5.2': eno,vn$(1),vn$(2),vn$(4),ss$,ytdp eof L580
	write #2,using 'Form POS 1,G 8,4*C 30,PD 5.2,N 2,C 11': eno,mat vn$,ytdp,0,ss$
	goto L540
 
L580: close #1:
	close #2:
	execute "Copy [Q]\GLmstr\PayeeGLBreakdown.h[cno] a:"
	goto Xit
 
Xit: fnXit
 
include: Ertn
 
