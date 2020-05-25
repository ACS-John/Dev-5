! Replace Test\ComboF
! -----------------------------------------------------------------------
	autoLibrary
	dim response$(5)*80
	cno=440 ! fncno(cno)
 
! open #1: 'Name=Test\Temp'&session$&'.dat,KFName=Temp'&session$&'.idx,Size=0,RecL=80,KPs=1,KLn=8,Replace',internal,outIn,keyed
! close #1:
! open #1: 'Name=Test\Temp'&session$&'.dat',internal,outIn,relative
! pr 'start building your big file at '&time$
! for j=1 to 50
!   write #1,using 'Form Pos 1,N 8,C 72': j,rpt$('X',72)
! next j
! close #1:
! pr 'done building file at '&time$
! execute 'Index Test\Temp'&session$&'.dat Test\Temp'&session$&'.idx 1 8,Replace'
! pr 'done indexing file at '&time$
! pr 'starting TOS call at '&time$
	fnTop("Test\ComboF","Test Combobox from File")
	fnTos(sn$="ComboF")
! fncmbrt2(1,36,1)
! fncombof("F",2,1,82,'Test\Temp'&session$&'.dat',1,8,9,72,'Test\Temp'&session$&'.idx',0,0)
! fncombof("F",3,1,82,'Test\Temp'&session$&'.dat',1,8,9,72,'Test\Temp'&session$&'.idx',1,0)
! fncombof("F2",4,1,82,'Test\Temp'&session$&'.dat',1,8,9,72,'Test\Temp'&session$&'.idx',2,0)
 
! fncombof("CityStZip",5,15,30,"[Q]\Data\CityStZip.dat",1,28,0,0,"[Q]\Data\CityStZip.idx") ! ,0,0, " ",fracustinfo,0)
! response$(1)='Billings MO 65610' ! pr 'past comboF call at '&time$
 
! fncombof('bank',6,10,0,"[Q]\CLmstr\BankMstr.h[cno]",1,2,3,30,"[Q]\CLmstr\BankIdx1.h[cno]",limit_to_list)
		f1Col1Len=21
		f1Col2=1+f1Col1Len+2 : f1Col2Len=36
		f1Col3=f1Col2+f1Col2Len+2 : f1Col3len=21
		f1Col4=f1Col3+f1Col3len+6 : f1Col4Len=38
	fncombof("fs-bal2",1,2,f1Col4Len,"[Q]\GLmstr\acglfnsc.h[cno]",1,5,6,30,"[Q]\GLmstr\agfsidx1.h[cno]",0,0, "Select the balance sheet reference number where this account should appear on the secondary balance sheet.",0)
	response$(1)=str$(10)
	fnCmdSet(2)
	fnAcs2(mat response$,ckey)
	pr mat response$
! fnpause
! end  ! fnXit
