00010 ! Replace Test\ComboF
00020 ! -----------------------------------------------------------------------
00030   library 'S:\Core\Library': fncombof,fnAcs,fnTos,fnCmdSet,fncmbrt2,fntop,fnpause,fnxit,fncmbrt2,fncno
00040   dim response$(5)*80
00050   cno=440 ! fncno(cno)
00052 ! 
00060 ! open #1: 'Name=Test\Temp'&session$&'.dat,KFName=Temp'&session$&'.idx,Size=0,RecL=80,KPs=1,KLn=8,Replace',internal,outIn,keyed
00070 ! close #1:
00080 ! open #1: 'Name=Test\Temp'&session$&'.dat',internal,outIn,relative
00090 ! pr 'start building your big file at '&time$
00100 ! for j=1 to 50
00110 !   write #1,using 'Form Pos 1,N 8,C 72': j,rpt$('X',72)
00120 ! next j
00200 ! close #1:
00220 ! pr 'done building file at '&time$
00240 ! execute 'Index Test\Temp'&session$&'.dat Test\Temp'&session$&'.idx 1 8,Replace'
00260 ! pr 'done indexing file at '&time$
00280 ! pr 'starting TOS call at '&time$
00300   fntop("Test\ComboF","Test Combobox from File")
00320   fnTos(sn$="ComboF")
00340 ! fncmbrt2(1,36,1)
00360 ! fncombof("F",2,1,82,'Test\Temp'&session$&'.dat',1,8,9,72,'Test\Temp'&session$&'.idx',0,0)
00380 ! fncombof("F",3,1,82,'Test\Temp'&session$&'.dat',1,8,9,72,'Test\Temp'&session$&'.idx',1,0)
00400 ! fncombof("F2",4,1,82,'Test\Temp'&session$&'.dat',1,8,9,72,'Test\Temp'&session$&'.idx',2,0)
00410 ! 
00420 ! fncombof("CityStZip",5,15,30,"[Q]\Data\CityStZip.dat",1,28,0,0,"[Q]\Data\CityStZip.idx") ! ,0,0, " ",fracustinfo,0)
00440 ! response$(1)='Billings MO 65610' ! pr 'past comboF call at '&time$
50000 ! 
50020 ! fncombof('bank',6,10,0,"[Q]\CLmstr\BankMstr.h[cno]",1,2,3,30,"[Q]\CLmstr\BankIdx1.h[cno]",limit_to_list)
50022     f1Col1Len=21 
50024     f1Col2=1+f1Col1Len+2 : f1Col2Len=36
50026     f1Col3=f1Col2+f1Col2Len+2 : f1Col3len=21
50028     f1Col4=f1Col3+f1Col3len+6 : f1Col4Len=38
50030   fncombof("fs-bal2",1,2,f1Col4Len,"[Q]\GLmstr\acglfnsc.h[cno]",1,5,6,30,"[Q]\GLmstr\Fnscindx.h[cno]",0,0, "Select the balance sheet reference number where this account should appear on the secondary balance sheet.",0)
50040   response$(1)=str$(10)
60000   fnCmdSet(2)
60020   fnAcs(sn$,0,mat response$,ckey)
60040   pr mat response$
60060 ! fnpause
60080 ! end  ! fnxit
