00010 ! Replace R:\acsCL\Conversion\APmstr-Cnv
00020 ! Pull stuff from old Accounts Payable into new Checkbook company
00030 ! ______________________________________________________________________
00040   library 'R:\Core\Library': fntop,fnxit, fnchain,fnerror,fncno,fntos,fncmdset,fnlbl,fntxt,fnacs,fnindex_it,fncopy
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim vn$*8,nam$*30,ad1$*30,ad2$*30,csz$*30,ss$*11,ph$*12,a(9),dt(5),cd(4)
00080   dim gl(3),ta(2),aa(2),gl$(5)*12,gld$(5)*20,gla(5),id$*20
00090   dim cap$*128,up$(4)*18
00100   dim resp$(20)*80
00110 ! ______________________________________________________________________
00130   let fntop(program$,cap$="Import from old AP")
00140   let fncno(cno)
44000 SCR1: ! 
44020   let fntos(sn$='AP-Import')
44040   let lc=0
44060   let mylen=40
44080   let mypos=mylen+2
44100   let fnlbl(lc+=1,1,'Path to Accounts Payable Data Files:',mylen,1)
44120   let fntxt(lc,mypos,40,58,0,'70',0,"Pick any file in the directory, it doesn't matter which one - Only the directory name matters")
44140   let resp$(1)='C:\acs\client\nwelec\old\APmstr'
44160   let fnlbl(lc+=1,1,'Old Accounts Payable Company Number:',mylen,1)
44180   let fnlbl(2,90,'') ! work around to make the little button show up
44200   let fnlbl(3,80,'') ! work around to make the little button show up
44220   let fntxt(lc,mypos,2,0,1,'30')
44240   let resp$(2)='3'
44260   let fncmdset(5)
44280   let fnacs(sn$,0,mat resp$,ckey)
46000   if ckey=5 then goto XIT
46020   let apcno=val(resp$(2))
46040   if ~exists('Q:\tmpAP') then 
46060     execute 'mkdir Q:\tmpAP'
46080   else 
46100     execute 'Free Q:\tmpAP\*.*' ioerr ignore
46120   end if 
48000   let fncopy(resp$(1)&'\*.h'&str$(apcno),'Q:\tmpAP\*.*')
48040   if exists("Q:\tmpAP\apcoinfo.h"&str$(apcno))=0 then goto SCR1
50000   open #apmstr=11: "Name=Q:\tmpAP\APmstr.h"&str$(apcno)&",KFName=Q:\tmpAP\apIndex.h"&str$(apcno),internal,input,keyed 
50020   open #aptrans=10: "Name=Q:\tmpAP\apTrans.H"&str$(apcno),internal,outin,relative 
50040   open #paymstr=21: "Name=Q:\CLmstr\PayMstr.H"&str$(cno)&",Version=1,size=0,RecL=276,Replace",internal,outin,relative 
50060 ! open #PAYMSTR=21: "Name=Q:\CLmstr\PayMstr.h"&STR$(CNO)&",Version=1,KFName=Q:\CLmstr\PayIdx1.h"&STR$(CNO)&",Size=0,RecL=164,Replace",Internal,Outin,Keyed
50080   open #payalloc=22: "Name=Q:\CLmstr\PayAlloc.H"&str$(cno)&",Size=0,RecL=56,Replace",internal,outin,relative 
50100   open #paytrans=23: "Name=Q:\CLmstr\PayTrans.H"&str$(cno)&",Version=2,Size=0,RecL=114,Replace",internal,outin,relative 
50120   open #unpdaloc=24: "Name=Q:\CLmstr\UnPdAloc.h"&str$(cno)&",SIZE=0,RecL=70,Replace",internal,outin,relative 
52000   do 
52020     read #apmstr,using 'Form POS 1,C 8,4*C 30,POS 159,C 12,POS 176,PD 5.2,POS 219,N 2,C 11,POS 213,2*PD 3': vn$,nam$,ad1$,ad2$,csz$,ph$,ytdp,typ,ss$,mat ta eof EO_11
52040     gosub UNPDMSTR
52060     mat ta=(0)
52080     let lr2=lrec(payalloc)+1
52100     write #payalloc,using 'Form POS 1,C 8,N 3,N 6,N 3,PD 3.2,C 30,PD 3',rec=lr2: vn$,mat gl,100,"",0
52120     mat ta=(lr2)
52140     let lr1=lrec(paymstr)+1
52160     write #paymstr,using 'Form POS 1,C 8,4*C 30,PD 5.2,N 2,C 11,2*PD 3,C 12',rec=lr1: vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$,mat ta,ph$
52180   loop 
54000 EO_11: ! 
54020   close #paymstr: 
54040   close #payalloc: 
54060   close #paytrans: 
54080   close #unpdaloc: 
54100   close #aptrans,free: 
54120   close #apmstr,free: 
54140   execute 'free Q:\tmpAP\*.* -n'
54160   execute 'RmDir Q:\tmpAP'
56000   let fnindex_it("Q:\CLmstr\PayMstr.h"&str$(cno),"Q:\CLmstr\PayIdx1.h"&str$(cno),"1,8")
56020   let fnindex_it("Q:\CLmstr\PayMstr.h"&str$(cno),"Q:\CLmstr\PayIdx2.h"&str$(cno),"9,28")
56040   let fnindex_it("Q:\CLmstr\PayTrans.H"&str$(cno),"Q:\CLmstr\UnPdIdx1.h"&str$(cno),"1,20")
56060   let fnindex_it("Q:\CLmstr\PayTrans.H"&str$(cno),"Q:\CLmstr\UnPdIdx2.h"&str$(cno),"31/27/1 2/4/26")
56080   let fnindex_it("Q:\CLmstr\Unpdaloc.H"&str$(cno),"Q:\CLmstr\Uaidx1.h"&str$(cno),"9 12")
56100   let fnindex_it("Q:\CLmstr\Unpdaloc.H"&str$(cno),"Q:\CLmstr\Uaidx2.h"&str$(cno),"1 20")
56120   goto DONE ! /r
58000 DONE: let fnchain("R:\acsCL\Conversion\GLBLD-CNV")
58040 XIT: let fnxit
58060 ! ______________________________________________________________________
60000 UNPDMSTR: ! r: BUILD UNPAID FILE
60020   let adr=ta(1)
60040 READ_APTRANS: ! 
60060   if adr=0 then goto EO_UNPDMSTR
60080   read #aptrans,using 'Form POS 1,C 8,C 12,C 20,8*PD 5.2,6*PD 4,3*N 1,N 2,6*C 12,5*C 20,5*PD 5.2,PD 3',rec=adr,reserve: v$,iv$,id$,mat a,mat dt,mat cd,dgl$,mat gl$,mat gld$,mat gla,nta norec EO_UNPDMSTR
60100   if dt(4)>0 then let adr=nta: goto READ_APTRANS ! only unpaids
60120   if a(2)=0 then goto UNPDMSTR_ATZ
60140   mat aa=(0)
60160   for j=1 to 5
60180     if gla(j)=0 then goto NXJ
60200     on cd(2) goto XB,XA,XA,XB,XA,XA none XB
60220 XA: ! 
60240     let gla(j)=-gla(j)
60260 XB: ! 
60280     let lr4=lrec(unpdaloc)+1
60300     write #unpdaloc,using 'Form POS 1,C 8,2*C 12,PD 5.2,C 30,PD 3',rec=lr4: vn$,iv$,gl$(j),gla(j),gld$(j),0
60320     if aa(1)=0 then let aa(1)=lr4
60340     if aa(2)>0 then 
60360       rewrite #unpdaloc,using 'Form POS 68,PD 3',rec=aa(2): lr4
60380     end if 
60400     let aa(2)=lr4
60420 NXJ: ! 
60440   next j
60460   if cd(2)=1 or cd(2)=4 or cd(2)=0 then 
60480     goto XD
60500   else if cd(2)=2 or cd(2)=3 or cd(2)=5 or cd(6)=6 then 
60520     goto XC
60540   end if 
60560 XC: ! 
60580   let a(2)=-a(2)
60600 XD: ! 
60620   if dt(1)=0 then let dt(1)=dt(5)
60640   let vn$=v$
60660   let upa=a(2) ! unpaid amount
60680   let up$(1)=str$(dt(1)) ! invoice date
60700   let up$(2)=str$(dt(2)) ! due date
60720   let up$(3)="" ! po #
60740   let up$(4)=id$(1:18)
60760   let lr3=lrec(paytrans)+1
60780   write #paytrans,using 'Form POS 1,C 8,c 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8': vn$,iv$,mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate
60800 UNPDMSTR_ATZ: ! 
60820   let adr=nta
60840   goto READ_APTRANS
60860 EO_UNPDMSTR: ! 
60880   return  ! /r
70000 ! <Updateable Region: ERTN>
70020 ERTN: let fnerror(cap$,err,line,act$,"xit")
70040   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
70060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
70080   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
70100 ERTN_EXEC_ACT: execute act$ : goto ERTN
70120 ! /region
