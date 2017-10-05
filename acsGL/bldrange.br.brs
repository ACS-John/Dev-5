00010 ! Replace S:\acsGL\BldRange
00020 ! Build Range of Accounts
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror, fnchain,fntos,fnlbl,fnqgl,fnrgl$,fntxt,fnfra,fncombof,fncmdkey,fnacs,fnagl$,fncmdset,fnconsole
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim io1$(12),gln(3,3),fin(3),ta(2),ac(18),te$*1
00080   dim d$*50,bc(13),bp(13),bm(13),rf(6),glk$*12,fsk$*5,resp$(20)*50
00090 ! ______________________________________________________________________
00100   ! fnconsole(off=0)
00110   fntop(program$,"Duplicate Range of Accounts")
00130   open #company=1: "Name="&env$('Q')&"\GLmstr\Company.h"&env$('cno')&",Shr",internal,input 
00140   read #company,using 'Form Pos 150,2*N 1': use_dept,use_sub ! read fund and sub codes from general
00150   close #company: 
00160   open #1: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\GLIndex.h"&env$('cno')&",Shr",internal,outin,keyed 
00170   fil$(1)="ACGLFNSB" : idx$(1)="FNSBINDX"
00180   fil$(2)="ACGLFNSI" : idx$(2)="FNSIINDX"
00190 ! ______________________________________________________________________
00210 MAIN: ! 
00230   fntos(sn$="Bldrange") 
00232   mylen=40: mypos=mylen+3 : right=1: rc=0
00240   fnfra(1,1,6,90,"Duplicate Range of Accounts","This option will allow you to quickly duplicate a range of general ledger numbers when setting up multiple departments with similar accounts. ",0)
00250   fnlbl(1,50,"Source",0,0,0,1)
00260   fnlbl(2,1,"1st G/L Number to Duplicate:",mylen,right,0,1)
00270   fnqgl(2,mypos,1,0) 
00272   resp$(rc+=1)=fnrgl$(gl1$)
00280   fnlbl(3,1,"Last G/L Number to Duplicate:",mylen,right,0,1)
00290   fnqgl(3,mypos,1,0) 
00292   resp$(rc+=1)=fnrgl$(gl2$)
00300   fnlbl(6,1,"First new general ledger # to be used:",mylen,right,0,1)
00310   fnlbl(5,44,"Fund #",6,1,0,1)
00320   fnlbl(5,58,"Sub #",6,2,0,1)
00330   if use_dept=1 then let fntxt(6,46,3,0,right,"30",0,"Enter the fund portion of the general ledger number.",1 ) else let fntxt(6,46,3,0,right,"30",1,"Enter the fund portion of the general ledger number.",1 ) ! : rESP$(RC+=1)=STR$(DNO)
00340   fntxt(6,51,6,0,right,"30",0,"Enter the main part of the general ledger number.",1 ) 
00342   resp$(rc+=1)=""
00350   if use_sub=1 then let fntxt(6,60,3,0,right,"30",0,"Enter the sub portion of the general ledger number.",1 ) else let fntxt(6,60,3,0,right,"30",1,"Enter the sub portion of the general ledger number.",1 ) 
00352     resp$(rc+=1)=""
00360   fnfra(9,1,8,125,"Duplicating Matching Range of Financial Statement Formats"," ",0)
00370 ! 
00380   fnlbl(1,1,"Beginning                         Ending",90,right,0,2)
00390   fnlbl(2,1,"Balance Sheet Refernece Number:",mylen,right,0,2)
00400   fncombof("fs-bal",2,43,25,env$('Q')&"\GLmstr\acglfnsb.h"&env$('cno'),1,5,6,30,env$('Q')&"\GLmstr\Fnsbindx.h"&env$('cno'),0,pas, "If the accounts you are duplicating are balance sheet accounts, select the beginning balance sheet reference number to match the first new balance sheet account.",2) 
00402   resp$(rc+=1)="" ! first balance sheet ref # to be duplicated
00410   fncombof("fs-bal2",2,85,25,env$('Q')&"\GLmstr\acglfnsb.h"&env$('cno'),1,5,6,30,env$('Q')&"\GLmstr\Fnsbindx.h"&env$('cno'),0,pas, "Select the last balance sheet reference number to be duplicated.",2) 
00412   resp$(5)="" ! ending balance sheet ref # to be duplicated
00420   fnlbl(4,1,"Beginning                         Ending",90,right,0,2)
00430   fnlbl(5,1,"Income Statement Refernece Number:",mylen,right,0,2)
00440   fncombof("fs-inc",5,43,25,env$('Q')&"\GLmstr\acglfnsi.h"&env$('cno'),1,5,6,30,env$('Q')&"\GLmstr\Fnsiindx.h"&env$('cno'),0,pas, "If you are duplicating income statement accounts, enter the first income statement reference to be duplicated.",2) 
00442   resp$(rc+=1)="" ! 1st income statement ref # to be duplicated
00450   fncombof("fs-inc-2",5,85,25,env$('Q')&"\GLmstr\acglfnsi.h"&env$('cno'),1,5,6,30,env$('Q')&"\GLmstr\Fnsiindx.h"&env$('cno'),0,pas, "If you are duplicating income statement accounts, enter the last income statement reference to be duplicated.",2) 
00452   resp$(rc+=1)="" ! last income statement ref # to be duplicated
00460   fnlbl(7,1,"First new reference # to be used:",mylen,right,0,2)
00470   fntxt(7,mylen+3,5,0,right,"30",0,"Enter the first new financial statement reference number to be matched with the new general ledger numbers.",2 ) 
00472   resp$(rc+=1)=""
00480   fncmdset(2)
00490   fnacs(sn$,0,mat resp$,ckey)
00500   pas=0 ! rebuild each time
00510   if ckey=5 then goto XIT
00520   gl$=fnagl$(resp$(1))
00530   gln(1,1)=val(gl$(1:3))
00540   gln(1,2)=val(gl$(4:9))
00550   gln(1,3)=val(gl$(10:12))
00560   gl$=fnagl$(resp$(2))
00570   gln(2,1)=val(gl$(1:3))
00580   gln(2,2)=val(gl$(4:9))
00590   gln(2,3)=val(gl$(10:12))
00600   gl$=cnvrt$("pic(Zz#)",val(resp$(3)))&cnvrt$("pic(zzzZz#)",val(resp$(4)))&cnvrt$("PIC(ZZ#)",val(resp$(5)))
00610   gln(3,1)=val(gl$(1:3))
00620   gln(3,2)=val(gl$(4:9))
00630   gln(3,3)=val(gl$(10:12))
00640   if val(resp$(6)(1:6))> 0 then fin(1)=val(resp$(6)(1:6))
00650   if val(resp$(7)(1:6))> 0 then fin(2)=val(resp$(7)(1:6))
00660   if val(resp$(8)(1:6))> 0 then fin(1)=val(resp$(8)(1:6))
00670   if val(resp$(9)(1:6))> 0 then fin(2)=val(resp$(9)(1:6))
00680   fin(3)=val(resp$(10))
00690   gl1=val(cnvrt$("PIC(###)",gln(1,1))&cnvrt$("PIC(######)",gln(1,2))&cnvrt$("PIC(###)",gln(1,3)))
00700   gl2=val(cnvrt$("PIC(###)",gln(2,1))&cnvrt$("PIC(######)",gln(2,2))&cnvrt$("PIC(###)",gln(2,3)))
00710   gl3=val(cnvrt$("PIC(###)",gln(3,1))&cnvrt$("PIC(######)",gln(3,2))&cnvrt$("PIC(###)",gln(3,3)))
00720   if gl1=0 then goto MAIN
00730   if gl2=0 then goto MAIN
00740   if gl3=0 then goto MAIN
00750   gf=gl3-gl1
00760   glk$=lpad$(str$(gln(1,1)),3)&lpad$(str$(gln(1,2)),6)&lpad$(str$(gln(1,3)),3)
00770   if fin(1)=0 then goto L990
00780   if fin(2)=0 then goto MAIN
00790   if fin(3)=0 then goto MAIN
00800 ! ______________________________________________________________________
00810   ff=fin(3)-fin(1)
00820   restore #1,key=glk$: nokey MAIN
00830 L830: read #1,using L1030: dno,ano,sno,d$,mat rf eof MAIN
00840   gl=val(cnvrt$("PIC(###)",dno)&cnvrt$("PIC(######)",ano)&cnvrt$("PIC(###)",sno))
00850   if gl>gl2 then goto MAIN
00860   if rf(1)>0 then fln=1: goto L890
00870   if rf(3)>0 then fln=2: goto L890
00880   goto L830
00890 L890: open #2: "Name="&env$('Q')&"\GLmstr\"&fil$(fln)&".h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\"&idx$(fln)&".h"&env$('cno')&",Shr",internal,outin,keyed 
00900   restore #2,key=lpad$(str$(fin(1)),5): nokey L970
00910 L910: read #2,using L920: rno,d$,te$,mat ac eof L980
00920 L920: form pos 1,n 5,c 50,c 1,2*n 2,15*n 1,n 3
00930   if rno>fin(2) then goto L980
00940   rno=rno+ff
00950   write #2,using L920: rno,d$,te$,mat ac
00960   goto L910
00970 L970: close #2: : goto MAIN
00980 L980: close #2: 
00990 L990: restore #1,key=glk$: nokey L1010
01000   goto L1020
01010 L1010: goto MAIN
01020 L1020: read #1,using L1030: dno,ano,sno,d$,mat rf eof END1
01030 L1030: form pos 1,n 3,n 6,n 3,c 50,6*pd 3,42*pd 6.2,2*pd 3
01040   gl=val(cnvrt$("PIC(###)",dno)&cnvrt$("PIC(######)",ano)&cnvrt$("PIC(###)",sno))
01050   if gl>gl2 then goto END1
01060   gl=gl+gf
01070   gl$=lpad$(str$(gl),12)
01080   dno=val(gl$(1:3))
01090   ano=val(gl$(4:9))
01100   sno=val(gl$(10:12))
01110   if fln=1 then rf(1)=rf(1)+ff
01120   if fln=2 then rf(3)=rf(3)+ff
01130   write #1,using L1030: dno,ano,sno,d$,mat rf,bb,cb,mat bc,mat bp,mat bm,pbp,mat ta
01140   goto L1020
01150 ! ______________________________________________________________________
01160 END1: !
01162   close #1: 
01170   close #2: ioerr ignore
01180   ! Execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&","&env$('Q')&"\GLmstr\GLIndex.h"&env$('cno')&",1,12,Replace,DupKeys -n"
01190   if fln>0 then execute "Index "&env$('Q')&"\GLmstr\"&fil$(fln)&".h"&env$('cno')&","&env$('Q')&"\GLmstr\"&idx$(fln)&".h"&env$('cno')&",1,5,Replace,DupKeys -n"
01200   goto MAIN
01210 ! ______________________________________________________________________
01220 XIT: fnxit
01230 ! ______________________________________________________________________
01240 ! <Updateable Region: ERTN>
01250 ERTN: fnerror(program$,err,line,act$,"xit")
01260   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01270   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01280   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01290 ERTN_EXEC_ACT: execute act$ : goto ERTN
01300 ! /region
