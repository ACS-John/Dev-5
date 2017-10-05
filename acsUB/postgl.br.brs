00010 ! Replace S:\acsUB\postgl
00020 ! Company Information File
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit,fnerror,fntop,fnxit,fnchain,fnstyp,fntos,fnlbl,fntxt,fnchk,fnqgl25,fnrgl$,fncomboa,fncmdkey,fnacs,fnagl$,fnmsgbox,fnfra,fnopt,fncmdset,fndat,fnopenprn,fncloseprn,fnflexinit1,fnflexadd1,fnqgl,fnget_services
00050   on error goto ERTN
00060   dim cap$*128,resp$(40)*60,gln$(10,3)*12,servicename$(10)*20
00070   dim dat$*20,amount(10,3),tg(10),totaltg(10),heading$*130,dollar(3)
00080   dim msgline$(3)*80,glwk$*256,option2$(10)*20,service$*20,gl$(3)*12
00090   dim item$(6)*20,service$(10)*20,a(7)
00100   fntop(program$,cap$="Post General Ledger")
00120   fndat(dat$,1)
00130   fnget_services(mat servicename$)
00140   let x=0
00150   for j=1 to 10
00160     service$(j)=servicename$(j)
00170     if trim$(servicename$(j))<>"" then option2$(x+=1)=servicename$(j)
00180     if trim$(servicename$(j))="" then servicename$(j)="N/A"
00190     servicename$(j)=trim$(servicename$(j)(1:8))&":"
00200   next j
00210   option2$(x)
00220   open #customer=1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\UBIndex.h"&env$('cno'),internal,input,keyed 
00230   open #ubtransvb=2: "Name="&env$('Q')&"\UBmstr\ubTransVB.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubtrindx.h"&env$('cno'),internal,input,keyed 
00240   let glwk$=env$('Q')&"\GLmstr\GL_Work_"&env$('acsUserId')&".h"&env$('cno')
00250   open #14: "Name="&glwk$&",Replace,RecL=104",internal,output ioerr ignore
00260   if exists(env$('Q')&"\UBmstr\glinfo.h"&env$('cno'))=0 then goto L270 else goto L290
00270 L270: open #15: "Name="&env$('Q')&"\UBmstr\Glinfo.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\glinfoidx.h"&env$('cno')&",Shr,Use,RecL=89,KPs=1,KLn=23",internal,outin,keyed 
00280   close #15: 
00290 L290: open #15: "Name="&env$('Q')&"\UBmstr\Glinfo.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\glinfoidx.h"&env$('cno')&",Shr",internal,outin,keyed 
00300 ! ______________________________________________________________________
00310   fnstyp(14)
00312   !  styp=11 for jobcost; styp=14 for regular payroll
00330   goto SCREEN1
00340 ! ______________________________________________________________________
00350 SCREEN2: ! 
00360   respc=x=0
00370   fntos(sn$="Postgl-2")
00372   mylen=10: mypos=mylen+3 : right=1 : respc=0
00380   let fram1=1: fnfra(1,1,12,100,"General Ledger Information","For each service, choose the appropriate g/l# for the bank, for the revenue, and possoibly the receivalbe account if using the accrual method.")
00390   fnlbl(2,20,"Cash In Bank",12,right,0,fram1)
00400   fnlbl(2,45,"Receivable",12,right,0,fram1)
00410   fnlbl(2,75,"Revenue",12,right,0,fram1)
00420   let x=0
00430   for j=1 to 10
00440     fnlbl(j+2,1,servicename$(j),mylen,right,0,fram1)
00450     fnqgl25(j+2,12,fram1,2,pas)
00452     resp$(respc+=1)=fnrgl$(gln$(x+=1))
00460     fnqgl25(j+2,42,fram1,2,pas)
00462     resp$(respc+=1)=fnrgl$(gln$(x+=1))
00470     fnqgl25(j+2,72,fram1,2,pas)
00472     resp$(respc+=1)=fnrgl$(gln$(x+=1))
00480   next j
00490   fncmdkey("&Save",1,1,0,"Saves any changes and returns to menu.")
00500   fncmdkey("&Create Accounts",3,0,0,"Allows you to create a chart of account (limited to the accounts you need) if general ledger or checkbook is not installed.")
00510   fncmdkey("&Cancel",5,0,1,"Returns to menu without saving any changes on the screen.")
00520   fnacs(sn$,0,mat resp$,ckey)
00530   if ckey=5 then goto L580
00540   if ckey=3 then chain "S:\acsUB\chartofaccounts"
00550   for j=1 to 30
00560     let gln$(j)=fnagl$(resp$(j))
00570   next j
00580 L580: rewrite #15,using "form pos 1,c 20,n 3,3*c 12,3*n 10.2",key=key$: service$,ratecode,gl$(1),gl$(2),gl$(3),mat amount
00590   goto SCREEN1
00600 SCREEN1: ! 
00610   fntos(sn$='Postub2')
00612   mylen=36 : mypos=mylen+2
00620   fnlbl(1,1,"Report Heading Date:",mylen,1,0)
00630   fntxt(1,mypos,20) 
00632   resp$(1)=dat$
00640   fnlbl(2,1,"Starting Date (blank for all):",mylen,1)
00650   fntxt(2,mypos,10,0,1,"3",0,"Enter the first day of the period being posted.")
00652   resp$(2)=str$(ld1)
00660   fnlbl(3,1,"Ending Date (blank for all):",mylen,1)
00670   fntxt(3,mypos,10,0,1,"3",0,"Enter the Last day of the period being posted.")
00672   resp$(3)=str$(hd1)
00680   fnchk(4,30,"General Ledger Installed:",1,0)
00682   if gli=1 then resp$(4)="True" else resp$(4)="False"
00690   fnchk(5,30,"Print Report:",1,0)
00692   if printreport=1 then resp$(5)="True" else resp$(5)="False"
00700   fnchk(6,30,"Show Details:",1,0)
00702   if showdetails=1 then resp$(6)="True" else resp$(6)="False"
00710   fnfra(8,1,2,60,"Method of Posting","You can either post on a Cash basis which only effects Cash and Revenues or you can post on an accrual method also effecting receivables.")
00720   fnopt(1,3,"Cash Basis",0,1) !:
        if basis=0 or basis=1 then resp$(7)="True"
00730   fnopt(2,3,"Accrual Method",0,1) !:
        if basis=2 then resp$(8)="False"
00740   fncmdkey("&Post",1,1,0,"Begins the posting process.")
00750   fncmdkey("&Assign GL Numbers",2,0,0,"Assign general ledger numbers to the various revenue accounts.")
00760   fncmdkey("&Cancel",5,0,1,"Returns to menu without saving any changes on the screen.")
00770   fnacs(sn$,0,mat resp$,ckey)
00780   if ckey=5 then goto XIT
00790   dat$=resp$(1) : ld1=val(resp$(2)) : hd1=val(resp$(3))
00800   postingdate=val(resp$(3)(5:8))*100+val(resp$(3)(3:4))
00810   if resp$(4)="True" then let gli=1 else let gli=0
00820   if resp$(5)="True" then printreport=1 else printreport=0
00830   if resp$(6)="True" then showdetails=1
00840   basis=0 : if resp$(7)="True" then basis=cash=1
00850   if resp$(8)="True" then basis=accrual=2
00860   if ld1>hd1 and ld1>0 and hd1>0 then !:
          mat msgline$(1): msgline$(1)="Ending Date Before Starting Date!" !:
          fnmsgbox(mat msgline$,resp$,cap$,48) : goto SCREEN1
00870   if basis=0 then !:
          mat msgline$(1): msgline$(1)="You must enter the basis for accounting!" !:
          fnmsgbox(mat msgline$,resp$,cap$,48) : goto SCREEN1
00880   if ckey=2 then goto GL_INFORMATION
00890   fnopenprn
00900   gosub HDR
00910   goto ACCUMULATE_TOTALS
00920 ! ______________________________________________________________________
00930   restore #15: 
00940 L940: read #15,using "form pos 60,3*n 10.2": mat dollar eof ACCUMULATE_TOTALS ! clear totals on end of each allocation record
00950   mat dollar=(0)
00960   rewrite #15,using "form pos 60,3*n 10.2": mat dollar
00970   goto L940
00980 ACCUMULATE_TOTALS: ! 
00990 L990: read #ubtransvb,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof CREATE_ENTRIES
01000   if tdate<ld1 or tdate>hd1 then goto L990
01010   read #1,using 'form pos 143,7*pd 2,pos 1806,3*n 2',key=p$: mat a,extra11,extra12,extra13 nokey L1030
01020   goto L1040
01030 L1030: mat msgline$(2): msgline$(1)="You have tranactions on customer # "&p$ !:
        msgline$(2)="but the customer record no longer exists. "&p$ !:
        fnmsgbox(mat msgline$,resp$,cap$,48)
01040 L1040: ! 1=charge,2=penalty,3=collection,4=credit memo, 5=debit memo
01050   for j =1 to 10
01060     if basis=cash and tcode=1 or tcde=2 or tcode=4 or tcode=5 then goto L990 ! dont record anything but collections on cash basis.
01070     if basis=cash and tcode=3 then amount(j,1)=amount(j,1)+tg(j) !:
            amount(j,3)=amount(j,3)-tg(j) !:
            let glcode=13 !:
            goto L1130 !:
            ! debit bank and credit revenues on collections
01080     if basis=accrual and tcode=1 then amount(j,2)=amount(j,2)+tg(j) !:
            amount(j,3)=amount(j,3)-tg(j) !:
            let glcode=23 !:
            ! record sales as debit to receivables and credit to  sales
01090     if basis=accrual and tcode=2 then amount(j,2)=amount(j,2)+tg(j) !:
            amount(j,3)=amount(j,3)-tg(j) !:
            let glcode=23 ! record penalties as debit to receivables and credits to  penalty sales
01100     if basis=accrual and tcode=3 then amount(j,1)=amount(j,1)+tg(j) !:
            amount(j,2)=amount(j,2)-tg(j) !:
            let glcode=12 ! debit bank, reduce receivables on collections
01110     if basis=accrual and tcode=4 then amount(j,2)=amount(j,2)-tg(j) !:
            amount(j,3)=amount(j,3)+tg(j) !:
            ! reduce receivables and sales on credit memos
01120     if basis=accrual and tcode=5 then amount(j,2)=amount(j,2)+tg(j) !:
            amount(j,3)=amount(j,3)-tg(j) !:
            ! increase receivables and increase sales on debit memos
01130 L1130: next j
01140   if showdetails=1 then pr #255,using "form pos 1,c 10,x 1,pic(zzzz/zz/zz),n 2,10 * n 9.2": p$,tdate,tcode,mat tg pageoflow PAGE_OVER_FLOW
01150   goto L990
01160 CREATE_ENTRIES: ! 
01170   for j=1 to 10
01180     if j=1 then ratecode=a(1)
01190     if j=2 then ratecode=a(2)
01200     if j=3 then ratecode=a(3)
01210     if j=4 then ratecode=a(4)
01220     if j=5 then ratecode=a(5)
01230     if j=6 then ratecode=extra11
01240     if j=7 then ratecode=extra12
01250     if j=8 then ratecode=extra13
01260     if j=9 then ratecode=a(6)
01270     if j=10 then ratecode=a(7)
01280     read #15,using "form pos 24,3*c 12",key=service$(j)&cnvrt$("pic(zz#)",ratecode): mat gl$ nokey L1300
01290     let gln$(j,1)=gl$(1): let gln$(j,2)=gl$(2): let gln$(j,3)=gl$(3) ! substitute individual breakdown based on water codes, etc into 30 item array used in entry generation
01300 L1300: next j
01310   let x=0
01320   for j=1 to 10
01330 L1330: let x+=1 : if x=4 then let x=0 : goto L1370
01340     if amount(j,x)=0 then goto L1360
01350     write #14,using "Form POS 1,c 12,N 6,PD 6.2,N 2,N 2,C 12,C 52,C 12": gln$(j,x),postingdate,amount(j,x),3,0,"UB","Utility Billing Summary","" ! RGL$(15)
01360 L1360: goto L1330
01370 L1370: next j
01380   let x=0
01390   pr #255,using "form skip 1,pos 1,c 60": "G/L Number                Debits      Credits"
01400   for j=1 to 10
01410 L1410: let x+=1: if x>3 then let x=0 : goto L1460
01420     if amount(j,x)=0 then goto L1450
01430     if amount(j,x)>0 then pr #255,using "form pos 1,c 12,x 7,n 13.2": gln$(j,x),amount(j,x) else pr #255,using "form pos 1,c 12,x 20,n 13.2": gln$(j,x),amount(j,x) pageoflow PAGE_OVER_FLOW
01440     if amount(j,x)>0 then totaldebits+=amount(j,x) else totalcredits+=amount(j,x)
01450 L1450: goto L1410
01460 L1460: next j
01470   pr #255,using "form pos 20,n 13.2,n 13.2": totaldebits,totalcredits
01480   fncloseprn
01490   if gli=1 then let fnchain("S:\acsGL\acglMrge")
01500 XIT: fnxit
01510 HDR: ! 
01520   pr #255: "\qc  {\f181 \fs18 \b "&env$('cnam')&"}"
01530   pr #255: "\qc {\f181 \fs24 \b UB Posting Recap}"
01540   pr #255: "\qc {\f181 \fs24 \b "&dat$&"}"
01550   if ld1<>0 and hd1<>0 then !:
          pr #255: "\qc {\f181 \fs18 \b "&trim$("From "&cnvrt$("pic(zzzz/zz/zz)",ld1)&" to "&cnvrt$("pic(zzzz/zz/zz)",hd1))&"}"
01560   pr #255,using 'Form POS 1,C 20,POS 110,C 12': "\ql","Page "&str$(p2+=1)
01570   pr #255: 
01580   heading$="Account      Date     Cd"
01590   for h=1 to 10
01600     heading$=heading$&lpad$(servicename$(h)(1:19),9)
01610     a=pos(heading$,":",1)
01620     if a>0 then heading$(a:a)=" "
01630   next h
01640   pr #255,using "form pos 1,c 130": heading$
01650   return 
01660 PAGE_OVER_FLOW: ! 
01670   pr #255: newpage
01680   gosub HDR
01690   continue 
01700 ! ______________________________________________________________________
01710 ! <Updateable Region: ERTN>
01720 ERTN: fnerror(program$,err,line,act$,"xit")
01730   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01740   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01750   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01760 ERTN_EXEC_ACT: execute act$ : goto ERTN
01770 ! /region
01780 ! ______________________________________________________________________
01790 GL_INFORMATION: ! 
01800   fntos(sn$="Breakdown") !:
        respc=0
01810   mat chdr$(6) : mat cmask$(6) : mat item$(6) !:
        chdr$(1)='Rec' !:
        chdr$(2)='Service Name' : chdr$(3)='Rate Code' !:
        chdr$(4)='GL-Cash' : chdr$(5)='GL-Receivable' !:
        chdr$(6)='GL-Revenue'
01820   cmask$(1)=cmask$(2)='' !:
        cmask$(3)=cmask$(4)=cmask$(5)=cmask$(6)='' !:
        fnflexinit1('GlBreak',1,1,20,100,mat chdr$,mat cmask$,1,0,frame) !:
        editrec=0
01830   restore #15: 
01840 READ_GLINFO_1: ! 
01850   read #15,using "form pos 1,c 20,n 3,3*c 12,3*n 10.2": service$,ratecode,gl$(1),gl$(2),gl$(3),mat dollar eof EO_FLEX1
01860   item$(1)=str$(rec(15)) !:
        item$(2)=service$ : item$(3)=str$(ratecode) !:
        item$(4)=gl$(1) !:
        item$(5)=gl$(2) !:
        item$(6)=gl$(3) !:
        fnflexadd1(mat item$)
01870   goto READ_GLINFO_1
01880 EO_FLEX1: ! 
01890   fncmdkey("&Add",1,0,0,"Add new records") !:
        fncmdkey("&Edit",2,1,0,"Highlight any record and press Enter or click Edit or press Alt+E to change any existing record.") !:
        fncmdkey("&Delete",3,0,0,"Highlight any record and press Alt+D or click Delete to remove any existing record.") !:
        fncmdkey("E&xit",5,0,1,"Exit to menu")
01900   fnacs(sn$,0,mat resp$,ck)
01910   addone=edit=0: holdvn$=""
01920   if ck=5 then goto SCREEN1 !:
        else if ck=1 then addone=1: service$="": ratecode=0: mat gl$=("") !:
          goto MAINTAIN_GLINFO
01930 if ck=2 or ck=3 then editrec=val(resp$(1))
01940 if editrec=0 then goto GL_INFORMATION
01950 if ck=2 or ck=3 then !:
        read #15,using "form pos 1,c 20,n 3,3*c 12,3*n 10.2",rec=editrec: service$,ratecode,gl$(1),gl$(2),gl$(3),mat dollar
01960 if ck=2 then edit=1 : holdvn$=vn$: goto MAINTAIN_GLINFO
01970 if ck=3 then gosub DELETE_GLINFO : goto GL_INFORMATION
01980 ! ______________________________________________________________________
01990 DELETE_GLINFO: ! 
01995 mat msgline$(2): msgline$(1)="You have chosen to delete a record." !:
      msgline$(2)="Take OK to delete, Cancel to retain." !:
      fnmsgbox(mat msgline$,resp$,cap$,49)
01996 if resp$="OK" then goto L2000 else goto L2010
02000 L2000: delete #15,rec=editrec: 
02010 L2010: goto GL_INFORMATION
02020 REINDEX: ! 
02030 execute "Index "&env$('Q')&"\UBmstr\Ubinfo.h"&env$('cno')&' '&env$('Q')&"\UBmstr\ubinfoidx.h"&env$('cno')&" 1 23 Replace DupKeys -n" ioerr L2040
02040 L2040: return 
02050 MAINTAIN_GLINFO: ! 
02060 right=1: mylen=25: mypos=mylen+3
02070 let fntos(sn$="Glinfo2")
02080 let fnlbl(1,1,"Service:",mylen,right)
02090 let fncomboa("GLCmbSrv",1,mypos,mat option2$,"Set up records for every service and all rate codes within that service",20)
02100 for j=1 to udim(optio2$)
02110   if option2$(j)=service$ then resp$(1)=option2$(j)
02120 next j
02130 let fnlbl(2,1,"Rate Code:",mylen,right)
02140 let fntxt(2,mypos,3,0,0,"30",0,"Set up the general ledger informatin for each rate code you have for each service.") !:
      resp$(2)=str$(ratecode)
02150 let fnlbl(3,1,"Cash G/L Number:",mylen,right)
02160 let fnqgl(3,mylen,0,2,pas) !:
      resp$(3)=fnrgl$(gl$(1))
02170 let fnlbl(4,1,"Receivable G/L Number:",mylen,right)
02180 let fnqgl(4,mylen,0,2,pas) !:
      resp$(4)=fnrgl$(gl$(2))
02190 let fnlbl(5,1,"Revenue G/L Number:",mylen,right)
02200 let fnqgl(5,mylen,0,2,pas) !:
      resp$(5)=fnrgl$(gl$(3))
02210 let fncmdkey("&Save",1,1,0,"Saves any changes and returns to gl breakdown screen.")
02220 let fncmdkey("&Create Accounts",3,0,0,"Allows you to create a chart of account (limited to the accounts you need) if general ledger or checkbook is not installed.")
02230 let fncmdkey("&Cancel",5,0,1,"Return to main screen without saving any changes.")
02240 let fnacs(sn$,0,mat resp$,ckey)
02250 if ckey=5 then goto GL_INFORMATION
02260 if ckey=3 then chain "S:\acsUB\chartofaccounts"
02270 service$=resp$(1)
02280 ratecode=val(resp$(2))
02290 let gl$(1)=fnagl$(resp$(3))
02300 let gl$(2)=fnagl$(resp$(4))
02310 let gl$(3)=fnagl$(resp$(5))
02320 breakdownde$=resp$(2)
02330 if edit=1 then rewrite #15,using "form pos 1,c 20,n 3,3*c 12,3*n 10.2",rec=editrec: service$,ratecode,gl$(1),gl$(2),gl$(3),mat dollar
02340 if addone=1 then write #15,using "form pos 1,c 20,n 3,3*c 12,3*n 10.2": service$,ratecode,gl$(1),gl$(2),gl$(3),mat dollar
02350 goto GL_INFORMATION
02360 ! 
