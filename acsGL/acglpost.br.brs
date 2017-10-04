00010 ! Replace S:\acsGL\ACGLPOST
00020 ! Post Entries from Holding File
00030 ! ______________________________________________________________________,
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnerror,fndate_mmddyy_to_ccyymmdd,fnlbl,fntos,fntxt,fncmdkey,fnacs,fnflexadd1,fnflexinit1,fnhamster,fnmsgbox,fncmdset,fnqgl,fnrgl$,fnagl$,fnindex_it,fngetdir2
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim ta(2)
00080   dim t$*12,n(2),l$*12,p$*30,ven$*8,zo(50),d$*50,cap$*128
00100   dim k(10,8),filename$(0)*132 ,dir(200),ml$(4)*80,item$(3)
00101   !
00102   mat chdr$(3) : mat cmask$(3)
00103   chdr$(1)="Date" 
00104   chdr$(2)="Creation Date" 
00105   chdr$(3)="Creation Time" 
00106   cmask$(1)='30'
00107   cmask$(2)=''
00108   cmask$(3)=''
00118 ! ______________________________________________________________________
00120   fntop(program$,cap$="Post Entries from Holding File")
00140   open #4: "Name="&env$('Q')&"\GLmstr\GLmstr.H"&env$('cno')&",Shr,KFName="&env$('Q')&"\GLmstr\GLINDEX.H"&env$('cno')&",Shr",internal,outin,keyed 
00150   open #2: "Name="&env$('Q')&"\GLmstr\GLTRANS.H"&env$('cno')&",Shr",internal,outin,relative 
00160   gosub BUILD_LAYOUT
00170 MAIN: ! r:
00180   fntos(sn$="AcglPost") 
00182   let mylen=10: let mypos=mylen+3 : let right=1
00190   fnlbl(1,8,"Date Range to Post")
00200   fnlbl(2,1,"From:",mylen,right)
00210   fntxt(2,mypos,8,0,right,"1001",0,"'From' date must always be answered and will be the first date you wish to review for posting..",0 ) 
00212   let resp$(1)=str$(from)
00220   fnlbl(3,1,"To:",mylen,right)
00230   fntxt(3,mypos,8,0,right,"1001",0,"'To' date must always be answered and will be the last day of the month or the last day of the period being processed..",0 ) 
00232   let resp$(2)=str$(to)
00240   fnlbl(4,37,"")
00250   fncmdkey("&Next",1,1,0,"Allows you to select files to be posted.")
00260   fncmdkey("&Cancel",5,0,1,"Returns to menu without posting.")
00270   fnacs(sn$,0,mat resp$,ckey)
00280   if ckey=5 then goto XIT
00290   let from =val(resp$(1))
00300   let to =val(resp$(2))
00310   let from$=cnvrt$("PIC(######)",from): let to$=cnvrt$("PIC(######)",to)
00320 ! DATE_LIST: !
00330   fntos(sn$="AcglPost1") 
00332   mylen=10: let mypos=mylen+3 : let right=1
00350   fnflexinit1('acglpost2',lc=1,1,15,30,mat chdr$,mat cmask$,1)
00372   fngetdir2(env$('Q')&'\GLmstr\',mat filename$,'','GL*.H'&env$('cno'),mat filedate$,mat filetime$)
00410   let dircount=0
00420   for filenameItem=1 to udim(mat filename$)
00440     let x=val(filename$(filenameItem)(3:8)) conv L420
00450     if x<10100 or x>123199 then goto L420
00460     if fndate_mmddyy_to_ccyymmdd(x)>=fndate_mmddyy_to_ccyymmdd(val(from$)) and fndate_mmddyy_to_ccyymmdd(x)<=fndate_mmddyy_to_ccyymmdd(val(to$)) then 
00462       mat dir(dircount+=1) : dir(dircount)=val(filename$(filenameItem)(3:8))
00470       let item$(1)=filename$(filenameItem)(3:8) 
00481       let item$(2)=filedate$(filenameItem)
00482       let item$(3)=filetime$(filenameItem)
00483       fnflexadd1(mat item$)
00486     end if
00488     L420: !
00490   nex filenameItem
00500   fnlbl(17,30,"")
00510   fncmdkey("&Post",1,1,0,"Post entries from the holding files that are displayed.")
00520   fncmdkey("&Review",2,0,0,"Review entries before posting.")
00530   fncmdkey("&Print",3,0,0,"Prints list of entries.")
00540   fncmdkey("&Cancel",5,0,1,"Returns to menu without posting.")
00550   fnacs(sn$,0,mat resp$,ckey)
00560   if ckey=1 then listing$='N' : goto PRINT_POST
00570   if ckey=2 then goto REVIEW
00580   if ckey=3 then listing$="Y" : goto PRINT_POST
00590   if ckey=5 then goto XIT
00592 ! /r
00600 PRINT_POST: !
00602   if listing$="Y" then listing=1 else listing=0
00610   if listing=1 then gosub L1230
00620   for j3=1 to dircount
00630     if dir(j3)<>0 then 
00632       close #3: ioerr ignore
00640       open #3: "Name="&env$('Q')&"\GLmstr\GL"&cnvrt$("PIC(######)",dir(j3))&".H"&env$('cno')&",RecL=104,USE",internal,outin,relative 
00650       if listing=1 then 
00660         pr #255,using 'form pos 1,c 11,pic(zz/zz/zz),skip 2': "File Date: ",dir(j3)
00664       end if
00668       do
00670         L670: !
00672         read #3,using L680: t$,s,k,mat n,l$,p$,ven$ eof L1050
00680         L680: form pos 1,c 12,n 6,pd 6.2,n 2,n 2,c 12,c 30,c 8
00690         ! If N(2)=9 Then Goto 660 ! CHECK PREVIOUS POST
00700         if k=0 and uprc$(ltrm$(rtrm$(p$)))<>"VOID" then goto L670
00710         if val(t$(1:3))=0 and val(t$(4:9))=0 and val(t$(10:12))=0 then goto L670
00720         if t$(3:3)=" " then let t$(3:3)="0"
00730         if t$(12:12)=" " then let t$(12:12)="0"
00740         read #4,using L750,key=t$: cb,mat ta nokey BAD_ACCOUNT
00750         L750: form pos 87,pd 6.2,pos 333,2*pd 3
00760         L760: !
00762         read #2,using L900,rec=1: lr2
00770         lr2=lrec(2)+1
00780         write #2,using L880,rec=lr2,reserve: t$,s,k,mat n,l$,p$,0 duprec L760
00790         if k>0 then let x=25 else let x=40
00800         if listing=1 then pr #255,using L810: t$,s,k,l$,p$,n(1) pageoflow L1290
00810         L810: form pos 1,c 12,x 3,pic(zz/zz/zz),pos x,pic(---,---,---.##),pos 56,c 15,c 33,n 1,skip 1
00820         if k<0 then let totalcr=totalcr+k else let totaldr=totaldr+k
00830         if ta(1)=0 then let ta(1)=lr2
00840         if ta(2)>0 then rewrite #2,using L900,rec=ta(2),reserve: lr2
00850         let ta(2)=lr2
00860         cb=cb+k
00870         rewrite #4,using L750,key=t$: cb,mat ta
00880         L880: form pos 1,c 12,n 6,pd 6.2,n 2,n 2,c 12,c 30,pd 3
00890         rewrite #2,using L900,rec=1,release: lr2
00900         L900: form pos 71,pd 3
00910         rewrite #3,using L920: 9
00920         L920: form pos 27,n 2
00930       loop
00940     BAD_ACCOUNT: ! r:
00960       mat ml$(4) 
00962       let ml$(1)="Account # "&t$&" is not in the general ledger file." 
00964       let ml$(2)="Transaction information: Date "&str$(s)&"; Amount "&str$(k) 
00966       let ml$(3)="Description "&trim$(p$)&". Yes to setup this account" 
00968       let ml$(4)="or No to change account #" 
00969       fnmsgbox(mat ml$,resp$,cap$,52)
00970       if resp$="Yes" then let in1=1 else let in1=0 ! one is to set up the account
00980       if resp$="Yes" then gosub ADD
00982       if resp$="No" then gosub CHANGE_ACCOUNT
00990       mat ta=(0)
01000       cb=0
01010       write #4,using L1020: t$,d$,mat zo
01020       L1020: form pos 1,c 12,c 50,6*pd 3,42*pd 6.2,2*pd 3
01040       goto L760 ! /r
01050     L1050: !
01052       close #3: 
01060       if listing =1 then pr #255,using L1070: "------------","------------","DAILY TOTALS",totaldr,totalcr,"------------","------------"
01070       L1070: form pos 27,c 12,x 3,c 12,skip 1,pos 5,c 20,pos 24,2*pic(----,---,---.##),skip 1,pos 27,c 12,x 3,c 12,skip 1
01080       let gtdr=gtdr+totaldr: let gtcr=gtcr+totalcr
01090       let totaldr=totalcr=0
01100     end if
01102   next j3
01110   close #1: ioerr ignore
01120   close #2: ioerr ignore
01130   close #3: ioerr ignore
01140   if listing=1 then pr #255,using L1070: "            ","            ","GRAND TOTALS",gtdr,gtcr,"============","============" else goto L1170
01150   fncloseprn
01160   if listing=1 then goto MAIN
01170 L1170: !
01172   for j=1 to dircount
01180     if dir(j)<>0 then 
01190       execute "Free "&env$('Q')&"\GLmstr\GL"&cnvrt$("PIC(######)",dir(j))&".H"&env$('cno')
01192     end if 
01200   next j
01210 XIT: let fnxit
01220 ! ______________________________________________________________________
01230 L1230: let fnopenprn
01240   pr #255,using L1250: env$('cnam'),"General Ledger Posting","From: "&from$&"   To: "&to$
01250 L1250: form pos 1,cc 80,skip 1,pos 30,c 30,skip 1,pos 28,c 40,skip 1
01260   pr #255,using L1270: "  ACCOUNT #       DATE          DEBITS        CREDITS   REFERENCE #   DESCRIPTION                   SOURCE"
01270 L1270: form pos 1,c 132,skip 1
01280   return 
01290 L1290: pr #255: newpage
01300   gosub L1230
01310   continue 
01350 ! ______________________________________________________________________
01360 REVIEW: ! 
01370   fntos(sn$="AcglPost2") !:
        let mylen=20: let mypos=mylen+3 : let right=1
01380   fnlbl(1,1,"Date to Review:",mylen,right)
01390   fntxt(1,mypos,8,0,right,"1001",0,"Enter the file date to be reviewed.",0 ) !:
        let resp$(1)=""
01400   fnlbl(2,40,"")
01410   fncmdkey("&Next",1,1,0,"Review the entries for the date entered.")
01420   fncmdkey("&Cancel",5,0,1,"Returns to listing of dates.")
01430   fnacs(sn$,0,mat resp$,ckey)
01440   if ckey=5 then goto MAIN
01450   let review=val(resp$(1))
01460   goto L1480 ! use hamster to review entries
01470 ! ______________________________________________________________________
01480 L1480: ! Holding file Transactions - Hamster
01490 ! ______________________________________________________________________
01500   dim cap$*128,lbl$(8)*38,tln(8),p$(8)*160,fltyp$(8),sln(8),mask(8),sp(8),c$(8,8)*256
01510 ! ______________________________________________________________________
01520   gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE !:
        gosub HAMSTER : gosub CLOSE_FILE
01530 ! Open #3: "Name="&env$('Q')&"\GLmstr\GL"&CNVRT$("PIC(######)",DIR(J3))&".H"&env$('cno')&",RecL=104,USE",Internal,Outin,Relative
01540   goto MAIN
01550 ! ______________________________________________________________________
01560 OPEN_FILE: ! r:
01562   open_file_count=1 : close #3: ioerr ignore ! this value is used in the close_file sub routine
01570   if exists(env$('Q')&"\GLmstr\GL"&cnvrt$("PIC(######)",review)&".H"&env$('cno'))=0 then gosub INDEX
01580   open #open_file_count: "Name="&env$('Q')&"\GLmstr\GL"&cnvrt$("PIC(######)",review)&".H"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\GL"&cnvrt$("PIC(######)",review)&"-idx.H"&env$('cno')&",RecL=104,kps=1,kln=12,USE",internal,outin,keyed 
01590   return  ! /r
01600 ! ______________________________________________________________________
01610 CLOSE_FILE: ! r:
01612   for j=1 to open_file_count
01620     close #j: ioerr ignore
01630   next j
01640   return  ! /r
01650 IGNORE: continue 
01660 INDEX: ! r:
01670   if ~fnindex_it(env$('Q')&"\GLmstr\GL"&cnvrt$("PIC(######)",review)&".H"&env$('cno'),env$('Q')&"\GLmstr\GL"&cnvrt$("PIC(######)",review)&"-idx.H"&env$('cno'),"1 12") then goto MAIN
01690   return  ! /r
01700 ! ______________________________________________________________________
01930 ! ______________________________________________________________________
01940 HAMSTER: ! r:
01950   fnhamster("TrAlloc",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
01960   return  ! /r
02000 ADD: ! r:
02010   fntos(sn$="acglpost4") 
02012   let mylen=23: let mypos=mylen+3 : let right=1: let rc=0
02020   if use_dept =1 then let fnlbl(1,26,"Fund #",6,2)
02030   if use_sub =1 then let fnlbl(1,40,"Sub #",6,2)
02040   fnlbl(2,1,"General Ledger Number:",mylen,right)
02050   if use_dept=1 then 
02052     fntxt(2,26,3,0,right,"30",0,"Enter the fund portion of the general ledger number.",0 ) 
02054     let resp$(rc+=1)=str$(dno)
02056   end if
02060   fntxt(2,31,6,0,right,"30",0,"Enter the main part of the general ledger number.",0 ) 
02062   let resp$(rc+=1)=str$(ano)
02070   if use_sub=1 then 
02072     fntxt(2,40,3,0,right,"30",0,"Enter the sub portion of the general ledger number.",0 ) 
02074     let resp$(rc+=1)=str$(sno)
02076   end if
02080   fnlbl(3,1,"Description:",mylen,right)
02090   fntxt(3,mypos,50,0,left,"",0,"Enter the account description.",0 )
02092   let resp$(rc+=1)=""
02100   fncmdset(2)
02110   fnacs(sn$,0,mat resp$,ckey)
02120   let pas=0
02130   if ckey=5 then goto MAIN
02150   let dno=ano=sno=0
02160   if use_dept=1 then let dno=val(resp$(1)) : ano=val(resp$(2))
02170   if use_dept=0 then ano=val(resp$(1))
02180   if use_dept=1 and use_sub=1 then sno=val(resp$(3))
02190   if use_dept=0 and use_sub=1 then sno=val(resp$(2))
02200 ! 
02210   if use_dept=1 and use_sub=1 then let d$=resp$(4)
02220   if use_dept=0 and use_sub=1 then let d$=resp$(3)
02230   if use_dept=0 and use_sub=0 then let d$=resp$(2)
02240   if use_dept=1 and use_sub=0 then let d$=resp$(3)
02250   let key$=cnvrt$("N 3",dno)&cnvrt$("N 6",ano)&cnvrt$("N 3",sno)
02260   read #4,using 'Form POS 1,N 3',key=key$: dno nokey L2310 ! 
02270 ! MSGBOX2: !
02280   mat ml$(3) 
02282   let ml$(1)="General ledger account # "&key$&" already " 
02284   let ml$(2)="exists. Take OK to change the account." 
02286   let ml$(3)="Take Cancel to set the account up." 
02288   fnmsgbox(mat ml$,resp$,cap$,49)
02290   if resp$="Cancel" then goto ADD
02300   if resp$="Ok" then gosub CHANGE_ACCOUNT
02310 L2310: return  ! /r
02320 ! 
16000 CHANGE_ACCOUNT: ! r:
16020   fntos(sn$="acglpost3") 
16040   let mylen=23: let mypos=mylen+3
16060   fnlbl(1,1,"Bank Account #:",mylen,right)
16080   fnqgl(1,mypos,0,2,pas) 
16100   let resp$(1)=fnrgl$(bankgl$)
16120   fncmdkey("&Next",1,1,0,"Continue posting.")
16140   fnacs(sn$,0,mat resp$,ckey)
16160   let key$=bankgl$=fnagl$(resp$(1)) ! gl number
16180   return  ! /r
18410 ! <updateable region: ertn>
18420 ERTN: let fnerror(program$,err,line,act$,"xit")
18430   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
18440   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
18450   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
18460 ERTN_EXEC_ACT: execute act$ : goto ERTN
18470 ! /region
20000 BUILD_LAYOUT: ! r:
20040 ! ** Field Labels    **
20060   let ic=0 ! temporary Item Counter
20080   lbl$(ic+=1)="G/L"
20100   lbl$(ic+=1)="Date"
20120   lbl$(ic+=1)="Amount"
20140   lbl$(ic+=1)="TCode"
20160   lbl$(ic+=1)="PCode"
20180   lbl$(ic+=1)="Ref#"
20200   lbl$(ic+=1)="Description"
20220   lbl$(ic+=1)="Vendor"
20240 ! ** Text Box / Field Display   Lengths   **
20260   let ic=0 ! temporary Item Counter
20280   let mmddyy=8
20300   ccyymmdd=10
20320   let tln(ic+=1)=12
20340   let tln(ic+=1)=6
20360   let tln(ic+=1)=12
20380   let tln(ic+=1)=2
20400   let tln(ic+=1)=2
20420   let tln(ic+=1)=12
20440   let tln(ic+=1)=30
20460   let tln(ic+=1)=8
20480 ! ** Field Types **
20500   let ic=0
20520   fltyp$(ic+=1)='C'
20540   fltyp$(ic+=1)='N'
20560   fltyp$(ic+=1)='PD'
20580   fltyp$(ic+=1)='N'
20600   fltyp$(ic+=1)='N'
20620   fltyp$(ic+=1)='C'
20640   fltyp$(ic+=1)='C'
20660   fltyp$(ic+=1)='C'
20680 ! ** Field Storage Lengths **
20700   let ic=0
20720   let mmddyy=6 : ccyymmdd=8
20740   sln(ic+=1)=12
20760   sln(ic+=1)=6
20780   sln(ic+=1)=6.2
20800   sln(ic+=1)=2
20820   sln(ic+=1)=2
20840   sln(ic+=1)=12
20860   sln(ic+=1)=30
20880   sln(ic+=1)=8
20900 ! ** Field Masks **
20920   let ic=0
20940   let pointtwo=32 : let number=30
20960   ccyymmdd=3 : let mmddyy=1 : let glnumber=53
20980   let mask(ic+=1)=0
21000   let mask(ic+=1)=1
21020   let mask(ic+=1)=10 ! mask 10 is 2 decimals and commas
21040   let mask(ic+=1)=30
21060   let mask(ic+=1)=30
21080   let mask(ic+=1)=0
21100   let mask(ic+=1)=0
21120   let mask(ic+=1)=0
21140 ! ** Storage Positions **
21160 ! starting field position - default to the same as order displayed
21180   let ic=0
21200   sp(ic+=1)=1
21220   sp(ic+=1)=13
21240   sp(ic+=1)=19 ! 
21260   sp(ic+=1)=25
21280   sp(ic+=1)=27
21300   sp(ic+=1)=29
21320   sp(ic+=1)=41
21340   sp(ic+=1)=71
21360 ! ** Combo Boxes **
21380   cl=1 : c$(cl,1)='ComboF'
21400   c$(cl,2)=env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')
21420   c$(cl,3)="1" : c$(cl,4)="12"
21440   c$(cl,5)="13": c$(cl,6)="40"
21460   c$(cl,7)=env$('Q')&"\GLmstr\glindex.h"&env$('cno')
21480 ! C$(CL,8)=limit to list option ('1'=Yes; '0'=No)
21500   limit_to_list$='1'
21520   return  ! /r
