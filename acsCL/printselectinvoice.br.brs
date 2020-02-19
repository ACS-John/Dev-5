00010 ! Replace S:\acsCL\PrintSelectInvoice
00020 ! unpaid invoice file
00030 !
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fndat,fnopenprn,fncloseprn,fnchain,fnerror,fntop,fnxit,fndate_mmddyy_to_ccyymmdd,fngethandle
00050   on error goto Ertn
00060 !
00070   dim cap$*128,cnam$*40,newkey$*20
00080   dim de$*30,gl(3),t1(5),sc3$(5)*55,up$(4),d(2),nam$*30
00090 !
00100   fntop(program$,cap$="Print Selected Invoice Listing")
00110   cancel=99
00120   fncno(cno,cnam$)
00130   open #20: "Name=[Q]\CLmstr\PostDat.H[cno],Shr,Use,RecL=12",internal,outIn,relative 
00140   read #20,using 'Form POS 1,2*N 6',rec=1: dt1,dt2 noRec L150 !:
        goto L160
00150 L150: write #20,using 'Form POS 1,2*N 6',rec=1: dt1,dt2
00160 L160: close #20: 
00170   open #20: "Name=[Q]\CLmstr\Company.h[cno],Shr",internal,input,relative  !:
        read #20,using 'Form POS 150,2*N 1,C 2',rec=1: mat d,bc$ !:
        close #20: 
00180   bankcode=val(bc$)
00190   open #bankmstr=12: "Name=[Q]\CLmstr\BankMstr.H[cno],KFName=[Q]\CLmstr\BankIdx1.H[cno],Shr",internal, outin, keyed 
00200   open #paymstr1:=fngethandle: "Name=[Q]\CLmstr\PayMstr.H[cno],KFName=[Q]\CLmstr\PayIdx1.H[cno],Shr",internal,outIn,keyed 
00210   open #paytrans:=fngethandle: "Name=[Q]\CLmstr\PayTrans.H[cno],Version=2,KFName=[Q]\CLmstr\UnPdIdx1.H[cno],Shr",internal,outIn,keyed 
00220   open #unpdaloc=5: "Name=[Q]\CLmstr\UnPdAloc.H[cno],KFName=[Q]\CLmstr\uaidx2.H[cno],Shr",internal,outIn,keyed 
00230   sc3$(2)=" Total Invoices selected for payment-Bank " !:
        sc3$(2)=sc3$(2)&ltrm$(str$(bankcode))&":" !:
        sc3$(3)=" Balance after paying selected Invoices:" !:
        sc3$(4)=" Total Invoices not selected for payment from Bank " !:
        sc3$(4)=sc3$(4)&rtrm$(str$(bankcode))&":" !:
        sc3$(5)=" Total of all unpaid Invoices (less credits) on file:"
00240   read #bankmstr,using 'Form Pos 45,PD 6.2,PD 6.2,G 8',key=bc$,release: bal,upi,lcn$ nokey L250
00250 L250: sc3$(1)=" Bank Code "&bc$&" Current Bank Balance:"
00260   t1(1)=bal
00270   fnopenprn
00280   restore #paytrans,key>="                    ": nokey EO_PAYTRANS !:
        hvn$="" : t1=pg1=0
00290   gosub HEADER
00300 READ_PAYTRANS: ! 
00310   read #paytrans,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,N 1,N 2,G 8,G 6,N 1': vn$,iv$,mat up$,upa,pcde,bc,ck$,dp,gde eof EO_PAYTRANS !:
        bank_code=bc
00320   if rtrm$(vn$)="" or pcde><1 or bc<>bankcode then t1(4)+=upa: goto READ_PAYTRANS ! reject and add invoices not selected or for other bank accounts
00330   if hvn$<>"" and hvn$><vn$ then gosub READ_AND_PRINT
00340   gosub BREAKDOWN
00350   t1+=upa : hvn$=vn$ : t1(2)+=upa
00360   goto READ_PAYTRANS
00370 !
00380 NEWPGE: pr #255: newpage: gosub HDR : continue 
00390 !
00400 HDR: ! 
00410   pr #255,using 'Form POS 1,C 8,Cc 82': date$,cnam$
00420   pr #255,using 'Form POS 1,C 4,N 4,POS 36,C 40': "Page",pg+=1,"Unpaid Invoice File Listing" !:
        pr #255: ""
00430   pr #255: "                             Invoice    Due     PO Number                                   Pay   Bank   Check     Date "
00440   pr #255: "Ref#  Payee #   Invoice Numb   Date    Date     GL Number   Description            Amount   Code  Code   Number    Paid "
00450   pr #255: "____  ________  ____________  ______  ______  ____________  __________________  __________  ____  ____  ________  ______"
00460   return 
00470 !
00480 DONE: ! 
00490   close #bankmstr: 
00500   close #paymstr1: 
00510   close #paytrans: 
00520   goto XIT
00530 !
00540 XIT: fnxit
00550 !
00560 PGOF: pr #255: newpage : gosub HEADER : continue 
00570 !
00580 HEADER: ! 
00590   pr #255,using 'Form POS 1,PIC(ZZ/ZZ/ZZ),CC 107': prd,cnam$
00600   pr #255,using 'Form POS 1,C 4,N 4,CC 107,SKIP 1,POS 1,C 8': "Page",pg1+=1,"Selected Invoice Listing for Bank "&ltrm$(str$(bankcode)),date$
00610   pr #255: "  Payee                  Invoice     Due                                                   Line Item                       Item"
00620   pr #255: " Number   Invoice Numb    Date      Date    Inv. Description       Amount     GL Number    Description                    Amount"
00630   pr #255: "________  ____________  ________  ________  __________________  ____________  ____________ ______________________________ __________"
00640   return 
00650 !
00660 READ_AND_PRINT: ! 
00670   nam$="" !:
        read #paymstr1,using 'Form POS 9,C 30',key=hvn$,release: nam$ nokey L680
00680 L680: pr #255,using 'Form POS 65,C 14,SKIP 1,POS 30,C 33,PIC(ZZZ,ZZZ,ZZZ.##CR)': "____________",nam$,t1 pageoflow PGOF !:
        pr #255: "" pageoflow PGOF
00690   t1=0
00700   return 
00710 !
00720 EO_PAYTRANS: ! 
00730   if hvn$<>"" then gosub READ_AND_PRINT
00740   t1(3)=t1(1)-t1(2): t1(5)=t1(4)+t1(2)
00750   for j=1 to 5 !:
          pr #255,using 'Form POS 5,C 55,PIC($$$$,$$$,$$$,$$$.##CR)': sc3$(j),t1(j) !:
        next j
00760   fncloseprn
00770   on fkey 99 ignore 
00780   goto XIT
00790 !
00800 BREAKDOWN: ! 
00810 ! 
00820   startxx=0
00830   restore #unpdaloc,key>=vn$&iv$: nokey EO_UNPDALOC
00840 L840: read #unpdaloc,using 'Form pos 1,c 20,Pos 21,N 3,N 6,N 3,PD 5.2,C 30': newkey$,mat gl,aa,de$ eof EO_UNPDALOC nokey EO_UNPDALOC !:
        if newkey$<>vn$ & iv$ then goto EO_UNPDALOC
00850   if sum(gl)=0 and aa=0 then goto L840 ! don't allow zero allocations to print
00860   if startxx=0 then !:
          pr #255,using 'Form POS 1,C 10,C 12,2*PIC(ZZZZ/ZZ/ZZ),X 2,C 18,PIC(ZZZ,ZZZ,ZZZ.##CR),POS 79,N 3,N 6,N 3,X 1,C 30,X 1,PIC(---,--#.##)': vn$,iv$,val(up$(1)),val(up$(2)),up$(4),upa,mat gl, de$, aa pageoflow PGOF
00870   if startxx>0 then !:
          pr #255,using 'Form POS 79,N 3,N 6,N 3,X 1,C 30,X 1,PIC(---,--#.##)': mat gl, de$, aa
00880   startxx+=1
00890   goto L840
00900 EO_UNPDALOC: ! 
00910   if startxx=0 then !:
          pr #255,using 'Form POS 1,C 10,C 12,2*PIC(ZZZZ/ZZ/ZZ),X 2,C 18,PIC(ZZZ,ZZZ,ZZZ.##CR),POS 79,N 3,N 6,N 3,X 1,C 30,X 1,PIC(---,--#.##)': vn$,iv$,val(up$(1)),val(up$(2)),up$(4),upa,mat gl, de$, aa pageoflow PGOF !:
          ! only on noRec if startxx=0 (no allocations)
00920   return 
00930 !
00940 ! <Updateable Region: ERTN>
00950 ERTN: fnerror(program$,err,line,act$,"xit")
00960   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00970   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00980   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00990 ERTN_EXEC_ACT: execute act$ : goto ERTN
01000 ! /region
01010 !
