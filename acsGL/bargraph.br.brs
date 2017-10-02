00010 ! Replace S:\acsGL\bargraph
00020 ! pr bar graph of earnings by month for current year of prior year.
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnsearch,fnerror,fnUseDeptNo, fntos,fnlbl,fncmdset,fnacs,fntxt, fnqgl,fnagl$,fnopt,fnfra,fnpa_finis
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim acno$*12,bc(13),bp(13),wrd2$(2)*54,cap$*128,bud(13)
00080   dim resp$(10)*80,profit(12),txt$*80
00090   dim month(13), month$(13)*25,month$*25
00100 ! ______________________________________________________________________
00110   let right=1 : center=2
00120   let fntop(program$,cap$="Print Bar Graph of Earnings")
00140   open #20: "Name="&env$('Q')&"\GLmstr\Company.h"&env$('cno')&",Shr",internal,outin,relative  !:
        read #20,using 'form pos 296,pos 384,n 2': lmu,nap : close #20: 
00150 ! ______________________________________________________________________
00160   open #1: "Name="&env$('Q')&"\GLmstr\Period.h"&env$('cno')&",Version=1,KFName="&env$('Q')&"\GLmstr\Period-Idx.h"&env$('cno')&",Use,RecL=35,KPs=1,KLn=2,Shr",internal,outin,keyed 
00170 L170: read #1,using "form pos 1, n 2,c 25": month,month$ eof L210 norec L210
00180   if month<1 or month>13 then goto L170
00190   let month(month)=month !:
        let month$(month)=month$
00200   goto L170
00210 L210: close #1: 
00220 ! ______________________________________________________________________
00230   open #1: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\GLINDEX.h"&env$('cno')&",Shr",internal,outin,keyed 
00240   open #11: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\glIndx2.h"&env$('cno')&",Shr",internal,outin,keyed 
00250   open #12: "Name="&env$('Q')&"\GLmstr\BudgetInfo.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\BudIndx.h"&env$('cno')&",Use,RecL=28,KPs=1,KLn=14,Shr",internal,outin,keyed 
00260 SCR1: ! 
00270   let t5=0
00280   let fntos(sn$='CloseYear3') !:
        let lc=0 : let mylen=20 : let mypos=mylen+2 : let width=50
00290   let fnlbl(lc+=1,1,"Year to Print:",18,1)
00300   let fntxt(lc,mypos,4,0,1,"30",0,"You must choose the year to use printing a chart.") !:
        let resp$(1)=""
00310   let fnfra(lc+=1,1,2,45,"Current Year or Prior Year","Indicate if the information is to be pulled form the current files or prior files.",0) !:
        let fnopt(1,2,"Pull from Current",0,1) !:
        let fnopt(2,2,"Pull from Prior Year",0,1)
00320   let fnlbl(lc+=4,1,"Enter the Last Retained Earnings Account",width,0)
00330   let fnlbl(lc+=1,1,"or Equity Account:",width,0)
00340   let fnqgl(lc,mypos) !:
        let resp$(2)=""
00350   let fncmdset(2)
00360   let fnacs(sn$,0,mat resp$,ckey)
00370   if ckey=5 then goto XIT
00380   let year=val(resp$(1))
00390   if resp$(2)="True" then let pullfrom$="Current"
00400   if resp$(3)="True" then let pullfrom$="Prior"
00410   let glnumber$=fnagl$(resp$(4))
00420   read #1,using L430,key=glnumber$: dno$,ano$,sno$ nokey SCR1
00430 L430: form pos 1,c 3,c 6,c 3
00440   acno$=glnumber$(1:3)&"         "
00450 ! ______________________________________________________________________
00460   read #1,using L480,key>=glnumber$: acno$,bb,cb,mat bc,mat bp,mat bud nokey SCR1
00470 L470: read #1,using L480: acno$,bb,cb,mat bc,mat bp, mat bud eof PRINT_CHART
00480 L480: form pos 1,c 12,pos 81,41*pd 6.2
00490   if fnUseDeptNo=0 or dn1=1 then goto L510
00500   if glnumber$(1:3)><acno$(1:3) then goto PRINT_CHART ! may want to quit
00510 L510: ! If ACNO$><GLNUMBER$ Then Goto 830
00520   for j=1 to 12
00530 ! check no further down in current year past last month closed  KJ need to do
00540     if pullfrom$="Current" then goto L550 else goto L590
00550 L550: if j>lmu then goto L610 ! don't go past last month updated
00560     if j=1 then let profit(j)+=bc(j): goto L610 ! first month
00570     let profit(j)+=bc(j)-bc(j-1) ! 2nd thru 12 th months
00580     goto L610
00590 L590: if j=1 then let profit(j)+=bp(j): goto L610 ! first month prior year
00600     let profit(j)+=bp(j)-bp(j-1) ! 2nd thru 12 th months
00610 L610: next j
00620   goto L470
00630 PRINT_CHART: ! 
00640   gosub VBOPENPRINT
00650 ! determine maximum height and depth
00660   for j=1 to 12
00670     if profit(j)>0 then let maximumdepth=max(profit(j),maximumdepth) ! largest loss any one month either this year or last year
00680     if profit(j)<0 then let maximumheight=min(profit(j),maximumheight) ! largest profit by month for either year  (profit is negative figure
00690   next j
00700 ! determine top line and bottom line
00710   if maximumheight<-1000000 then let top=1000000 : let x=10000: goto DETERMINE_BOTTOM_LINE
00720   if maximumheight>-50000 and maximumheight<-10000 then let top=50000: let x=500: goto DETERMINE_BOTTOM_LINE
00730   if maximumheight<-10000 then let top=100000: let x=1000: goto DETERMINE_BOTTOM_LINE
00740   if maximumheight<-1000 then let top=10000: let x=100: goto DETERMINE_BOTTOM_LINE
00750   if maximumheight<-100 then let top=1000 : let x=10: goto DETERMINE_BOTTOM_LINE
00760 DETERMINE_BOTTOM_LINE: ! 
00770   if maximumdepth<1000 then bottom=-1000 : goto L800
00780   if maximumdepth<10000 then bottom=-10000 : goto L800
00790   if maximumdepth<100000 then bottom=-100000 : goto L800
00800 L800: let spacing=10 : let lyne=10
00810   cnam=(len(trim$(env$('cnam')))/2)+50
00820   pr #20: 'Call Print.MyFontsize(14)'
00830   pr #20: 'Call Print.AddText("'&env$('cnam')&'",'&str$(cnam)&','&str$(1)&')'
00840   pr #20: 'Call Print.MyFontsize(12)'
00850   let txt$="Earnings By Month " !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(80)&','&str$(5)&')'
00860   let txt$="For the Year "&str$(year) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(80)&','&str$(10)&')'
00870   pr #20: 'Call Print.MyFontsize(9)'
00877 ! pr #20: 'Call Print.AddText("'&month$(1)&'",'&STR$(10)&','&STR$(11)&')'
00880   for j=1 to 12
00890     let txt$=trim$(month$(j))(1:3) !:
          let indent=8+(10*j) !:
          pr #20: 'Call Print.AddText("'&txt$&'",'&str$(indent)&','&str$(15)&')'
00900   next j
00910   let txt$=cnvrt$("pic(--------)",top) !:
        pr #20: 'Call Print.AddText('&txt$&','&str$(1)&','&str$(lyne+=spacing)&')'
00920   pr #20: 'Call Print.AddLine('&str$(15)&','&str$(lyne)&',200,0)' ! left,up/down,lenght of top line on chart
00930   for j=1 to 9
00940     let txt$=cnvrt$("pic(--------)",top-((.10*j)*top)) !:
          pr #20: 'Call Print.AddText('&txt$&','&str$(1)&','&str$(lyne+=spacing)&')'
00950     pr #20: 'Call Print.AddLine('&str$(15)&','&str$(lyne)&',200,0)' ! left,up/down,lenght of top line on chart
00960   next j
00970   goto L1140
00980   let txt$=cnvrt$("pic(--------)",top*.80) !:
        pr #20: 'Call Print.AddText('&txt$&','&str$(1)&','&str$(lyne+=spacing)&')'
00990   pr #20: 'Call Print.AddLine('&str$(15)&','&str$(lyne)&',200,0)' ! left,up/down,lenght of top line on chart
01000   let txt$=cnvrt$("pic(--------)",top*.70) !:
        pr #20: 'Call Print.AddText('&txt$&','&str$(1)&','&str$(lyne+=spacing)&')'
01010   pr #20: 'Call Print.AddLine('&str$(15)&','&str$(lyne)&',200,0)' ! left,up/down,lenght of top line on chart
01020   let txt$=cnvrt$("pic(--------)",top*.60) !:
        pr #20: 'Call Print.AddText('&txt$&','&str$(1)&','&str$(lyne+=spacing)&')'
01030   pr #20: 'Call Print.AddLine('&str$(15)&','&str$(lyne)&',200,0)' ! left,up/down,lenght of top line on chart
01040   let txt$=cnvrt$("pic(--------)",top*.50) !:
        pr #20: 'Call Print.AddText('&txt$&','&str$(1)&','&str$(lyne+=spacing)&')'
01050   pr #20: 'Call Print.AddLine('&str$(15)&','&str$(lyne)&',200,0)' ! left,up/down,lenght of top line on chart
01060   let txt$=cnvrt$("pic(--------)",top*.40) !:
        pr #20: 'Call Print.AddText('&txt$&','&str$(1)&','&str$(lyne+=spacing)&')'
01070   pr #20: 'Call Print.AddLine('&str$(15)&','&str$(lyne)&',200,0)' ! left,up/down,lenght of top line on chart
01080   let txt$=cnvrt$("pic(--------)",top*.30) !:
        pr #20: 'Call Print.AddText('&txt$&','&str$(1)&','&str$(lyne+=spacing)&')'
01090   pr #20: 'Call Print.AddLine('&str$(15)&','&str$(lyne)&',200,0)' ! left,up/down,lenght of top line on chart
01100   let txt$=cnvrt$("pic(--------)",top*.20) !:
        pr #20: 'Call Print.AddText('&txt$&','&str$(1)&','&str$(lyne+=spacing)&')'
01110   pr #20: 'Call Print.AddLine('&str$(15)&','&str$(lyne)&',200,0)' ! left,up/down,lenght of top line on chart
01120   let txt$=cnvrt$("pic(--------)",top*.10) !:
        pr #20: 'Call Print.AddText('&txt$&','&str$(1)&','&str$(lyne+=spacing)&')'
01130   pr #20: 'Call Print.AddLine('&str$(15)&','&str$(lyne)&',200,0)' ! left,up/down,lenght of top line on chart
01140 L1140: ! zero line starts right here
01142   for j=1 to 12
01143     let txt$=trim$(month$(j))(1:3) !:
          let indent=8+(10*j) !:
          pr #20: 'Call Print.AddText("'&txt$&'",'&str$(indent)&','&str$(120)&')'
01144   next j
01150   let txt$=cnvrt$("pic(-------#)",0) !:
        pr #20: 'Call Print.AddText('&txt$&','&str$(1)&','&str$(lyne+=spacing)&')'
01160   pr #20: 'Call Print.AddLine('&str$(15)&','&str$(lyne)&',200,0)' ! left,up/down,lenght of top line on chart
01170   for j=1 to 10
01180     let txt$=cnvrt$("pic(-------#)",-top*(.10*j)) !:
          pr #20: 'Call Print.AddText('&txt$&','&str$(1)&','&str$(lyne+=spacing)&')'
01190     pr #20: 'Call Print.AddLine('&str$(15)&','&str$(lyne)&',200,0)' ! left,up/down,lenght of top line on chart
01200   next j
01210   let linezero=(spacing*10)+20
01220   column=18 ! spacing sideways
01230   pr #20: 'Call Print.MyFontBold(1)'
01240   for j=1 to 12
01250     let homedot=linezero+(profit(j)/x)
01260 ! never could make the line chart work!! kj
01270 ! If PROFIT(J+1)<=0 Then Let LINELENGTH=SQR((10*10)+(ABS(PROFIT(J+1)/X)-LINEZERO+HOMEDOT)*(ABS(PROFIT(J+1)/X)-LINEZERO+HOMEDOT)) ! profit
01280 ! If PROFIT(J+1)>0 Then Let LINELENGTH=SQR((10*10)+(ABS(PROFIT(J+1)/X)+LINEZERO)*(ABS(PROFIT(J+1)/X)+LINEZERO)) ! loss
01290 ! Let NEXTDOT=LINEZERO+(PROFIT(J+1)/X)-HOMEDOT
01300 ! \If NEXTDOT=0 Then Let NEXTDOT=1
01310     if homedot<10 then let homedot=10
01320     if homedot>220 then let homedot=220
01330     pr #20: 'Call Print.AddLine('&str$(column-1)&','&str$(homedot)&','&str$(7)&','&str$(linezero-homedot)&',1)'
01340     for q=1 to 6
01350       pr #20: 'Call Print.AddLine('&str$(column-1+q)&','&str$(homedot)&','&str$(7-q)&','&str$(linezero-homedot)&',1)'
01360     next q
01370     pr #20: 'Call Print.MyFontsize(6)'
01380     if profit(j)<0 then let txt$=cnvrt$("pic(--------#)",-round(profit(j),0)): pr #20: 'Call Print.AddText('&txt$&','&str$(column)&','&str$(homedot-3)&')'
01390     if profit(j)>0 then let txt$=cnvrt$("pic(--------#)",-round(profit(j),0)): pr #20: 'Call Print.AddText('&txt$&','&str$(column)&','&str$(homedot+2)&')'
01400     pr #20: 'Call Print.MyFontsize(9)'
01410     column+=10
01420   next j
01421   for j=1 to nap
01422     let txt$=trim$(month$(j))(1:3) !:
          let indent=8+(10*j) !:
          pr #20: 'Call Print.AddText("'&txt$&'",'&str$(indent)&','&str$(230)&')'
01423   next j
01430   gosub RELEASE_PRINT
01450   close #1: 
01460   goto XIT
01470 ! ______________________________________________________________________
01480 XIT: let fnxit
01490 ! ______________________________________________________________________
01500 ! <Updateable Region: ERTN>
01510 ERTN: let fnerror(program$,err,line,act$,"xit")
01520   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01530   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01540   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01550 ERTN_EXEC_ACT: execute act$ : goto ERTN
01560 ! /region
01570 ! ______________________________________________________________________
01580 VBOPENPRINT: ! 
01590   if file(20)=-1 then 
01600     open #20: "Name="&env$('Q')&"\GLmstr\linechart"&wsid$&".txt,Replace,RecL=5000",display,output 
01610     pr #20: 'Call Print.MyOrientation("Portrait")'
01620     let lyne=margin ! starting of 1st line
01630     column1=16 !:
          column2=103 !:
          column3=153
01640   end if 
01650   return 
01660 RELEASE_PRINT: ! 
01680   let fnpa_finis
01700   return 
