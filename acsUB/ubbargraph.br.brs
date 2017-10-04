00010 ! Replace S:\acsUB\ubbargraph
00020 ! pr bar graph of earnings by month for current year of prior year.
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnsearch,fnerror,fncno, fntos,fnlbl,fncmdset,fnacs,fntxt, fnqgl,fnagl$,fnopt,fnfra,fncomboa,fndat,fnmsgbox,fndate_mmddyy_to_ccyymmdd,fnpa_finis
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim acno$*12,bc(13),bp(13),wrd2$(2)*54,cap$*128,bud(13)
00080   dim month(13), month$(24)*25,month$*25,actualdate$(24)
00090   let right=1 : center=2
00100   fntop(program$,cap$="Bar Graph")
00110   fncno(cno,cnam$)
00120   dim cd1(24),rw(8,13),e$*30,u1(24),u2(24),u3(24,13),message$*60
00130   dim n2(24),n3(24,13),cap$*128,cnam$*40,resp$(27),txt$*80
00140   dim servicename$(10)*20,dat$*20,msgline$(2)*40,tg(11),opt$(3)*20
00150   dim srv$(10)*2,dollars(24)
00160 ! ______________________________________________________________________
00170   fncno(cno,cnam$)
00180   fndat(dat$,1)
00190 ! 
00200   fntop("S:\acsUB\UBbargraph",cap$="Bar Graph")
00210   open #1: "Name="&env$('Q')&"\UBmstr\Company.h"&str$(cno),internal,input  !:
        read #1,using "Form POS 121,N 6": d1 ioerr L230 !:
        close #1: 
00220   let magicdate=fndate_mmddyy_to_ccyymmdd(d1)-20000 ! don't start with anything older that two years ago
00230 L230: open #20: "Name="&env$('Q')&"\UBmstr\ubData\Service.h"&str$(cno)&",Shr",internal,input,relative  !:
        read #20,using "Form POS 1,10*C 20,10*c 2",rec=1: mat servicename$,mat srv$ !:
        close #20: 
00240   open #2: "Name="&env$('Q')&"\UBmstr\UBTransVB.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\UBTrIndx.h"&str$(cno)&",Shr",internal,input,keyed 
00250   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,outin,keyed 
00260 L260: read #1,using L990,release: z$,e$,bildat eof SCREEN1
00265   if bildat<>d1 then goto L260 ! current customer
00270   restore #2,key>=z$&"         ": nokey L260
00280 L280: read #2,using L1040: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof SCREEN1
00290   if p$<>z$ then goto L350 !:
          ! history record must belong to this customer
00300   if tcode<>1 then goto L280 ! charge transaction
00310   if tdate<magicdate then goto L280
00320   let j=j+1 !:
        if j>24 then goto SCREEN1
00330   let resp$(j)=str$(tdate)
00340   goto L280
00350 L350: if resp$(12)="" then goto L260 ! try another customer
00360 ! ______________________________________________________________________
00370 SCREEN1: ! 
00380   restore #1: 
00390   fntos(sn$="ubbargraph") !:
        let rc=0
00400   fnlbl(1,1,"Billing dates to be used:",35,1)
00410   fntxt(2,1,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        let rc+=1 : let resp$(rc)=resp$(rc)
00420   fntxt(2,15,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        let rc+=1 : let resp$(rc)=resp$(rc)
00430   fntxt(2,29,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        let rc+=1 : let resp$(rc)=resp$(rc)
00440   fntxt(2,43,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        let rc+=1 : let resp$(rc)=resp$(rc)
00450   fntxt(2,57,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        let rc+=1 : let resp$(rc)=resp$(rc)
00460   fntxt(2,71,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        let rc+=1 : let resp$(rc)=resp$(rc)
00470   fntxt(4,1,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        let rc+=1 : let resp$(rc)=resp$(rc)
00480   fntxt(4,15,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        let rc+=1 : let resp$(rc)=resp$(rc)
00490   fntxt(4,29,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        let rc+=1 : let resp$(rc)=resp$(rc)
00500   fntxt(4,43,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        let rc+=1 : let resp$(rc)=resp$(rc)
00510   fntxt(4,57,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        let rc+=1 : let resp$(rc)=resp$(rc)
00520   fntxt(4,71,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        let rc+=1 : let resp$(rc)=resp$(rc)
00530   fntxt(6,1,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        let rc+=1 : let resp$(rc)=resp$(rc)
00540   fntxt(6,15,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        let rc+=1 : let resp$(rc)=resp$(rc)
00550   fntxt(6,29,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        let rc+=1 : let resp$(rc)=resp$(rc)
00560   fntxt(6,43,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        let rc+=1 : let resp$(rc)=resp$(rc)
00570   fntxt(6,57,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        let rc+=1 : let resp$(rc)=resp$(rc)
00580   fntxt(6,71,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        let rc+=1 : let resp$(rc)=resp$(rc)
00590   fntxt(8,1,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        let rc+=1 : let resp$(rc)=resp$(rc)
00600   fntxt(8,15,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        let rc+=1 : let resp$(rc)=resp$(rc)
00610   fntxt(8,29,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        let rc+=1 : let resp$(rc)=resp$(rc)
00620   fntxt(8,43,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        let rc+=1 : let resp$(rc)=resp$(rc)
00630   fntxt(8,57,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        let rc+=1 : let resp$(rc)=resp$(rc)
00640   fntxt(8,71,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        let rc+=1 : let resp$(rc)=resp$(rc)
00650   fnlbl(10,1,"Service to Analyze:",24,1,0)
00660   let opt$(1)="Water"
00670   if srv$(3)="EL" then let opt$(2)= servicename$(3)
00680   if srv$(4)="GA" then let opt$(3)= servicename$(4)
00690   fncomboa("ubbargraph",10,26,mat opt$,"",13) !:
        let rc+=1 : let resp$(rc)=opt$(1)
00700   fnfra(12,1,2,45,"Base graph on usage or dollars","You can either analyze dollars or usage.",0) !:
        fnopt(1,2,"Use Usage",0,1) !:
        let resp$(rc+=1)="True" !:
        fnopt(2,2,"Use Dollars",0,1)
00710   fncmdset(2) !:
        fnacs(sn$,0,mat resp$,ckey)
00720   if ckey=5 then goto XIT
00730   for j=1 to 24
00740 L740: let x=pos(resp$(j),"/",1) !:
          if x>0 then let resp$(j)(x:x)="": goto L740
00750     cd1(j)=val(resp$(j)) conv MSGBOX
00760     let y=val(resp$(j)(5:6))
00770     if y=1 then let month$(j)="Jan"
00780     if y=2 then let month$(j)="Feb"
00790     if y=3 then let month$(j)="Mar"
00800     if y=4 then let month$(j)="Apr"
00810     if y=5 then let month$(j)="May"
00820     if y=6 then let month$(j)="Jun"
00830     if y=7 then let month$(j)="Jul"
00840     if y=8 then let month$(j)="Aug"
00850     if y=9 then let month$(j)="Sep"
00860     if y=10 then let month$(j)="Oct"
00870     if y=11 then let month$(j)="Nov"
00880     if y=12 then let month$(j)="Dec"
00890   next j
00900   if cd1(1)=0 then goto MSGBOX
00910   if resp$(25)="Water" then codepos=143: let service=1: let opt=1
00920   if resp$(25)=trim$(opt$(2)) then codepos=147: let service=3: let opt=2
00930   if resp$(25)=trim$(opt$(3)) then codepos=149 : let service=4 : let opt=3
00940   if resp$(26)="True" then baseon=1 else baseon =2 ! 1=usage  2=dollars
00950   for j=1 to 24
00960     actualdate$(j)=resp$(j)
00970   next j
00980 L980: read #1,using L990: z$,e$,servicecode eof STORE_GRAPH_INFO
00990 L990: form pos 1,c 10,x 30,c 30,pos 296,pd 4
01000   restore #2,key>=z$&"         ": nokey L980
01010 L1010: read #2,using L1040: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof STORE_GRAPH_INFO
01020   if p$<>z$ then goto L980 ! history record must belong to this customer
01030   if tcode<>1 then goto L1010 ! charge transactions only
01040 L1040: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
01050 ! ______________________________________________________________________
01060   if service=1 and baseon=1 then let usage=wu ! analyzing water
01070   if service=1 and baseon=2 then let usage=tg(1) ! analyzing water dollars
01080   if service=3 and baseon=1 then let usage=eu ! analyzing electric
01090   if service=3 and baseon=2 then let usage=tg(3) ! analyzing electric dollars
01100   if service=4 and baseon=1 then let usage=gu ! analyzing gas
01110   if service=4 and baseon=2 then let usage=tg(4) ! analyzing gas dollars
01120   for j=1 to 24
01130     if cd1(j)><tdate then goto L1150
01140     let n2(j)=n2(j)+1 !:
          let u1(j)=u1(j)+usage !:
          let u2(j)=u2(j)+usage
01150 L1150: next j
01160   goto L1010 ! read next transaction
01170 STORE_GRAPH_INFO: ! 
01180   for j=1 to 24
01190     let dollars(j)=u1(j)
01200   next j
01210 PRINT_CHART: ! 
01220   gosub VBOPENPRINT
01230 ! determine maximum height and depth
01240   for j=1 to 24
01250     if dollars(j)>0 then let maximumheight=max(dollars(j),maximumheight) ! largest dollars by month for either year  (dollars is negative figure
01260   next j
01270 ! determine top line and bottom line
01280   if baseon=1 then let top$=str$(maximumheight): let toplen=len(top$): let top=toplen*10
01290   if baseon=2 then let top$=str$(round(maximumheight,0)): let toplen=len(top$): let top=toplen*10
01300   let toplen$=str$(val(top$(1:1))+1)
01310   for j=1 to toplen-1
01320     let toplen$=toplen$&str$(0)
01330   next j
01340   let top=val(toplen$)
01350   let x=top*.10
01360 DETERMINE_BOTTOM_LINE: ! 
01370   let spacing=10 : let lyne=30
01380   cnam=(len(trim$(cnam$))/2)+110
01390   pr #20: 'Call Print.MyFontsize(14)'
01400   let txt$=cnam$ !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(cnam)&','&str$(10)&')'
01410   pr #20: 'Call Print.MyFontsize(12)'
01420   if baseon=1 then let txt$="Usage By Month"
01430   if baseon=2 then let txt$="Dollars By Month"
01440   let servicetype=(len(trim$(txt$))/2)+120
01450   pr #20: 'Call Print.AddText("'&txt$&'",'&str$(servicetype)&','&str$(16)&')'
01460   if baseon=1 then let txt$=trim$(opt$(opt))
01470   if baseon=2 then let txt$=trim$(opt$(opt))
01480   let servicetype=(len(trim$(txt$))/2)+140
01490   pr #20: 'Call Print.AddText("'&txt$&'",'&str$(servicetype)&','&str$(20)&')'
01500   pr #20: 'Call Print.MyFontsize(9)'
01510 ! For J=1 To 24 ! pr month names across top
01520 ! Let TXT$=TRIM$(MONTH$(J))(1:3) !:
        ! Let INDENT=8+(10*J) !:
        ! pr #20: 'Call Print.AddText("'&TXT$&'",'&STR$(INDENT)&','&STR$(25)&')'
01530 ! Next J
01540   let txt$=cnvrt$("pic(--------)",top) !:
        pr #20: 'Call Print.AddText('&txt$&','&str$(1)&','&str$(lyne+=spacing)&')'
01550   pr #20: 'Call Print.AddLine('&str$(15)&','&str$(lyne)&',240,0)' ! left,up/down,lenght of top line on chart
01560   for j=1 to 10 ! wording down side
01570     let txt$=cnvrt$("pic(-------#)",top-((.10*j)*top)) !:
          pr #20: 'Call Print.AddText('&txt$&','&str$(1)&','&str$(lyne+=spacing)&')'
01580     pr #20: 'Call Print.AddLine('&str$(15)&','&str$(lyne)&',240,0)'
01590   next j
01600 ! zero line starts right here
01610   let linezero=(spacing*10)+40
01620   column=18 ! spacing sideways
01630   pr #20: 'Call Print.MyFontBold(1)'
01640   for j=1 to 24
01650     let homedot=140-((dollars(j)/top)*100)
01660     if homedot<0 then let homedot=0
01670     if homedot>140 then let homedot=140
01680     pr #20: 'Call Print.AddLine('&str$(column-1)&','&str$(homedot)&','&str$(7)&','&str$(linezero-homedot)&',1)'
01690     for q=1 to 6
01700       pr #20: 'Call Print.AddLine('&str$(column-1+q)&','&str$(homedot)&','&str$(7-q)&','&str$(linezero-homedot)&',1)'
01710     next q
01720     pr #20: 'Call Print.MyFontsize(6)'
01730     if dollars(j)>0 then let txt$=cnvrt$("pic(--------#)",round(dollars(j),0)): pr #20: 'Call Print.AddText('&txt$&','&str$(column-2)&','&str$(homedot-2)&')'
01740     pr #20: 'Call Print.MyFontsize(9)'
01750     column+=10
01760   next j
01770   pr #20: 'Call Print.MyFontBold(0)'
01780   for j=1 to 24 ! month wording at bottom of page
01790     let txt$=trim$(month$(j))(1:3) !:
          let indent=8+(10*j) !:
          pr #20: 'Call Print.AddText("'&txt$&'",'&str$(indent)&','&str$(linezero+5)&')'
01800     let txt$=actualdate$(j)(7:8)
01810     pr #20: 'Call Print.AddText("'&txt$&'",'&str$(indent+1)&','&str$(linezero+8)&')'
01820     let txt$=actualdate$(j)(3:4)
01830     pr #20: 'Call Print.AddText("'&txt$&'",'&str$(indent+1)&','&str$(linezero+11)&')'
01840   next j
01850   gosub RELEASE_PRINT
01860   close #1: 
01870   goto XIT
01880 ! ______________________________________________________________________
01890 XIT: let fnxit
01900 ! ______________________________________________________________________
01910 ! <Updateable Region: ERTN>
01920 ERTN: let fnerror(program$,err,line,act$,"xit")
01930   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01940   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01950   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01960 ERTN_EXEC_ACT: execute act$ : goto ERTN
01970 ! /region
01980 ! ______________________________________________________________________
01990 VBOPENPRINT: ! 
02000   if file(20)=-1 then 
02010     open #20: "Name="&env$('Q')&"\UBmstr\linechart"&wsid$&".txt,Replace,RecL=5000",display,output 
02020     pr #20: 'Call Print.MyOrientation("Landscape")'
02030     let lyne=margin ! starting of 1st line
02040     column1=16 !:
          column2=103 !:
          column3=153
02050   end if 
02060   return 
02070 RELEASE_PRINT: ! 
02090   fnpa_finis
02110   return 
02120 MSGBOX: ! !:
        let msgline$(1)="You have entered dates in an" !:
        let msgline$(2)="invalid format.  Use mmddyy format." !:
        fnmsgbox(mat msgline$,resp$,cap$,1) !:
        goto SCREEN1
