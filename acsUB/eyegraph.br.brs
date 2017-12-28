00020 ! pr bar graph of eye improvement
00030 ! ______________________________________________________________________Designated SPEC file does not exist.
00040   library 'S:\Core\Library': fntop,fnxit, fnsearch,fnerror,fncno, fnTos,fnLbl,fnCmdSet,fnAcs,fnTxt, fnqgl,fnagl$,fnOpt,fnFra,fncomboa,fndat,fnmsgbox,fndate_mmddyy_to_ccyymmdd,fnpa_finis
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim acno$*12,bc(13),bp(13),wrd2$(2)*54,cap$*128,bud(13)
00080   dim month(13), month$(24)*25,month$*25,actualdate$(24)
00090   right=1 : center=2
00100   fntop(program$,cap$="Bar Graph")
00110   fncno(cno,cnam$)
00120   dim cd1(24),rw(8,13),e$*30,u1(24),u2(24),u3(24,13),message$*60
00130   dim n2(24),n3(24,13),cap$*128,cnam$*40,resp$(27),txt$*80
00140   dim serviceName$(10)*20,dat$*20,msgline$(2)*40,tg(11),opt$(3)*20
00150   dim srv$(10)*2,dollars(24)
00160 ! ______________________________________________________________________
00170   fncno(cno,cnam$)
00180   fndat(dat$,1)
00190 ! 
00200   fntop("S:\acsUB\UBbargraph",cap$="Bar Graph")
00220   magicdate=fndate_mmddyy_to_ccyymmdd(d1)-20000 ! don't start with anything older that two years ago
00235   goto SCREEN1
00240   open #2: "Name="&env$('Q')&"\UBmstr\UBTransVB.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\UBTrIndx.h"&env$('cno')&",Shr",internal,input,keyed 
00250   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,outIn,keyed 
00260 L260: read #1,using L1050,release: z$,e$,bildat eof SCREEN1
00270   if bildat<>d1 then goto L260 ! current customer
00280   restore #2,key>=z$&"         ": nokey L260
00290 L290: read #2,using L1100: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof SCREEN1
00300   if p$<>z$ then goto L360 !:
          ! history record must belong to this customer
00310   if tcode<>1 then goto L290 ! charge transaction
00320   if tdate<magicdate then goto L290
00330   j=j+1 !:
        if j>24 then goto SCREEN1
00340   resp$(j)=str$(tdate)
00350   goto L290
00360 L360: if resp$(12)="" then goto L260 ! try another customer
00370 ! ______________________________________________________________________
00380 SCREEN1: ! 
00381 ! add dates here kj
00385   mat resp$=("")
00390   resp$(1)="20110112"
00400   resp$(2)="20110126"
00410   resp$(3)="20110131"
00420   resp$(4)="20110201"
00430   resp$(5)="20110202"
00436   resp$(6)="20110207"
00437   resp$(7)="20110209"
00438   resp$(8)="20110215"
00450   fnTos(sn$="ubbargraph") !:
        rc=0
00460   fnLbl(1,1,"Billing dates to be used:",35,1)
00470   fnTxt(2,1,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        rc+=1 : resp$(rc)=resp$(rc)
00480   fnTxt(2,15,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        rc+=1 : resp$(rc)=resp$(rc)
00490   fnTxt(2,29,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        rc+=1 : resp$(rc)=resp$(rc)
00500   fnTxt(2,43,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        rc+=1 : resp$(rc)=resp$(rc)
00510   fnTxt(2,57,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        rc+=1 : resp$(rc)=resp$(rc)
00520   fnTxt(2,71,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        rc+=1 : resp$(rc)=resp$(rc)
00530   fnTxt(4,1,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        rc+=1 : resp$(rc)=resp$(rc)
00540   fnTxt(4,15,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        rc+=1 : resp$(rc)=resp$(rc)
00550   fnTxt(4,29,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        rc+=1 : resp$(rc)=resp$(rc)
00560   fnTxt(4,43,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        rc+=1 : resp$(rc)=resp$(rc)
00570   fnTxt(4,57,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        rc+=1 : resp$(rc)=resp$(rc)
00580   fnTxt(4,71,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        rc+=1 : resp$(rc)=resp$(rc)
00590   fnTxt(6,1,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        rc+=1 : resp$(rc)=resp$(rc)
00600   fnTxt(6,15,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        rc+=1 : resp$(rc)=resp$(rc)
00610   fnTxt(6,29,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        rc+=1 : resp$(rc)=resp$(rc)
00620   fnTxt(6,43,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        rc+=1 : resp$(rc)=resp$(rc)
00630   fnTxt(6,57,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        rc+=1 : resp$(rc)=resp$(rc)
00640   fnTxt(6,71,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        rc+=1 : resp$(rc)=resp$(rc)
00650   fnTxt(8,1,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        rc+=1 : resp$(rc)=resp$(rc)
00660   fnTxt(8,15,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        rc+=1 : resp$(rc)=resp$(rc)
00670   fnTxt(8,29,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        rc+=1 : resp$(rc)=resp$(rc)
00680   fnTxt(8,43,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        rc+=1 : resp$(rc)=resp$(rc)
00690   fnTxt(8,57,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        rc+=1 : resp$(rc)=resp$(rc)
00700   fnTxt(8,71,10,10,0,"3",0,"Select up to 24 billing dates to use in the graph.") !:
        rc+=1 : resp$(rc)=resp$(rc)
00710   fnLbl(10,1,"Service to Analyze:",24,1,0)
00720   opt$(1)="Water"
00730   if srv$(3)="EL" then opt$(2)= serviceName$(3)
00740   if srv$(4)="GA" then opt$(3)= serviceName$(4)
00750   fncomboa("ubbargraph",10,26,mat opt$,"",13) !:
        rc+=1 : resp$(rc)=opt$(1)
00760   fnFra(12,1,2,45,"Base graph on usage or dollars","You can either analyze dollars or usage.",0) !:
        fnOpt(1,2,"Use Usage",0,1) !:
        resp$(rc+=1)="True" !:
        fnOpt(2,2,"Use Dollars",0,1)
00770   fnCmdSet(2) !:
        fnAcs(sn$,0,mat resp$,ckey)
00780   if ckey=5 then goto XIT
00790   for j=1 to 24
00800 L800: x=pos(resp$(j),"/",1) !:
          if x>0 then resp$(j)(x:x)="": goto L800
00810     cd1(j)=val(resp$(j)) conv MSGBOX
00820     y=val(resp$(j)(5:6))
00830     if y=1 then month$(j)="Jan"
00840     if y=2 then month$(j)="Feb"
00850     if y=3 then month$(j)="Mar"
00860     if y=4 then month$(j)="Apr"
00870     if y=5 then month$(j)="May"
00880     if y=6 then month$(j)="Jun"
00890     if y=7 then month$(j)="Jul"
00900     if y=8 then month$(j)="Aug"
00910     if y=9 then month$(j)="Sep"
00920     if y=10 then month$(j)="Oct"
00930     if y=11 then month$(j)="Nov"
00940     if y=12 then month$(j)="Dec"
00950   next j
00960   if cd1(1)=0 then goto MSGBOX
00970   if resp$(25)="Water" then codepos=143: service=1: opt=1
00980   if resp$(25)=trim$(opt$(2)) then codepos=147: service=3: opt=2
00990   if resp$(25)=trim$(opt$(3)) then codepos=149 : service=4 : opt=3
01000   if resp$(26)="True" then baseon=1 else baseon =2 ! 1=usage  2=dollars
01010   for j=1 to 24
01020     actualdate$(j)=resp$(j)
01030   next j
01035   goto SETDOLLARS
01040 L1040: read #1,using L1050: z$,e$,servicecode eof STORE_GRAPH_INFO
01050 L1050: form pos 1,c 10,x 30,c 30,pos 296,pd 4
01060   restore #2,key>=z$&"         ": nokey L1040
01070 L1070: read #2,using L1100: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof STORE_GRAPH_INFO
01080   if p$<>z$ then goto L1040 ! history record must belong to this customer
01090   if tcode<>1 then goto L1070 ! charge transactions only
01100 L1100: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
01110 ! ______________________________________________________________________
01120   if service=1 and baseon=1 then usage=wu ! analyzing water
01130   if service=1 and baseon=2 then usage=tg(1) ! analyzing water dollars
01140   if service=3 and baseon=1 then usage=eu ! analyzing electric
01150   if service=3 and baseon=2 then usage=tg(3) ! analyzing electric dollars
01160   if service=4 and baseon=1 then usage=gu ! analyzing gas
01170   if service=4 and baseon=2 then usage=tg(4) ! analyzing gas dollars
01180   for j=1 to 24
01190     if cd1(j)><tdate then goto L1210
01200     n2(j)=n2(j)+1 !:
          u1(j)=u1(j)+usage !:
          u2(j)=u2(j)+usage
01210 L1210: next j
01220   goto L1070 ! read next transaction
01230 STORE_GRAPH_INFO: ! 
01240 SETDOLLARS: ! for J=1 To 24  set calculated vision kj
01250   dollars(1)=100 ! first day tested
01260   dollars(2)=93 ! first time checked 1-10
01270   dollars(3)=88 ! 1/26
01280   dollars(4)=85 ! 2/1/11
01290   dollars(5)=82 ! 2/2/11
01291   dollars(6)=77 ! 2/3/11
01292   dollars(7)=71 ! 2/9/11
01293   dollars(8)=57 ! 2/15/11
01300 ! dOLLARS(J)=U1(J)
01310 ! Next J
01320 PRINT_CHART: ! 
01330   gosub VBOPENPRINT
01340 ! determine maximum height and depth
01350   for j=1 to 24
01360     if dollars(j)>0 then maximumheight=max(dollars(j),maximumheight) ! largest dollars by month for either year  (dollars is negative figure
01370   next j
01380 ! determine top line and bottom line
01390   if baseon=1 then top$=str$(maximumheight): toplen=len(top$): top=toplen*10
01400   if baseon=2 then top$=str$(round(maximumheight,0)): toplen=len(top$): top=toplen*10
01410   toplen$=str$(val(top$(1:1))+1)
01420   for j=1 to toplen-1
01430     toplen$=toplen$&str$(0)
01440   next j
01450   top=val(toplen$) : top=100 ! change top here kj
01460   x=top*.10
01470 DETERMINE_BOTTOM_LINE: ! 
01480   spacing=10 : lyne=30
01485   cnam$="Ken Johnson"
01490   cnam=(len(trim$(cnam$))/2)+120
01500   pr #20: 'Call Print.MyFontsize(14)'
01510   txt$=cnam$ !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(cnam)&','&str$(10)&')'
01520   pr #20: 'Call Print.MyFontsize(12)'
01530   txt$="Eye Chart"
01540   servicetype=(len(trim$(txt$))/2)+120
01550   pr #20: 'Call Print.AddText("'&txt$&'",'&str$(servicetype)&','&str$(16)&')'
01560   if baseon=1 then txt$=""
01570   if baseon=2 then txt$=trim$(opt$(opt))
01580   servicetype=(len(trim$(txt$))/2)+140
01590   pr #20: 'Call Print.AddText("'&txt$&'",'&str$(servicetype)&','&str$(20)&')'
01600   pr #20: 'Call Print.MyFontsize(9)'
01610   txt$=cnvrt$("pic(--------)",top) !:
        pr #20: 'Call Print.AddText('&txt$&','&str$(1)&','&str$(lyne+=spacing)&')'
01620   pr #20: 'Call Print.AddLine('&str$(15)&','&str$(lyne)&',240,0)' ! left,up/down,lenght of top line on chart
01630   for j=1 to 10 ! wording down side
01640     txt$=cnvrt$("pic(-------#)",top-((.10*j)*top)) !:
          pr #20: 'Call Print.AddText('&txt$&','&str$(1)&','&str$(lyne+=spacing)&')'
01650     pr #20: 'Call Print.AddLine('&str$(15)&','&str$(lyne)&',240,0)'
01660   next j
01670 ! zero line starts right here
01680   linezero=(spacing*10)+40
01690   column=18 ! spacing sideways
01700   pr #20: 'Call Print.MyFontBold(1)'
01710   for j=1 to 24
01720     homedot=140-((dollars(j)/top)*100)
01730     if homedot<0 then homedot=0
01740     if homedot>140 then homedot=140
01750     pr #20: 'Call Print.AddLine('&str$(column-1)&','&str$(homedot)&','&str$(7)&','&str$(linezero-homedot)&',1)'
01760     for q=1 to 6
01770       pr #20: 'Call Print.AddLine('&str$(column-1+q)&','&str$(homedot)&','&str$(7-q)&','&str$(linezero-homedot)&',1)'
01780     next q
01790     pr #20: 'Call Print.MyFontsize(6)'
01800     if dollars(j)>0 then txt$=cnvrt$("pic(--------#)",round(dollars(j),0)): pr #20: 'Call Print.AddText('&txt$&','&str$(column-2)&','&str$(homedot-2)&')'
01810     pr #20: 'Call Print.MyFontsize(9)'
01820     column+=10
01830   next j
01840   pr #20: 'Call Print.MyFontBold(0)'
01850   for j=1 to 24 ! month wording at bottom of page
01860     txt$=trim$(month$(j))(1:3) !:
          indent=8+(10*j) !:
          pr #20: 'Call Print.AddText("'&txt$&'",'&str$(indent)&','&str$(linezero+5)&')'
01870     txt$=actualdate$(j)(7:8)
01880     pr #20: 'Call Print.AddText("'&txt$&'",'&str$(indent+1)&','&str$(linezero+8)&')'
01890     txt$=actualdate$(j)(3:4)
01900     pr #20: 'Call Print.AddText("'&txt$&'",'&str$(indent+1)&','&str$(linezero+11)&')'
01910   next j
01920   gosub RELEASE_PRINT
01930   close #1: 
01940   goto XIT
01950 ! ______________________________________________________________________
01960 XIT: fnxit
01970 ! ______________________________________________________________________
01980 ! <Updateable Region: ERTN>
01990 ERTN: fnerror(program$,err,line,act$,"xit")
02000   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
02010   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02020   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02030 ERTN_EXEC_ACT: execute act$ : goto ERTN
02040 ! /region
02050 ! ______________________________________________________________________
03000 VBOPENPRINT: ! r:
03020   if file(20)=-1 then 
03040     open #20: "Name="&env$('Q')&"\UBmstr\linechart"&wsid$&".txt,Replace,RecL=5000",display,output 
03060     pr #20: 'Call Print.MyOrientation("Landscape")'
03080     lyne=margin ! starting of 1st line
03100     column1=16
03120     column2=103
03140     column3=153
03160   end if 
03180   return  ! /r
03200 RELEASE_PRINT: ! r:
03220   fnpa_finis
03240   return  ! /r
03260 MSGBOX: ! 
03280   msgline$(1)="You have entered dates in an"
03300   msgline$(2)="invalid format.  Use mmddyy format."
03320   fnmsgbox(mat msgline$,resp$,cap$,1)
03340   goto SCREEN1
